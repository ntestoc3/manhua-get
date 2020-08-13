#!/usr/bin/env hy

(require [hy.extra.anaphoric [*]]
         [helpers [*]]
         )

(import requests
        json
        re
        base64
        os
        logging
        sys
        argparse
        time
        shutil

        [tqdm [tqdm]]
        [bs4 [BeautifulSoup]]
        [datetime [datetime]]
        [retry [retry]]
        [fake-useragent [UserAgent]]
        [helpers [*]]
        )

(setv ua (UserAgent :use-cache-server True ))
(setv proxy {"http" "http://localhost:8080"
             "https" "http://localhost:8080"})

(setv http-args {"verify" True
                 "proxies" None #_proxy
                 "timeout" 30})

(defn random-ua
  []
  "随机获取一个user-agent"
  ua.random)

(setv base-url "https://www.x18mh.com")

(defn valid-jpg?
  [img-path]
  (and
    (> (os.path.getsize img-path) 20)
    (with [f (open img-path "rb")]
      (f.seek -2 2)
      (-> (f.read)
          (.endswith b"\xff\xd9")))))

(defn parse-chapters
  [data]
  (some-> data
          (.select-one "div.plist")
          (.select "li a")
          (->> (map #%(dict {"url" (of %1 "href")
                             "title" (of %1 "title")})))
          list))

(defn parse-title
  [data]
  (some-> data
          (.select-one "div.title h1")
          (. text)))

(defn parse-info
  [body]
  "解析漫画信息"
  (setv data (BeautifulSoup body "lxml"))
  {"title" (parse-title data)
   "chapters" (parse-chapters data)})

(setv pbar (tqdm :total 0))
(defn update-progress-total
  [n]
  (+= pbar.total n)
  (pbar.refresh))

(defn update-progress
  [&optional [n 1]]
  (pbar.update n))

(with-decorator (retry Exception :delay 5 :backoff 4 :max-delay 120)
  (defn get-manhua-info
    [mid]
    "获取所有漫画信息,标题和章节"
    (logging.info "get chapters for:%s" mid)
    (setv headers {"user-agent" (random-ua)})
    (some-> (requests.get
              f"{base-url}/manhua/{mid}/"
              :headers headers #**http-args)
            (doto (setattr "encoding" "gbk2312"))
            (. text)
            parse-info)))

(defn valid-download?
  [out-path real-size]
  (-> (os.stat out-path)
      (. st-size)
      (= (int real-size))))

(with-decorator (retry Exception :delay 5 :backoff 4 :max-delay 120)
  (defn save-image
    [img-url out-path]
    (setv headers {"user-agent" (random-ua)
                   "Accept-Encoding" "gzip, deflate, br"
                   "Accept" "image/webp,image/apng,image/*,*/*"})
    (when (not (and (os.path.exists out-path)
                    (or (valid-jpg? out-path)
                        (valid-download? out-path
                                         (-> (requests.head img-url)
                                             (. headers)
                                             (of "Content-Length"))))))
      (setv r (requests.get
                img-url
                :stream True
                :headers headers #**http-args))
      (when (= r.status_code 200)
        (with [outf (open out-path "wb")]
          (setv r.raw.decode-content True)
          (shutil.copyfileobj r.raw outf)))
      (when (not (valid-download? out-path
                                  (of r.headers "Content-Length")))
        (raise (Exception f"save no valid image url:{img-url} path:{out-path}"))))))

(defn parse-image-urls
  [body]
  (some-> (re.findall r"qTcms_S_m_murl_e=\"(.*)\";" body)
          first
          base64.b64decode
          (.decode "utf-8")
          (.split "$qingtiandy$")))

(with-decorator (retry Exception :delay 5 :backoff 4 :max-delay 120)
  (defn get-manhua-images
    [chapter-url]
    (logging.info "get image for:%s" chapter-url)
    (setv headers {"user-agent" (random-ua)})
    (some-> (requests.get
              f"{base-url}{chapter-url}"
              :headers headers #**http-args)
            (. text)
            parse-image-urls)))

(defn save-images
  [img-urls save-path]
  (logging.info "save-images [%d] to: %s." (len img-urls) save-path)
  (when (not (os.path.exists save-path))
    (os.makedirs save-path))
  (update-progress-total (len img-urls))
  (->2> (enumerate img-urls)
        (pmap #%(do (save-image (second %1)
                                (os.path.join save-path f"{(first %1) :03}.jpg"))
                    (update-progress))
              :proc 5))
  (logging.info "save-images to %s over!" save-path))

(defn save-manhua
  [mid chapters &optional [proc 5]]
  (setv info (get-manhua-info mid))
  (->2> (of info "chapters")
        (filter (fn [ch]
                  (-> (of ch "title")
                      (in chapters))))
        (pmap #%(-> (get-manhua-images (of %1 "url"))
                    (save-images (os.path.join "manga"
                                               (of info "title")
                                               (of %1 "title"))))
              :proc proc)))

(defn save-chapters
  [mid outf]
  (with [w (open outf "w")]
    (-> (get-manhua-info mid)
        (of "chapters")
        (->> (map #%(of %1 "title"))
             list
             (sorted)
             (.join "\n"))
        (w.write))))

(defmain [&rest args]
  (logging.basicConfig :level logging.INFO
                       ;; :filename "app.log"
                       ;; :filemode "w"
                       :style "{"
                       :format "{asctime} [{levelname}] {filename}({funcName})[{lineno}] {message}")

  (setv opts (parse-args [
                          ["-c" "--chapters-file"  :type (argparse.FileType "r")
                           :help "包含章节名的文件"]
                          ["-p" "--parall" :type int :default 5
                           :help "并发下载数量"]
                          ["mid" :type int  :help "漫画id"]]
                         (rest args)
                         :description "漫画下载工具
    如果没有指定包含章节名的文件，则默认寻找`mid`.txt文件
    如果找不到章节文件，则下载章节信息到`mid`.txt文件，然后退出"
                         :formatter-class argparse.RawTextHelpFormatter
                         ))
  (logging.info "opts:%s" opts )
  (if (and (none? opts.chapters-file)
           (not (os.path.exists f"{opts.mid}.txt")))
      (save-chapters opts.mid f"{opts.mid}.txt")
      (do
        (logging.info f"save manhua for {opts.mid}")
        (setv f (if opts.chapters-file
                    opts.chapters-file
                    (open f"{opts.mid}.txt" "r")))
        (->2> (f.read)
              (.splitlines)
              (map #%(.strip %1))
              (filter (comp not empty?))
              list
              (save-manhua opts.mid :proc opts.parall))))

  (logging.info "over!")
  )
