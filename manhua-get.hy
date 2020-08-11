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
                 "proxies" None
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
          parse-info))

(with-decorator (retry Exception :delay 5 :backoff 2 :max-delay 120)
  (defn save-image
    [img-url out-path]
    (when (not (and (os.path.exists out-path)
                    (valid-jpg? out-path)))
      (setv headers {"user-agent" (random-ua)
                     "Accept-Encoding" "gzip, deflate, br"
                     "Accept" "image/webp,image/apng,image/*,*/*"})
      (setv r (requests.get
                img-url
                :stream True
                :headers headers #**http-args))
      (when (= r.status_code 200)
        (with [outf (open out-path "wb")]
          (setv r.raw.decode-content True)
          (shutil.copyfileobj r.raw outf)))
      (when (not (valid-jpg? out-path))
        (raise (Exception "save no valid image"))))))

(defn parse-image-urls
  [body]
  (some-> (re.findall r"qTcms_S_m_murl_e=\"(.*)\";" body)
          first
          base64.b64decode
          (.decode "utf-8")
          (.split "$qingtiandy$")))

(with-decorator (retry Exception :delay 5 :backoff 2 :max-delay 120)
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
  (->2> (enumerate img-urls)
        (pmap #%(save-image (second %1)
                            (os.path.join save-path f"{(first %1) :03}.jpg"))
              :proc 5))
  (logging.info "save-images to %s over!" save-path))

(defn save-manhua
  [mid ch-count &optional [start 0] [proc 5]]
  (setv info (get-manhua-info mid))
  (->2> (-> (of info "chapters")
            (cut start ch-count))
        (pmap #%(-> (get-manhua-images (of %1 "url"))
                    (save-images (os.path.join "manga"
                                               (of info "title")
                                               (of %1 "title"))))
              :proc proc)))

(defmain [&rest args]
  (logging.basicConfig :level logging.INFO
                       ;; :filename "app.log"
                       ;; :filemode "w"
                       :style "{"
                       :format "{asctime} [{levelname}] {filename}({funcName})[{lineno}] {message}")

  (setv opts (parse-args [["-s" "--start" :type int :default 0
                           :help "start chapter index to download, reverse order"]
                          ["-c" "--count" :type int
                           :help "total chapters to download, None will download all"]
                          ["-p" "--parall" :type int :default 5
                           :help "parall run downloader"]
                          ["mid" :nargs "+" :type int  :help "manhua id"]]
                         (rest args)
                         :description "find subdomains for root domain"))
  (logging.info "opts:%s" opts )
  (for [mid opts.mid]
    (save-manhua mid opts.count :start opts.start :proc opts.parall))

  (logging.info "over!")
  )
