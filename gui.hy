#!/usr/bin/env hy

(require [hy.extra.anaphoric [*]]
         [helpers [*]]
         [hy.contrib.walk [let]]
         )

(import threading
        logging
        sys
        os

        [PyQt5.QtWidgets [QDialog
                          QApplication
                          QMessageBox
                          QProgressBar
                          QAbstractItemView
                          QHeaderView
                          QStyleOptionButton
                          QStyle
                          ]]
        [PyQt5.QtCore [QRect Qt QThreadPool]]
        [PyQt5 [QtWidgets QtGui QtCore]]

        [manhua-get [get-manhua-info
                     get-manhua-images
                     save-image
                     ]]
        [functools [partial]]
        [retry [retry]]
        [helpers [*]]
        [worker [Worker]]
        [mhdlg [Ui_MHDlg]])

(defclass CheckBoxDelegate [QtWidgets.QItemDelegate]
  "提供QCheckBox的delegate"
  (defn __init__ [self parent]
    (QtWidgets.QItemDelegate.__init__ self parent))

  (defn createEditor [self parent option index]
    "不创建editor"
    None)

  (defn paint [self painter option index]
    "绘制checkbox"
    (->> (if (= (int (index.data))
                0)
             QtCore.Qt.Unchecked
             QtCore.Qt.Checked)
         (self.drawCheck painter option option.rect)))

  (defn editorEvent [self event model option index]
    (when (not (pos? (int (& (index.flags)
                             QtCore.Qt.ItemIsEditable))))
      (return False))

    (when (and (= (event.type)
                  QtCore.QEvent.MouseButtonRelease)
               (= (event.button)
                  QtCore.Qt.LeftButton))
      (self.setModelData None model index)
      (return True))
    False)

  (defn setModelData [self editor model index]
    (->2> (if (zero? (int (index.data)))
              1
              0)
          (model.setData index QtCore.Qt.EditRole))))

(setv PROGRESS-ROLE (+ QtCore.Qt.UserRole 999))

(defclass ProgressDelegate [QtWidgets.QStyledItemDelegate]
  "提供QProgress的delegate"
  (defn paint [self painter option index]
    (setv progress (index.data PROGRESS-ROLE))
    (setv opt (QtWidgets.QStyleOptionProgressBar))
    (set-attrs opt
               rect option.rect
               minimum 0
               maximum 100
               progress progress
               text f"{progress :.1f}%"
               textVisible True)
    (-> (QtWidgets.QApplication.style)
        (.drawControl QtWidgets.QStyle.CE_ProgressBar opt painter))))

(defclass MainDlg [QDialog Ui_MHDlg]
  (defn __init__ [self &optional [parent None]]
    (-> (super)
        (.__init__ parent))
    (self.setupUi self)

    (setv self.chs-model (QtGui.QStandardItemModel))
    (->> ["" "章节" "下载进度"]
         (self.chs-model.setHorizontalHeaderLabels))
    (self.chs-table.setModel self.chs-model)

    (doto (self.chs-table.horizontalHeader)
          ;; ;; 禁止修改列宽
          ;; (.setDisabled True)
          ;;(.setSectionResizeMode QHeaderView.Stretch)
          )
    (self.chs-table.setColumnWidth 0 30)
    (self.chs-table.setColumnWidth 1 200)

    (-> (self.chs-table.viewport)
        (.installEventFilter self))
    (self.chs-table.installEventFilter self)

    (->> (CheckBoxDelegate self.chs-table)
         (self.chs-table.setItemDelegateForColumn 0))
    (->> (ProgressDelegate self.chs-table)
         (self.chs-table.setItemDelegateForColumn 2))

    (setv self.threadpool (QThreadPool))

    (self.chs-table.setColumnHidden 3 True)

    (global form)
    (setv form self)
    )

  (defn closeEvent [self event]
    (logging.info "close event")
    (event.accept)
    (os._exit 0)
    )

  (defn reject [self]
    (self.close)
    None)

  (defn get-chapters [self]
    (setv mid (self.mid-editor.text))
    (if mid
        (self.get-manhua-info mid)
        (QMessageBox.information self
                                 "提示" "漫画id不能为空"
                                 QMessageBox.Ok QMessageBox.Ok)))

  (defn add-chapter-row [self title url]
    (self.chs-model.appendRow [(QtGui.QStandardItem "0")
                               (QtGui.QStandardItem title)
                               (doto (QtGui.QStandardItem)
                                     (.setData 0 PROGRESS-ROLE))
                               (QtGui.QStandardItem url)
                               ])
    )

  (defn change-selection [self]
    (for [idx (-> (self.chs-table.selectionModel)
                  (.selectedRows)
                  sorted)]
      (setv item (self.chs-model.item (idx.row) 0))
      (logging.warning f"item text:{(item.text)} data:{(item.data)}" )
      (-> (if (= (item.text) "0")
              "1"
              "0")
          (item.setData Qt.DisplayRole))))

  (defn eventFilter [self source event]
    (cond [(or (and (= QtCore.QEvent.KeyPress (event.type))
                    (= QtCore.Qt.Key_Space (event.key)))
               (and (= QtCore.QEvent.MouseButtonPress (event.type))
                    (= QtCore.Qt.RightButton (event.button))
                    (is source (self.chs-table.viewport))))

           (do (print "change selection.")
               (self.change-selection)
               True)]

          [(and (= QtCore.QEvent.KeyPress (event.type))
                (= QtCore.Qt.Key-Escape (event.key)))

           (do (print "clear selection.")
               (self.chs-table.clearSelection)
               True)]

          [True
           (-> (super MainDlg self)
               (.eventFilter source event))]))

  (defn get-manhua-info [self mid]
    (setv self.infos (-> (.strip mid)
                         (get-manhua-info)))
    (when self.infos
      (self.mahua-title.setText (of self.infos "title"))
      (for [cht (of self.infos "chapters")]
        (self.add-chapter-row (of cht "title")
                              (of cht "url")))

      (self.chs-table.setColumnHidden 3 True)
      ))

  (defn read-row-items [self row]
    (->> (map #%(self.chs-model.item row %1) [0 1 2 3])
         (zip ["checked" "title" "progress" "url"])
         (dict)))

  (with-decorator (retry Exception :delay 5 :backoff 4 :max-delay 120)
    (defn download-chapter [self info progress-callback]
      (when (not info)
        (return None))

      (logging.info "download chapter worker")
      (setv url (.text (of info "url")))
      (setv title (.text (of info "title")))

      (logging.info f" start download chapter {title}")

      (setv img-urls (get-manhua-images url))
      (setv target-path (os.path.join "manga"
                                      (self.mahua-title.text)
                                      title))
      (when (not (os.path.exists target-path))
        (os.makedirs target-path))

      (logging.info "save-image [%d] to: %s." (len img-urls) target-path)

      (for [i (range (len img-urls))]
        (save-image (of img-urls i)
                    (os.path.join target-path f"{i :03}.jpg"))
        (progress-callback.emit (/ (* i 100)
                                   (len img-urls))))
      (logging.info "save-images to %s over!" target-path)
      (len img-urls)))

  (defn download-result [self info o]
    (setv title (.text (of info "title")))
    (logging.info f"{title} download result: {o}"))

  (defn download-complete [self info]
    (setv title (.text (of info "title")))
    (logging.info f"{title} download complete")
    (-> (of info "progress")
        (.setData 100 PROGRESS-ROLE)))

  (defn download-progress [self info n]
    (setv title (.text (of info "title")))
    (logging.info f"{title} progress {n}")
    (-> (of info "progress")
        (.setData n PROGRESS-ROLE)))

  (defn get-checked-chapters [self]
    (->> (self.chs-model.rowCount)
         range
         (map #%(doto (self.read-row-items %1)
                      (-> (of "row")
                          (setv %1))))
         (filter #%(= (.text (of %1 "checked"))
                      "1"))
         list))

  (defn download-jpgs [self]
    (self.threadpool.setMaxThreadCount 5)
    (setv infos (self.get-checked-chapters))
    (print  f"download-jpgs {(len infos)}")
    (logging.info f"download-jpgs {(len infos)}")
    (for [info infos]
      (-> (doto (Worker self.download-chapter info)
                (.signals.result.connect (partial self.download-result info))
                (.signals.finished.connect (partial self.download-complete info))
                (.signals.progress.connect (partial self.download-progress info)))
          (self.threadpool.start)))
    (logging.info "download-jpgs run over.")))

(defmain [&rest args]
  (logging.basicConfig :level logging.INFO
                       ;; :filename "app.log"
                       ;; :filemode "w"
                       :style "{"
                       :format "{asctime} [{levelname}] {filename}({funcName})[{lineno}] {message}")
  (setv app (QtWidgets.QApplication sys.argv))
  (doto (MainDlg)
        (.show)
        (.raise_))
  (sys.exit (app.exec_))
  )
