#!/usr/bin/env hy

(require [hy.extra.anaphoric [*]]
         [helpers [*]]
         )

(import threading
        logging
        sys

        [PyQt5.QtWidgets [QDialog
                          QApplication
                          QMessageBox
                          QProgressBar
                          QAbstractItemView
                          QHeaderView
                          QStyleOptionButton
                          QStyle
                          ]]
        [PyQt5.QtCore [QRect Qt]]
        [PyQt5 [QtWidgets QtGui QtCore]]

        [manhua-get [get-manhua-info
                     ]]
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
               text f"{progress}%"
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
    )

  (defn get-chapters [self]
    (setv mid (self.mid-editor.text))
    (if mid
        (self.get-manhua-info mid)
        (QMessageBox.information self
                                 "提示" "漫画id不能为空"
                                 QMessageBox.Ok QMessageBox.Ok)))

  (defn add-chapter-row [self title]
    (self.chs-model.appendRow [(QtGui.QStandardItem "0")
                               (QtGui.QStandardItem title)
                               (doto (QtGui.QStandardItem)
                                     (.setData 0 PROGRESS-ROLE))
                               ])
    )

  (defn change-selection [self]
    (for [idx (-> (self.chs-table.selectionModel)
                  (.selectedRows)
                  sorted)]
      (setv old-check (-> (idx.row)
                          (self.chs-model.index 0)
                          (self.chs-model.data)))
      (logging.info "old check of %s is %s" idx old-check)
      (->> (if (= old-check "0")
               "1"
               "0")
           (QtGui.QStandardItem)
           (self.chs-model.setItem (idx.row) 0))))

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
        (self.add-chapter-row (of cht "title")))))

  (defn download-jpgs [self]
    (print "TODO download jpgs")
    )
  )

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
