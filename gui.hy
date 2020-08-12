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

(defclass CheckBoxHeader [QHeaderView]
  (setv is-on False)

  (defn __init__ [self orientation &optional [parent None]]
    (QHeaderView.__init__ self orientation parent))

  (defn paintSection [self painter rect index]
    (painter.save)
    (QHeaderView.paintSection self painter rect index)
    (painter.restore)

    (when (zero? index)
      (setv option (QStyleOptionButton))
      (set-attrs option
                 rect (QRect 10 10 10 10)
                 state (if self.is-on
                           QStyle.State-On
                           QStyle.State-Off))
      (-> (self.style)
          (.drawControl QStyle.CE-CheckBox
                        option
                        painter))))

  (defn mousePressEvent [self event]
    (setv self.is-on (not self.is-on))
    (self.updateSection 0)
    (QHeaderView.mousePressEvent self event)))

(defclass MainDlg [QDialog Ui_MHDlg]
  (defn __init__ [self &optional [parent None]]
    (-> (super)
        (.__init__ parent))
    (self.setupUi self)

    (-> (CheckBoxHeader Qt.Horizontal self)
        (self.chs-table.setHorizontalHeader))

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

  (defn eventFilter [self source event]
    (if (and (= QtCore.QEvent.KeyPress (event.type))
             (= QtCore.Qt.Key_Space (event.key)))
        (do (print "space")
            True)
        (-> (super MainDlg self)
            (.eventFilter source event))))

  (defn get-manhua-info [self mid]
    (setv self.infos (get-manhua-info mid))
    (self.mahua-title.setText (of self.infos "title"))
    (for [cht (of self.infos "chapters")]
      (self.add-chapter-row (of cht "title"))))

  (defn download-jpgs [self]
    (print "TODO download jpgs")
    )
  )

(defmain [&rest args]
  (setv app (QtWidgets.QApplication sys.argv))
  (doto (MainDlg)
        (.show)
        (.raise_))
  (sys.exit (app.exec_))
  )
