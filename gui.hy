
(import threading
        logging
        sys

        [PyQt5.QtWidgets [QDialog QApplication QMessageBox]]
        [PyQt5 [QtWidgets QtGui QtCore]]

        [mhdlg [Ui_MHDlg]])

(defclass MainDlg [QDialog Ui_MHDlg]
  (defn __init__ [self &optional [parent None]]
    (-> (super)
        (.__init__ parent))
    (self.setupUi self)

    (setv self.chs-model (QtGui.QStandardItemModel))
    (->> ["选择" "章节" "下载进度"]
         (self.chs-model.setHorizontalHeaderLabels))
    (self.chs_table.setModel self.chs-model)
    )

  (defn get-chapters [self]
    (print "TODO get chapters")
    )

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
