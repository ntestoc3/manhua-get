
(import traceback
        sys
        logging

        [PyQt5.QtGui [*]]
        [PyQt5.QtCore [*]])

(defclass WorkerSignals [QObject]
  " Defines the signals available from a running worker thread.

    Supported signals are:

    finished
        No data

    error
        `list` [exctype, value, traceback.format_exc() ]

    result
        `object` data returned from processing, anything

    progress
        `int` indicating % progress "

  (setv finished (pyqtSignal))
  (setv error (pyqtSignal list))
  (setv result (pyqtSignal object))
  (setv progress (pyqtSignal int))
  )

(defclass Worker [QRunnable]
  " Worker thread
    Inherits from QRunnable to handler worker thread setup, signals and wrap-up.

    :param callback: The function callback to run on this worker thread. Supplied args and 
                     kwargs will be passed through to the runner.
    :type callback: function
    :param args: Arguments to pass to the callback function
    :param kwargs: Keywords to pass to the callback function
  "
  (defn __init__ [self f &rest args &kwargs kwargs]
    (-> (super Worker self)
        (.__init__))

    (setv self.f f)
    (setv self.args args)
    (setv self.kwargs kwargs)
    (setv self.signals (WorkerSignals))

    (setv (of self.kwargs "progress_callback")
          self.signals.progress))

  (with-decorator (pyqtSlot)
    (defn run [self]
      (try
        (logging.info f"worker run f:{self.f} args:{self.args}")
        (setv result (self.f #* self.args #** self.kwargs))
        (except [e Exception]
          (logging.exception f"worker run. args:{self.args} kwargs:{self.kwargs}")
          (setv [exctype value] (cut (sys.exc-info) 0 2))
          (self.signals.error.emit [exctype value (traceback.format-exc)]))
        (else
          (self.signals.result.emit result))
        (finally
          (self.signals.finished.emit))))))
