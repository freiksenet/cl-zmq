(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "thr-parms")

(defvar *elapsed* nil)
(defvar *throughput* nil)
(defvar *megabits* nil)

(zmq::with-context (ctx 1 1)
  (zmq:with-socket (s ctx zmq:sub)
    (zmq:setsockopt s zmq:subscribe "*")
    (zmq:setsockopt s zmq:rate *rate*)
    (zmq:bind s *address*)
    (let ((msg (zmq:make-message)))
      (zmq:recv s msg)
      (setf *elapsed*
	    (zmq:with-stopwatch
	      (dotimes (i (1- *message-count*))
		(zmq:recv s msg))))))
  (setq *throughput* (* (/ *message-count* *elapsed*) 1e6)
	*megabits* (/ (* *throughput* *message-count* 8) 1e6))

  (format t "message size: ~d [B]~%" *message-size*)
  (format t "message count: ~d~%" *message-count*)
  (format t "mean throughput: ~d [msg/s]~%" (round *throughput*))
  (format t "mean throughput: ~,3f [Mb/s]~%" *megabits*))

(tg:gc)
(sb-ext:quit)

;
