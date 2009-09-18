(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(defparameter ctx (zmq:init 1 1))
(defparameter s (zmq:socket ctx zmq:req))

(with-foreign-string (addr *address*)
  (zmq:connect s addr))

(defvar *elapsed* nil)
(defvar *latency* nil)

(with-foreign-object (msg 'zmq:msg)
  (zmq:msg-init-size msg *message-size*)
  (with-foreign-object (watch :long 2)
    (setf watch (zmq:stopwatch-start))
    (dotimes (i *roundtrip-count*)
      (zmq:send s msg 0)
      (zmq:recv s msg 0))
    (setf *elapsed* (zmq:stopwatch-stop watch)))
  (zmq:msg-close msg))

(setf *latency* (/ *elapsed* (* 2 *roundtrip-count*)))

(format t "message size: ~d [B]~%" *message-size*)
(format t "roundtrip count: ~d~%" *roundtrip-count*)
(format t "average latency: ~f [us]~%" *latency*)

(zmq:close s)
(zmq:term ctx)
(sb-ext:quit)
;
