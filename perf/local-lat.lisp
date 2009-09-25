(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(defparameter ctx (zmq:init 1 1))
(defparameter s (zmq:socket ctx zmq:rep))

(zmq:with-socket (s ctx zmq:rep)
  (zmq:bind s *address*)
  (let ((msg (zmq:make-message)))
    (dotimes (i *roundtrip-count*)
      (zmq:recv s msg 0)
      (zmq:send s msg 0)))
  (zmq:sleep 1))

(zmq:term ctx)
(sb-ext:quit)
;
