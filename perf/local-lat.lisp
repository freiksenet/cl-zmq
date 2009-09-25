(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(defparameter ctx (zmq:init 1 1))
(defparameter s (zmq:socket ctx zmq:rep))

(zmq:bind s *address*)

(with-foreign-object (msg 'zmq:msg)
  (zmq:msg-init msg)
  (dotimes (i *roundtrip-count*)
    (zmq:recv s msg 0)
    (zmq:send s msg 0))
  (zmq:msg-close msg))

(zmq:sleep 1)
(zmq:close s)
(zmq:term ctx)
(sb-ext:quit)
;
