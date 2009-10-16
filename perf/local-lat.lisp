(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(zmq::with-context (ctx 1 1)
  (zmq:with-socket (s ctx zmq:rep)
    (zmq:bind s *address*)
    (let ((msg (zmq:make-message)))
      (dotimes (i *roundtrip-count*)
;; non-blocking recv
	#+nil
	(tagbody retry
	   (handler-case
	       (progn
		 (zmq:recv s msg zmq:noblock)
		 (format t "size ~d, ~a~%" (zmq:msg-size msg) (zmq:msg-data-as-array msg)))
	     (zmq:error-again (c)
	       (declare (ignore c))
	       (sleep 0.01)
	       (go retry))))
;; blocking recv
        (zmq:recv s msg)
	(zmq:send s msg)))
    (zmq:sleep 1)))
(tg:gc)
(sb-ext:quit)

;
