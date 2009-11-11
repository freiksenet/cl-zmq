(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(zmq:with-context (ctx 1 1 zmq:poll)
  (zmq:with-socket (s ctx zmq:rep)
    (zmq:bind s *address*)
    (let ((msg (make-instance 'zmq:msg)))
      (zmq:with-polls ((poll-in . ((s . zmq:pollin)))
		       (poll-out . ((s . zmq:pollout))))
	(dotimes (i *roundtrip-count*)
	  (zmq:poll poll-in)
	  (zmq:recv s msg zmq:noblock)
	  (zmq:poll poll-out)
	  (zmq:send s msg zmq:noblock))))))

(tg:gc)
(sb-ext:quit)

;
