(require :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "lat-parms")

(zmq:with-context (ctx 1 1 zmq:poll)
  (zmq:with-socket (s ctx zmq:rep)
    (zmq:bind s *address*)
    (let ((msg (zmq:make-message)))
      (zmq:with-poll (poll-in ((s . zmq:pollin)))
	(zmq:with-poll (poll-out ((s . zmq:pollout)))
	  (dotimes (i *roundtrip-count*)
	    (zmq:poll poll-in)
	    (zmq:recv s msg zmq:noblock)
	    (zmq:poll poll-out)
	    (zmq:send s msg zmq:noblock)))))))

(tg:gc)
(sb-ext:quit)

;
