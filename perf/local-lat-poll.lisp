;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(asdf:oos 'asdf:load-op :zeromq)

(defpackage :zeromq-test
  (:use :cl))

(in-package :zeromq-test)

(load "lat-parms")

(zmq:with-context (ctx 1)
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
#+sbcl (sb-ext:quit)
#+clisp (ext:quit)

;
