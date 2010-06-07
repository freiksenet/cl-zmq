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
	       (isys:usleep (round (* 1e6 0.01)))
	       (go retry))))
;; blocking recv
        (zmq:recv s msg)
	(zmq:send s msg)))
    (isys:usleep (round 1e6))))

(tg:gc)
#+sbcl (sb-ext:quit)
#+clisp (ext:quit)
#+ccl (ccl:quit)

;
