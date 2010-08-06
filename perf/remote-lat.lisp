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
(load "helpers")

(defvar *elapsed* nil)
(defvar *latency* nil)

(zmq::with-context (ctx 1)
  (zmq:with-socket (s ctx zmq:req)
    (zmq:connect s *address*)
    (let ((msg (make-instance 'zmq:msg :size *message-size*)))
      (setf *elapsed*
            (with-stopwatch
                (dotimes (i *roundtrip-count*)
                  (zmq:send s msg)
                  (zmq:recv s msg)))))
    (sleep 1)))

(setf *latency* (/ *elapsed* (* 2 *roundtrip-count*)))

(format t "message size: ~d [B]~%" *message-size*)
(format t "roundtrip count: ~d~%" *roundtrip-count*)
(format t "average latency: ~f [us]~%" *latency*)

(tg:gc)
#+sbcl (sb-ext:quit)
#+clisp (ext:quit)
#+ccl (ccl:quit)
