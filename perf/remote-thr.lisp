(asdf:oos 'asdf:load-op :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "thr-parms")

(zmq::with-context (ctx 1 1)
  (zmq:with-socket (s ctx zmq:pub)
    (zmq:setsockopt s zmq:rate *rate*)
    (zmq:connect s *connect-address*)
    (let ((msg (make-instance 'zmq:msg :data #(1 2 3))))
      (dotimes (i *message-count*)
	(zmq:send s msg)))))

(tg:gc)
#+sbcl (sb-ext:quit)
#+clisp (ext:quit)
;
