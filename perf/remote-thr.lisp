(asdf:oos 'asdf:load-op :cl-zmq)

(defpackage :zmq-test
  (:use :cl :cffi))

(in-package :zmq-test)

(load "thr-parms")

(zmq::with-context (ctx 1 1)
  (zmq:with-socket (s ctx zmq:pub)
    (zmq:setsockopt s zmq:rate *rate*)
    (zmq:connect s *connect-address*)
    (let ((msg (make-instance 'zmq:msg :size *message-size*)))
      (dotimes (i *message-count*)
	(zmq:msg-init-size msg *message-size*)
	(zmq:send s msg)
	(zmq:msg-close msg))
      (zmq:sleep 10))))

(tg:gc)
#+sbcl (sb-ext:quit)
#+clisp (ext:quit)
;
