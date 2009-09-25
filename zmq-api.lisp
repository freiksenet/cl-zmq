(in-package :cl-zmq)

(defun bind (s address)
  (with-foreign-string (addr address)
    (%bind s addr)))
;
