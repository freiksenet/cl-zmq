(in-package :cl-zmq)

(defun bind (s address)
  (with-foreign-string (addr address)
    (%bind s addr)))

(defun make-message (&optional (size nil size-p))
  (let ((msg (foreign-alloc 'zmq:msg)))
    (msg-init msg)
    (when size-p
      (msg-init-size msg size))
    (tg:finalize msg #'(lambda (msg) (free-message msg)))
    msg))

(defun free-message (msg)
  (msg-close msg)
  (foreign-free msg))

(defmacro with-context ((context app-threads io-threads) &body body)
  `(let ((,context (init ,app-threads ,io-threads)))
     ,@body
     (term ,context)))

(defmacro with-socket ((socket context type) &body body)
  `(let ((,socket (socket ,context ,type)))
     ,@body
     (close ,socket)))
;
