(in-package :cl-zmq)

(defun bind (s address)
  (with-foreign-string (addr address)
    (%bind s addr)))

(defun connect (s address)
  (with-foreign-string (addr address)
    (%connect s addr)))

(defun make-message (&optional (size nil size-p))
  (let ((msg (foreign-alloc 'zmq:msg)))
    (tg:finalize msg #'(lambda (msg) (free-message msg)))
    (msg-init msg)
    (when size-p
      (msg-init-size msg size))
    msg))

(defun free-message (msg)
  (msg-close msg)
  (foreign-free msg))

(defmacro with-context ((context app-threads io-threads &optional flags) &body body)
  `(let ((,context (init ,app-threads ,io-threads (or ,flags 0))))
     ,@body
     (term ,context)))

(defmacro with-socket ((socket context type) &body body)
  `(let ((,socket (socket ,context ,type)))
     ,@body
     (close ,socket)))

(defmacro with-stopwatch (&body body)
  (let ((watch (gensym)))
    `(with-foreign-object (,watch :long 2)
       (setq ,watch (stopwatch-start))
       ,@body
       (stopwatch-stop ,watch))))

;
