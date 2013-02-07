;; This file is part of CL-ZMQ.

(in-package :zeromq)

(define-condition zmq-error (error)
  ((code :reader error-code :initarg :code)
   (description :reader error-description :initarg :description))
  (:report (lambda (condition stream)
             (format stream "ZMQ Error ~A - ~A."
                     (error-code condition)
                     (error-description condition)))))

(defun call-with-error-check (function args &key (type :int) error-p)
  (let ((error-p (or error-p
                     (if (eq type :int)
                         #'minusp
                         #'null-pointer-p)))
        (ret (apply function args)))
    (if (funcall error-p ret)
        (let* ((error-code (%errno))
               (error-description
                 (convert-from-foreign (%strerror error-code) :uint)))
          (make-condition 'zmq-error
                          :code error-code
                          :description error-description))
        ret)))

;; Stolen from CFFI. Uses custom allocator (alloc-fn) instead of foreign-alloc
(defun copy-lisp-string-octets (string alloc-fn
                                &key
                                  (encoding cffi::*default-foreign-encoding*)
                                  (start 0) end)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (cffi::with-checked-simple-vector ((string
                                      (coerce string 'babel:unicode-string))
                                     (start start) (end end))
    (declare (type simple-string string))
    (let* ((mapping (cffi::lookup-mapping
                     cffi::*foreign-string-mappings*
                     encoding))
           (count (funcall (cffi::octet-counter mapping) string start end 0))
           (ptr (funcall alloc-fn count)))
      (funcall (cffi::encoder mapping) string start end ptr 0)
      (values ptr count))))

(defmacro with-zmq-string (args &body body)
  `(with-foreign-string (,@args :null-terminated-p '())
     ,@body))
