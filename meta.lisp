(in-package :cl-zmq)

(define-condition error-again (error)
  ((argument :reader error-again :initarg :argument))
  (:report (lambda (condition stream)
	     (write-string (convert-from-foreign
			    (%strerror (error-again condition))
			    :string)
			   stream))))

(defmacro defcfun* (name-and-options return-type &body args)
  (let* ((c-name (car name-and-options))
	 (l-name (cadr name-and-options))
	 (n-name (cffi::format-symbol t "%~A" l-name))
	 (name (list c-name n-name))

	 (docstring (when (stringp (car args)) (pop args)))
	 (ret (gensym)))
    (loop with opt
       for i in args
       unless (consp i) do (setq opt t)
       else
       collect i into args*
       and if (not opt) collect (car i) into names
       else collect (car i) into opts
       and collect (list (car i) 0) into opts-init
       end
       finally (return
	 `(progn
	    (defcfun ,name ,return-type
	      ,@args*)

	    (defun ,l-name (,@names &optional ,@opts-init)
	      ,docstring
	      (let ((,ret (,n-name ,@names ,@opts)))
		(if ,(if (eq return-type :pointer)
			   `(zerop (pointer-address ,ret))
			   `(not (zerop ,ret)))
		    (cond
		      ((eq *errno* isys:eagain) (error 'error-again :argument *errno*))
		      (t (error (convert-from-foreign (%strerror *errno*) :string))))
		,ret))))))))
