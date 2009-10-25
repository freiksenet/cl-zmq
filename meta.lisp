(in-package :cl-zmq)

(defclass zmq (standard-class) ())

(defmethod validate-superclass ((obj zmq) (obj1 standard-class)) t)

(defvar *zmq-slot-readers* (make-hash-table :test 'equal))
(defvar *zmq-slot-writers* (make-hash-table :test 'equal))

(defmethod slot-value-using-class ((class zmq) inst slot)
  (if (string= (string-upcase (slot-definition-name slot)) "RAW")
      (call-next-method class inst)
      (funcall (gethash (cons (class-name class) (slot-definition-name slot))
			*zmq-slot-readers*)
	       inst)))

(defmethod (setf slot-value-using-class) (new (class zmq) inst slot)
  (if (string= (string-upcase (slot-definition-name slot)) "RAW")
      (call-next-method new class inst)
      (funcall (gethash (cons (class-name class) (slot-definition-name slot))
			*zmq-slot-writers*)
	       new inst)))

(defmacro define-wrapper (class-and-type supers &optional slots)
  (destructuring-bind (class-name &optional (struct-type class-name))
      (cffi::ensure-list class-and-type)
    (let ((slots (or slots (cffi::foreign-slot-names struct-type)))
	  (raw-accessor (cffi::format-symbol t "~A-RAW" class-name)))
      `(progn
	 (defclass ,class-name ,supers
	   (,@(loop for slot in slots collect
		   `(,slot :initarg ,(intern (string-upcase slot) "KEYWORD")))
	    (raw :accessor ,raw-accessor))
	   (:metaclass zmq))

	 ,@(loop for slot in slots
	      for slot-name = (cffi::format-symbol t "~A-~A" class-name slot)
	      for slot-type = (cffi::slot-type (cffi::get-slot-info class-name slot))
	      collect
		`(defun ,slot-name (inst)
		   ,(if (or (eq slot-type :char) (eq slot-type :uchar))
			`(convert-from-foreign
			  (foreign-slot-value (,raw-accessor inst) ',class-name ',slot) :string)
			(if (cffi::aggregatep (cffi::parse-type slot-type))
			    `(make-instance ',slot-type
					    :pointer (foreign-slot-value (,raw-accessor inst) ',class-name ',slot))
			    `(foreign-slot-value (,raw-accessor inst) ',class-name ',slot))))
	      collect
		`(setf (gethash (cons ',class-name ',slot) *zmq-slot-readers*)
		       (fdefinition ',slot-name))

	      collect
		`(defun (setf ,slot-name) (new inst)
		   (setf (foreign-slot-value (,raw-accessor inst) ',class-name ',slot)
			 (convert-to-foreign new ',slot-type)))
	      collect
		`(setf (gethash (cons ',class-name ',slot) *zmq-slot-writers*)
		       (fdefinition '(setf ,slot-name))))

         (defmethod initialize-instance :before ((inst ,class-name) &key pointer finalizer)
	   (let ((obj (or pointer (foreign-alloc ',class-name))))
	     (setf (,raw-accessor inst) obj)
	     (when finalizer
	       (tg:finalize inst (lambda () (funcall finalizer obj))))
	     (unless pointer
	       (tg:finalize inst (lambda () (foreign-free obj))))))
         ',class-name))))

(defmacro def-c-struct (name &rest args)
  "Define cffi struct and generate wrapper"
  `(progn
     (defcstruct ,name
       ,@args)
     (define-wrapper ,name ())))

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
