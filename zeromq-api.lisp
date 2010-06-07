;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :zeromq)

(defcfun ("memcpy" memcpy) :pointer
  (dst	:pointer)
  (src	:pointer)
  (len	:long))

;; Stolen from CFFI. Uses custom allocator (alloc-fn) instead of foreign-alloc
(defun copy-lisp-string-octets (string alloc-fn &key (encoding cffi::*default-foreign-encoding*)
                             (null-terminated-p t) (start 0) end)
  "Allocate a foreign string containing Lisp string STRING.
The string must be freed with FOREIGN-STRING-FREE."
  (check-type string string)
  (cffi::with-checked-simple-vector ((string (coerce string 'babel:unicode-string))
				     (start start) (end end))
    (declare (type simple-string string))
    (let* ((mapping (cffi::lookup-mapping cffi::*foreign-string-mappings* encoding))
           (count (funcall (cffi::octet-counter mapping) string start end 0))
           (length (if null-terminated-p
                       (+ count (cffi::null-terminator-len encoding))
                       count))
	   (ptr (funcall alloc-fn length)))
      (funcall (cffi::encoder mapping) string start end ptr 0)
      (when null-terminated-p
        (dotimes (i (cffi::null-terminator-len encoding))
          (setf (mem-ref ptr :char (+ count i)) 0)))
      (values ptr length))))

(defclass msg ()
  ((raw		:accessor msg-raw :initform nil)))

(defmethod initialize-instance :after ((inst msg) &key size data)
  (let ((obj (foreign-alloc 'msg)))
    (tg:finalize inst (lambda ()
			(%msg-close obj)
			(foreign-free obj)))
    (cond (size (%msg-init-size obj size))
	  (data
	   (etypecase data
	     (string (copy-lisp-string-octets
		      data (lambda (sz)
			     (%msg-init-size obj sz)
			     (%msg-data obj))))
	     ((simple-array (unsigned-byte 8))
	      (let ((len (length data)))
		(%msg-init-size obj len)
		(with-pointer-to-vector-data (ptr data)
		  (memcpy (%msg-data obj) ptr len))))
	     (array (progn
		      (%msg-init-size obj (length data))
		      (let ((ptr (%msg-data obj))
			    (i -1))
			(map nil (lambda (x)
				   (setf (mem-aref ptr :uchar (incf i)) x))
			     data))))))
	  (t (msg-init obj)))
    (setf (msg-raw inst) obj)))

(defclass pollitem ()
  ((raw		:accessor pollitem-raw :initform nil)
   (socket	:accessor pollitem-socket :initform nil :initarg :socket)
   (fd		:accessor pollitem-fd :initform -1 :initarg :fd)
   (events	:accessor pollitem-events :initform 0 :initarg :events)
   (revents	:accessor pollitem-revents :initform 0)))

(defmethod initialize-instance :after ((inst pollitem) &key)
  (let ((obj (foreign-alloc 'pollitem)))
    (setf (pollitem-raw inst) obj)
    (tg:finalize inst (lambda () (foreign-free obj)))))

(defun bind (s address)
  (with-foreign-string (addr address)
    (%bind s addr)))

(defun connect (s address)
  (with-foreign-string (addr address)
    (%connect s addr)))

(defmacro with-context ((context app-threads io-threads &optional flags) &body body)
  `(let ((,context (init ,app-threads ,io-threads (or ,flags 0))))
     (unwind-protect
	  (progn ,@body)
       (term ,context))))

(defmacro with-socket ((socket context type) &body body)
  `(let ((,socket (socket ,context ,type)))
     (unwind-protect
	  (progn ,@body)
       (close ,socket))))

(defun msg-data-as-is (msg)
  (%msg-data (msg-raw msg)))

(defun msg-data-as-string (msg)
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop (pointer-address data))
      (convert-from-foreign data :string))))

(defun msg-data-as-array (msg)
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop (pointer-address data))
      (let* ((len (msg-size msg))
	     (arr (make-array len :element-type '(unsigned-byte 8))))
	(declare (type (simple-array (unsigned-byte 8)) arr))
	(with-pointer-to-vector-data (ptr arr)
	  (memcpy ptr data len))
	arr))))

(defun send (s msg &optional flags)
  (%send s (msg-raw msg) (or flags 0)))

(defun recv (s msg &optional flags)
  (%recv s (msg-raw msg) (or flags 0)))

(defun msg-init-size (msg size)
  (%msg-init-size (msg-raw msg) size))

(defun msg-close (msg)
  (%msg-close (msg-raw msg)))

(defun msg-size (msg)
  (%msg-size (msg-raw msg)))

(defun msg-move (dst src)
  (%msg-move (msg-raw dst) (msg-raw src)))

(defun msg-copy (dst src)
  (%msg-copy (msg-raw dst) (msg-raw src)))

(defun setsockopt (socket option value)
  (etypecase value
    (string (with-foreign-string (string value)
	      (%setsockopt socket option string (length value))))
    (integer (with-foreign-object (int :int64)
	       (setf (mem-aref int :int64) value)
	       (%setsockopt socket option int (foreign-type-size :int64))))))

(defun getsockopt (socket option)
  (with-foreign-objects ((opt :int64)
			 (len :long))
    (setf (mem-aref opt :int64) 0
	  (mem-aref len :long) (foreign-type-size :int64))
    (%getsockopt socket option opt len)
    (mem-aref opt :int64)))

(defun poll (items &optional (timeout -1))
  (let ((len (length items)))
    (with-foreign-object (%items 'pollitem len)
      (dotimes (i len)
	(let ((item (nth i items))
	      (%item (mem-aref %items 'pollitem i)))
	  (with-foreign-slots ((socket fd events revents) %item pollitem)
	    (setf socket (pollitem-socket item)
		  fd (pollitem-fd item)
		  events (pollitem-events item)))))
      (let ((ret (%poll %items len timeout)))
	(cond
	  ((zerop ret) nil)
	  ((plusp ret)
	    (loop for i below len
	       for revent = (foreign-slot-value (mem-aref %items 'pollitem i)
						'pollitem
						'revents)
	       collect (setf (pollitem-revents (nth i items)) revent)))
	  (t (error (convert-from-foreign (%strerror (errno)) :string))))))))

(defmacro with-polls (list &body body)
  `(let ,(loop for (name . polls) in list
	    collect `(,name
		      (list
		       ,@(loop for (socket . events) in polls
			    collect `(make-instance 'pollitem
						    :socket ,socket
						    :events ,events)))))
     ,@body))

(defun version ()
  (with-foreign-objects ((major :int)
			 (minor :int)
			 (patch :int))
    (%version major minor patch)
    (format nil "~d.~d.~d"
	    (mem-ref major :int)
	    (mem-ref minor :int)
	    (mem-ref patch :int))))

;
