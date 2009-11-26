(in-package :cl-zmq)

(defclass msg ()
  ((raw		:accessor msg-raw :initform nil)
   (shared	:accessor msg-shared :initform 0 :initarg :shared)))

(defmethod initialize-instance :after ((inst msg) &key size data)
  (let ((obj (foreign-alloc 'msg)))
    (with-slots (raw shared) inst
      (setf raw obj)
      (tg:finalize inst (lambda ()
			  (msg-close raw)
			  (foreign-free raw)))
      (when shared
	(setf (foreign-slot-value obj 'msg 'shared) (if shared 1 0)))
      (cond (size (msg-init-size raw size))
	    (data
	     (multiple-value-bind (ptr len)
		 (etypecase data
		   (string (foreign-string-alloc data))
		   (array (values (foreign-alloc :uchar :initial-contents data)
				  (length data))))
	       (msg-init-data raw ptr len (callback zmq-free))))
	    (t (msg-init raw))))))

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
  "Bind the socket to a particular address."
  (with-foreign-string (addr address)
    (%bind s addr)))

(defun connect (s address)
  "Connect the socket to a particular address."
  (with-foreign-string (addr address)
    (%connect s addr)))

(defmacro with-context ((context app-threads io-threads &optional flags) &body body)
  "Run body in 0MQ context."
  `(let ((,context (init ,app-threads ,io-threads (or ,flags 0))))
     ,@body
     (term ,context)))

(defmacro with-socket ((socket context type) &body body)
  "Run body in socket context."
  `(let ((,socket (socket ,context ,type)))
     ,@body
     (close ,socket)))

(defmacro with-stopwatch (&body body)
  "Measure runtime of body."
  (let ((watch (gensym)))
    `(with-foreign-object (,watch :long 2)
       (setq ,watch (stopwatch-start))
       ,@body
       (stopwatch-stop ,watch))))

(defun msg-data-as-is (msg)
  (%msg-data (msg-raw msg)))

(defun msg-data-as-string (msg)
  "Return message data in the form of string."
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop (pointer-address data))
      (convert-from-foreign data :string))))

(defun msg-data-as-array (msg)
  "Return message data in the form of bytes array."
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop (pointer-address data))
      (let* ((len (msg-size msg))
	     (arr (make-array len :element-type '(unsigned-byte))))
	(dotimes (i len)
	  (setf (aref arr i) (mem-aref data :uchar i)))
	arr))))

(defun send (s msg &optional flags)
  "Send the message 'msg' to the socket 's'. 'flags' argument can be
combination the flags described above.

Function raises zmq:error-again for the non-blocking operation if
syscall returns EAGAIN."
  (%send s (msg-raw msg) (or flags 0)))

(defun recv (s msg &optional flags)
  "Receive a message from the socket 's'. 'flags' argument can be combination
of the flags described above with the exception of ZMQ_NOFLUSH.

Function raises zmq:error-again for the non-blocking operation if
syscall returns EAGAIN."
  (%recv s (msg-raw msg) (or flags 0)))

(defun msg-size (msg)
  "Return size of message data (in bytes)."
  (%msg-size (msg-raw msg)))

(defun msg-move (dst src)
  "Move the content of the message from 'src' to 'dest'. The content isn't
copied, just moved. 'src' is an empty message after the call. Original
content of 'dest' message is deallocated."
  (%msg-move (msg-raw dst) (msg-raw src)))

(defun msg-copy (dst src)
  "Copy the 'src' message to 'dest'. The content isn't copied, instead
reference count is increased. Don't modify the message data after the
call as they are shared between two messages. Original content of 'dest'
message is deallocated."
  (%msg-copy (msg-raw dst) (msg-raw src)))

(defun setsockopt (socket option value)
  "Sets an option on the socket. 'option' argument specifies the option (see
the option list above). 'optval' is a pointer to the value to set,
'optvallen' is the size of the value in bytes."
  (etypecase value
    (string (with-foreign-string (string value)
	      (%setsockopt socket option string (length value))))
    (integer (with-foreign-object (int :long 2)
	       (setf (mem-aref int :long 0) value)
	       (%setsockopt socket option int (foreign-type-size :long))))))

(defun poll (items)
  "Polls for the items specified by 'items'. Number of items in the array is
determined by 'nitems' argument. Returns number of items signaled, -1
in the case of error."
  (let ((len (length items)))
    (with-foreign-object (%items 'pollitem len)
      (dotimes (i len)
	(let ((item (nth i items))
	      (%item (mem-aref %items 'pollitem i)))
	  (with-foreign-slots ((socket fd events revents) %item pollitem)
	    (setf socket (pollitem-socket item)
		  fd (pollitem-fd item)
		  events (pollitem-events item)))))
      (let ((ret (%poll %items len)))
	(if (> ret 0)
	    (loop for i below len
	       for revent = (foreign-slot-value (mem-aref %items 'pollitem i)
						'pollitem
						'revents)
	       collect (setf (pollitem-revents (nth i items)) revent))
	    (error (convert-from-foreign (%strerror *errno*) :string)))))))

(defmacro with-polls (list &body body)
  "Automatically creates lists of pollitems."
  `(let ,(loop for (name . polls) in list
	    collect `(,name
		      (list
		       ,@(loop for (socket . events) in polls
			    collect `(make-instance 'pollitem
						    :socket ,socket
						    :events ,events)))))
     ,@body))

;
