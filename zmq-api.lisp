(in-package :cl-zmq)

(defun bind (s address)
  "Bind the socket to a particular address."
  (with-foreign-string (addr address)
    (%bind s addr)))

(defun connect (s address)
  "Connect the socket to a particular address."
  (with-foreign-string (addr address)
    (%connect s addr)))

;; Do I really need all this horrible meta stuff just for 2 cstructs?..
(defmethod initialize-instance :after ((inst msg) &key size data)
  (with-slots (raw) inst
    (tg:finalize inst (lambda () (msg-close raw)))
    (when size
      (msg-init-size raw size))
    (when data
      (multiple-value-bind (ptr len)
	  (etypecase data
	    (string (let ((ptr (convert-to-foreign data :string)))
		      (values ptr (1+ (foreign-funcall "strlen" :pointer ptr :long)))))
	    (array (let* ((len (length data))
			  (ptr (foreign-alloc :uchar :count len)))
		     (dotimes (i len)
		       (setf (mem-aref ptr :uchar i) (aref data i)))
		     (values ptr len))))
	(msg-init-data raw ptr len (callback zmq-free))))))

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
						    :events ,events )))))
     ,@body))

;
