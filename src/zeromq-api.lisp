;; This file is part of CL-ZMQ.

(in-package :zeromq)

;; Basics

(defun version ()
  (with-foreign-objects ((major :int)
                         (minor :int)
                         (patch :int))
    (%version major minor patch)
    (values
     (mem-ref major :int)
     (mem-ref minor :int)
     (mem-ref patch :int))))

;; Contexts

(defmacro with-context ((context) &body body)
  `(let ((,context (ctx-new)))
     (unwind-protect
          (progn ,@body)
       (ctx-destroy ,context))))

(defun ctx-new ()
  (call-with-error-check
   #'%ctx-new '()
   :type :pointer))

(defun ctx-get (context option)
  (call-with-error-check
   #'%ctx-get
   (list context
         (foreign-enum-value
               'context-options
               option))))

(defun ctx-set (context option value)
  (call-with-error-check
   #'%ctx-set
   (list context
         (foreign-enum-value
          'context-options
          option)
         value)))

(defun ctx-destroy (context)
  (call-with-error-check
   #'%ctx-destroy
   (list context)))

;; Sockets

(defun socket (context type)
  (call-with-error-check
   #'%socket
   (list context
         (foreign-enum-value 'socket-types
                             type))
   :type :pointer))

(defun close (socket)
  (call-with-error-check
   #'%close
   (list socket)))

(defmacro with-socket ((socket context type) &body body)
  `(let ((,socket (socket ,context ,type)))
     (unwind-protect
          (progn ,@body)
       (close ,socket))))

(defun bind (s address)
  (with-foreign-string (addr address)
    (call-with-error-check #'%bind (list s addr))))

(defun unbind (s address)
  (with-foreign-string (addr address)
    (call-with-error-check #'%unbind (list s addr))))

(defun connect (s address)
  (with-foreign-string (addr address)
    (call-with-error-check #'%connect (list s addr))))

(defun disconnect (s address)
  (with-foreign-string (addr address)
    (call-with-error-check #'%disconnect (list s addr))))

(defun get-socket-option-value-type (option)
  (case option
    ((:affinity) :uint64)
    ((:subscribe
      :unsubscribe
      :identity
      :tcp-accept-filter
      :last-endpoint) :string)
    ((:maxmsgsize) :int64)
    (otherwise :int)))

(defun getsockopt (socket option)
  (let ((return-type (get-socket-option-value-type option))
        (option-code (foreign-enum-value 'socket-options option)))
    (if (eq return-type :string)
        (with-foreign-objects ((str :char 255)
                               (len 'size-t))
          (setf (mem-aref len 'size-t) (* (foreign-type-size :char) 255))
          (%getsockopt socket option-code str len)
          (foreign-string-to-lisp str :count (mem-aref len 'size-t)))
        (with-foreign-objects ((obj return-type)
                               (len 'size-t))
          (setf (mem-aref len 'size-t) (foreign-type-size return-type))
          (%getsockopt socket option-code obj len)
          (mem-aref obj return-type)))))

(defun setsockopt (socket option value)
  (let ((return-type (get-socket-option-value-type option))
        (option-code (foreign-enum-value 'socket-options option)))
    (if (eq return-type :string)
        (with-zmq-string ((string len) value)
          (%setsockopt socket option-code string len))
        (with-foreign-object (obj return-type)
          (setf (mem-aref obj return-type) value)
          (%setsockopt socket option-code obj
                       (foreign-type-size return-type))))))

;; Messages

(defstruct (msg (:constructor %make-msg))
  raw)

(defun make-msg (&key size data)
  (let ((msg (%make-msg))
        (raw-msg (foreign-alloc 'c-msg)))
    (tg:finalize msg (lambda ()
                       (%msg-close raw-msg)
                       (foreign-free raw-msg)))
    (cond (size (%msg-init-size raw-msg size))
          (data
           (etypecase data
             (string
              (with-foreign-string (fstr data)
                (copy-lisp-string-octets
                 data
                 (lambda (size)
                   (%msg-init-size raw-msg size)
                   (%msg-data raw-msg)))))
             ((simple-array (unsigned-byte 8))
              (let ((len (length data)))
                (%msg-init-size raw-msg len)
                (with-pointer-to-vector-data (ptr data)
                  (%memcpy (%msg-data raw-msg) ptr len))))
             (array (progn
                      (%msg-init-size raw-msg (length data))
                      (let ((ptr (%msg-data raw-msg))
                            (i -1))
                        (map nil (lambda (x)
                                   (setf (mem-aref ptr :uchar (incf i)) x))
                             data))))))
          (t (%msg-init raw-msg)))
    (setf (msg-raw msg) raw-msg)
    msg))

(defun msg-data-as-is (msg)
  (%msg-data (msg-raw msg)))

(defun msg-data-as-string (msg)
  (let ((data (%msg-data (msg-raw msg)))
        (size (%msg-size (msg-raw msg))))
    (unless (zerop (pointer-address data))
      (foreign-string-to-lisp data :count size))))

(defun msg-data-as-array (msg)
  (let ((data (%msg-data (msg-raw msg))))
    (unless (zerop (pointer-address data))
      (let* ((len (msg-size msg))
             (arr (#+lispworks sys:in-static-area
                               #-lispworks cl:identity
                               (make-array len :element-type '(unsigned-byte 8)))))
        (declare (type (simple-array (unsigned-byte 8)) arr))
        (with-pointer-to-vector-data (ptr arr)
          (%memcpy ptr data len))
        arr))))

(defun msg-size (msg)
  (%msg-size (msg-raw msg)))

(defun msg-move (dst src)
  (%msg-move (msg-raw dst) (msg-raw src)))

(defun msg-copy (dst src)
  (%msg-copy (msg-raw dst) (msg-raw src)))

;; Sending and recieving

(defun send (s data &rest flags)
  (with-foreign-string ((buf len) data)
    (%send s buf (1- len)
           (foreign-bitfield-value 'send-recv-options flags))))

(defun recv (s length &rest flags)
  (with-foreign-string ((buf len) (make-string length))
    (%recv s buf (1- len)
           (foreign-bitfield-value 'send-recv-options flags))
    (foreign-string-to-lisp buf)))

(defun msg-send (s msg &rest flags)
  (%msg-send (msg-raw msg) s
             (foreign-bitfield-value 'send-recv-options flags)))

(defun msg-recv (s msg &rest flags)
  (%msg-recv (msg-raw msg) s
             (foreign-bitfield-value 'send-recv-options flags)))

;; Polls
;; (xxx)freiksenet: probably broken, don't use it yet.

(defclass pollitem ()
  ((raw         :accessor pollitem-raw :initform nil)
   (socket      :accessor pollitem-socket :initform (cffi:null-pointer) :initarg :socket)
   (fd          :accessor pollitem-fd :initform -1 :initarg :fd)
   (events      :accessor pollitem-events :initform 0 :initarg :events)
   (revents     :accessor pollitem-revents :initform 0)))

(defmethod initialize-instance :after ((inst pollitem) &key)
  (let ((obj (foreign-alloc 'pollitem)))
    (setf (pollitem-raw inst) obj)
    (tg:finalize inst (lambda () (foreign-free obj)))))

(defun poll (items &optional (timeout -1))
  (let ((len (length items)))
    (with-foreign-object (%items 'c-pollitem len)
      (dotimes (i len)
        (let ((item (nth i items))
              (%item (mem-aref %items 'c-pollitem i)))
          (with-foreign-slots ((socket fd events revents) %item pollitem)
            (setf socket (pollitem-socket item)
                  fd (pollitem-fd item)
                  events (pollitem-events item)))))
      (let ((ret (%poll %items len timeout)))
        (cond
          ((zerop ret) nil)
          ((plusp ret)
           (loop for i below len
              for revent = (foreign-slot-value (mem-aref %items 'c-pollitem i)
                                               'c-pollitem
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
