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

;; Sockets

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
    (call-with-error-check #'unbind (list s addr))))

(defun connect (s address)
  (with-foreign-string (addr address)
    (call-with-error-check #'%connect (list s addr))))

(defun disconnect (s address)
  (with-foreign-string (addr address)
    (call-with-error-check #'%connect (list s addr))))

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

;; Messages

(defclass msg ()
  ((raw :accessor msg-raw :initform nil)))

(defmethod initialize-instance :after ((inst msg) &key size data)
  (let ((obj (foreign-alloc 'c-msg)))
    (tg:finalize inst (lambda ()
                        (%msg-close obj)
                        (foreign-free obj)))
    (cond (size (%msg-init-size obj size))
          (data
           (etypecase data
             (string
              (with-foreign-string (fstr data)
                (copy-lisp-string-octets
                 data (lambda (sz)
                        (%msg-init-size obj sz)
                        (%msg-data obj)))))
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
          (t (%msg-init obj)))
    (setf (msg-raw inst) obj)))

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
          (memcpy ptr data len))
        arr))))

(defun msg-close (msg)
  (%msg-close (msg-raw msg)))

(defun msg-init-size (msg size)
  (%msg-init-size (msg-raw msg) size))

(defun msg-size (msg)
  (%msg-size (msg-raw msg)))

(defun msg-move (dst src)
  (%msg-move (msg-raw dst) (msg-raw src)))

(defun msg-copy (dst src)
  (%msg-copy (msg-raw dst) (msg-raw src)))

;; Sending and recieving

(defun send (s data &optional (flags 0))
  (with-foreign-string ((buf len) data)
    (%send s buf (1- len) flags)))

(defun recv (s data length &optional flags)
  (with-foreign-string ((buf len) (make-string length))
    (%recv s buf (1- len) flags)))

(defun msg-send (s msg &optional flags)
  (%msg-send (msg-raw msg) s (or flags 0)))

(defun msg-recv (s msg &optional flags)
  (%msg-recv (msg-raw msg) s (or flags 0)))

;; Polls

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
