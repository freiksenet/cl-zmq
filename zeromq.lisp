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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ basics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("memcpy" %memcpy) :pointer
  (dst :pointer)
  (src :pointer)
  (len :long))

(defcfun ("zmq_version" %version) :void
  (major :pointer)
  (minor :pointer)
  (patch :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_errno" %errno) :int)

(defcfun ("zmq_strerror" %strerror) :pointer
  (errnum :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defctype c-context :pointer)

(defctype c-socket :pointer)

(defcstruct c-msg
  (_ :uchar :count 32))

(defcstruct c-pollitem
  (socket c-socket)
  (fd :int)
  (events :short)
  (revents :short))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ contexts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_ctx_new" ctx-new) c-context)

(defcfun ("zmq_ctx_get" %ctx-get) :int
  (context c-context)
  (option-name :int))

(defcfun ("zmq_ctx_set" %ctx-set) :int
  (context c-context)
  (option-name :int)
  (option-value :int))

(defcfun ("zmq_ctx_destroy" ctx-destroy) :int
  (context c-context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ sockets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant pair 0)
(defconstant pub 1)
(defconstant sub 2)
(defconstant req 3)
(defconstant rep 4)
(defconstant dealer 5)
(defconstant xreq dealer)
(defconstant router 6)
(defconstant xrep router)
(defconstant pull 7)
(defconstant push 8)

(defconstant hwm 1)
(defconstant swap 3)
(defconstant affinity 4)
(defconstant identity 5)
(defconstant subscribe 6)
(defconstant unsubscribe 7)
(defconstant rate 8)
(defconstant recovery-ivl 9)
(defconstant mcast-loop 10)
(defconstant sndbuf 11)
(defconstant rcvbuf 12)
(defconstant rcvmore 13)

(defconstant noblock 1)
(defconstant sndmore 2)

(defcfun ("zmq_socket" socket) c-socket
  (context c-context)
  (type :int))

(defcfun ("zmq_close" close) :int
  (socket c-socket))

(defcfun ("zmq_getsockopt" %getsockopt) :int
  (socket c-socket)
  (option-name :int)
  (option-value :pointer)
  (option-len :pointer))

(defcfun ("zmq_setsockopt" %setsockopt) :int
  (socket c-socket)
  (option-name :int)
  (option-value :pointer)
  (optvallen :long))

(defcfun ("zmq_bind" %bind) :int
  (socket c-socket)
  (endpoint :pointer :char))

(defcfun ("zmq_unbind" %unbind) :int
  (socket c-socket)
  (endpoint :pointer :char))

(defcfun ("zmq_connect" %connect) :int
  (socket c-socket)
  (endpoint :pointer :char))

(defcfun ("zmq_disconnect" %disconnect) :int
  (socket c-socket)
  (endpoint :pointer :char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ message definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_msg_init" %msg-init) :int
  (msg :pointer))

(defcfun ("zmq_msg_init_size" %msg-init-size) :int
  (msg c-msg)
  (size :long))

;; This currently won't work properly for the reasons defined here
;; http://13-49.blogspot.fi/2010/06/why-zero-copy-is-missing-in-cl-zmq.html
;; Don't use it unless you know what you are doing.
(defcallback zmq-free :void ((ptr :pointer) (hint :pointer))
  (declare (ignorable hint))
  (foreign-free ptr))

(defcfun ("zmq_msg_init_data" %%msg-init-data) :int
  (msg c-msg)
  (data :pointer)
  (size :long)
  (ffn :pointer)
  (hint :pointer))

(defun %zmq-init-data (msg data size)
  (%%msg-init-data msg data size 'zmq-free))
;; End of non-functional code

(defcfun ("zmq_msg_close" %msg-close) :int
  (msg c-msg))

(defcfun ("zmq_msg_move" %msg-move) :int
  (dest c-msg)
  (src c-msg))

(defcfun ("zmq_msg_copy" %msg-copy) :int
  (dest c-msg)
  (src c-msg))

(defcfun ("zmq_msg_data" %msg-data) :pointer
  (msg c-msg))

(defcfun ("zmq_msg_size" %msg-size) :int
  (msg c-msg))

(defcfun ("zmq_msg_get" %msg-get) :int
  (msg c-msg)
  (property :int))

(defcfun ("zmq_msg_set" %msg-set) :int
  (msg c-msg)
  (property :int)
  (value :int))

(defcfun ("zmq_msg_more" %msg-more) :int
  (msg c-msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ send/recieve
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_send" %send) :int
  (socket c-socket)
  (data :pointer)
  (size :long)
  (flags :int))

(defcfun ("zmq_recv" %recv) :int
  (socket c-socket)
  (data :pointer)
  (size :long)
  (flags :int))

(defcfun ("zmq_msg_send" %msg-send) :int
  (msg c-msg)
  (socket c-socket)
  (flags :int))

(defcfun ("zmq_msg_recv" %msg-recv) :int
  (msg c-msg)
  (socket c-socket)
  (flags :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ polling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant pollin 1)
(defconstant pollout 2)
(defconstant pollerr 4)

(defcfun ("zmq_poll" %poll) :int
  (items :pointer)
  (nitems :int)
  (timeout :long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ proxy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant streamer 1)
(defconstant forwarder 2)
(defconstant queue 3)

(defcfun ("zmq_proxy" proxy) :int
  (frontend :pointer)
  (backend :pointer)
  (capture :pointer))
