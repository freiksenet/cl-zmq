;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of 0MQ.
;;
;; 0MQ is free software; you can redistribute it and/or modify it under
;; the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; 0MQ is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.
;;
;; You should have received a copy of the Lesser GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :zeromq)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ errors.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant hausnumero 156384712)

;;  Native 0MQ error codes.
(defconstant emthread (+ hausnumero 50))
(defconstant efsm (+ hausnumero 51))
(defconstant enocompatproto (+ hausnumero 52))

(defcfun ("zmq_strerror" %strerror) :pointer
  (errnum	:int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ message definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant max-vsm-size 30)

;;  Message types. These integers may be stored in 'content' member of the
;;  message instead of regular pointer to the data.
(defconstant delimiter 31)
(defconstant vsm 32)

;; Message flags. ZMQ_MSG_SHARED is strictly speaking not a message flag
;; (it has no equivalent in the wire format), however, making  it a flag
;; allows us to pack the stucture tigher and thus improve performance.
(defconstant msg-more 1)
(defconstant msg-shared 128)

(defcstruct (msg)
  (content	:pointer)
  (shared	:uchar)
  (vsm-size	:uchar)
  (vsm-data	:uchar :count 30))	;; FIXME max-vsm-size

(defcfun ("zmq_msg_init" msg-init) :int
  (msg	msg))

(defcfun* ("zmq_msg_init_size" %msg-init-size) :int
  (msg	msg)
  (size	:long))

(defcallback zmq-free :void ((ptr :pointer) (hint :pointer))
  (declare (ignorable hint))
  (foreign-free ptr))

(defcfun ("zmq_msg_init_data" msg-init-data) :int
  (msg	msg)
  (data	:pointer)
  (size	:long)
  (ffn	:pointer)			; zmq_free_fn
  (hint	:pointer))

(defcfun* ("zmq_msg_close" %msg-close) :int
  (msg	msg))

(defcfun ("zmq_msg_move" %msg-move) :int
  (dest	msg)
  (src	msg))

(defcfun ("zmq_msg_copy" %msg-copy) :int
  (dest	msg)
  (src	msg))

(defcfun ("zmq_msg_data" %msg-data) :pointer
  (msg	msg))

(defcfun ("zmq_msg_size" %msg-size) :int
  (msg	msg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ infrastructure (a.k.a. context) initialisation & termination.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant poll 1)

(defcfun* ("zmq_init" init) :pointer
  (app-threads	:int)
  (io-threads	:int)
  (flags	:int))

(defcfun ("zmq_term" term) :int
  (context	:pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  0MQ socket definition.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant p2p 0)
(defconstant pub 1)
(defconstant sub 2)
(defconstant req 3)
(defconstant rep 4)
(defconstant xreq 5)
(defconstant xrep 6)
(defconstant upstream 7)
(defconstant downstream 8)

(defconstant hwm 1)
(defconstant lwm 2)
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

(defconstant noblock 1)
(defconstant more 2)

(defcfun* ("zmq_socket" socket) :pointer
  (context	:pointer)
  (type		:int))

(defcfun ("zmq_close" close) :int
  (s	:pointer))

(defcfun* ("zmq_setsockopt" %setsockopt) :int
  (s		:pointer)
  (option	:int)
  (optval	:pointer)
  (optvallen	:long))

(defcfun* ("zmq_bind" %bind) :int
  (s	:pointer)
  (addr	:pointer :char))

(defcfun* ("zmq_connect" %connect) :int
  (s	:pointer)
  (addr	:pointer :char))


(defcfun* ("zmq_send" %send) :int
  (s		:pointer)
  (msg		msg)
  :optional
  (flags	:int))

(defcfun* ("zmq_recv" %recv) :int
  (s		:pointer)
  (msg		msg)
  :optional
  (flags	:int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  I/O multiplexing.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant pollin 1)
(defconstant pollout 2)
(defconstant pollerr 4)

(defcstruct pollitem
  (socket	:pointer)
  (fd		:int)
  (events	:short)
  (revents	:short))

(defcfun ("zmq_poll" %poll) :int
  (items	:pointer)
  (nitems	:int)
  (timeout	:long))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Helper functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcfun ("zmq_stopwatch_start" stopwatch-start) :pointer)

(defcfun ("zmq_stopwatch_stop" stopwatch-stop) :ulong
  (watch	:pointer))

(defcfun ("zmq_sleep" sleep) :void
  (seconds	:int))

(defcfun ("zmq_version" %version) :void
  (major	:pointer)
  (minor	:pointer)
  (patch	:pointer))

(defcfun ("zmq_errno" errno) :int)
