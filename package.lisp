;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(defpackage #:zeromq
  (:nicknames :zmq)
  (:use :cl :cffi)
  (:shadow #:sleep #:close)
  (:export
   ;; constants
   #:affinity
   #:delimiter
   #:downstream
   #:efsm
   #:emthread
   #:enocompatproto
   #:forwarder
   #:hausnumero
   #:hwm
   #:identity
   #:lwm
   #:max-vsm-size
   #:mcast-loop
   #:msg-shared
   #:msg-tbc
   #:noblock
   #:p2p
   #:poll
   #:pollerr
   #:pollin
   #:pollout
   #:pub
   #:queue
   #:rate
   #:raw
   #:rcvmore
   #:recovery-ivl
   #:rep
   #:req
   #:sndmore
   #:streamer
   #:sub
   #:subscribe
   #:swap
   #:unsubscribe
   #:upstream
   #:vsm

   #:events

   ;; structures
   #:msg
   #:pollitem

   ;; functions
   #:bind
   #:close
   #:connect
   #:device
   #:errno
   #:getsockopt
   #:init
   #:msg-close
   #:msg-copy
   #:msg-data-as-array
   #:msg-data-as-is
   #:msg-data-as-string
   #:msg-init
   #:msg-init-data
   #:msg-init-size
   #:msg-move
   #:msg-raw
   #:msg-size
   #:msg-type
   #:poll
   #:pollitem-events
   #:pollitem-fd
   #:pollitem-raw
   #:pollitem-revents
   #:pollitem-socket
   #:recv
   #:send
   #:setsockopt
   #:sleep
   #:socket
   #:stopwatch-start
   #:stopwatch-stop
   #:strerror
   #:term
   #:version

   ;; macros
   #:with-context
   #:with-polls
   #:with-socket
   #:with-stopwatch

   ;; conditions
   #:error-again))

(in-package :zeromq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library zeromq
    (:unix (:or "libzmq.so.0.0.0" "libzmq.so"))
    (:windows "libzmq.dll")
    (t "libzmq")))

(use-foreign-library zeromq)
