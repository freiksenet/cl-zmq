;; This file is part of CL-ZMQ.

(defpackage #:zeromq
  (:nicknames :zmq)
  (:use :cl :cffi)
  (:shadow #:close #:identity #:push)
  (:export
   ;; constants
   #:affinity
   #:delimiter
   #:downstream
   #:efsm
   #:emthread
   #:enocompatproto
   #:forwarder
   #:hwm
   #:identity
   #:mcast-loop
   #:msg-shared
   #:msg-tbc
   #:noblock
   #:pair
   #:poll
   #:pollerr
   #:pollin
   #:pollout
   #:pub
   #:pull
   #:push
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
   #:dealer
   #:xrep
   #:router
   #:xreq

   #:events

   ;; structures
   #:msg
   #:pollitem

   ;; functions
   #:version

   #:ctx-new
   #:ctx-get
   #:ctx-set
   #:ctx-destroy

   #:socket
   #:close
   #:bind
   #:unbind
   #:connect
   #:disconnect
   #:getsockopt
   #:setsockopt

   #:msg-data-as-is
   #:msg-data-as-array
   #:msg-data-as-string
   #:msg-close
   #:msg-init-size
   #:msg-size
   #:msg-move
   #:msg-copy

   #:send
   #:recv
   #:msg-send
   #:msg-recv

   #:poll

   #:proxy

   ;; macros
   #:with-context
   #:with-polls
   #:with-socket

   ;; conditions
   #:zmq-error))

(in-package :zeromq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library zeromq
    (:darwin (:or "libzmq.0.dylib" "libzmq.dylib"))
    (:unix (:or "libzmq.so.0.0.0" "libzmq.so"))
    (:windows "libzmq.dll")
    (t "libzmq")))

(use-foreign-library zeromq)
