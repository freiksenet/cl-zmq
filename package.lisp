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
   #:recovery-ivl
   #:rep
   #:req
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
