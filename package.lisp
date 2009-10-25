;; Copyright 2009 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is a part of CL-ZMQ
;;
;; CL-ZMQ is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; CL-ZMQ is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defpackage #:cl-zmq
  (:nicknames :zmq)
  (:use :cl :cffi :closer-mop)
  (:shadow #:sleep #:close)
  (:export
   ;; constants
   #:hausnumero
   #:emthread
   #:efsm
   #:enocompatproto
   #:max-vsm-size
   #:delimiter
   #:vsm
   #:poll
   #:pollin
   #:pollout
   #:p2p
   #:pub
   #:sub
   #:req
   #:rep
   #:hwm
   #:lwm
   #:swap
   #:affinity
   #:identity
   #:subscribe
   #:unsubscribe
   #:rate
   #:recovery-ivl
   #:mcast-loop
   #:noblock
   #:noflush

   #:events

   ;; structures
   #:msg
   #:pollitem

   ;; functions
   #:make-message
   #:strerror
   #:msg-init
   #:msg-init-size
   #:msg-init-data
   #:msg-close
   #:msg-move
   #:msg-copy
   #:msg-data-as-string
   #:msg-data-as-array
   #:msg-size
   #:msg-type
   #:init
   #:term
   #:socket
   #:close
   #:setsockopt
   #:bind
   #:connect
   #:send
   #:flush
   #:recv
   #:stopwatch-start
   #:stopwatch-stop
   #:sleep
   #:poll
   #:pollitem-socket
   #:pollitem-fd
   #:pollitem-events
   #:pollitem-revents

   ;; macros
   #:with-context
   #:with-socket
   #:with-stopwatch
   #:with-poll

   ;; conditions
   #:error-again))

(in-package :cl-zmq)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library zmq
    (:unix (:or "libzmq.so.0.0.0" "libzmq.so"))
    (t "libzmq")))

(use-foreign-library zmq)
