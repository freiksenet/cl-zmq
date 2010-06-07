;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(in-package :zeromq-test)

;(defvar *address* "pgm://lo;224.0.0.1:8000")
(defvar *bind-address* "tcp://lo:8000")
(defvar *connect-address* "tcp://localhost:8000")
(defvar *message-count* 1000)
(defvar *message-size* 32)
(defvar *rate* 256)
