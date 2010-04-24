;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi)
  (asdf:operate 'asdf:load-op :trivial-garbage)
#-windows
  (asdf:operate 'asdf:load-op :iolib.syscalls))

(defpackage #:zeromq-asd
  (:use :cl :asdf))

(in-package #:zeromq-asd)

(defsystem zeromq
  :name "zeromq"
  :version "0.1"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :licence "LGPLv3"
  :description "Zero MQ 2 bindings"
  :serial t
  :components ((:file "package")
               (:file "meta")
               (:file "zeromq")
               (:file "zeromq-api")))
