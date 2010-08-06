;; Copyright (c) 2009, 2010 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(asdf:defsystem zeromq
  :name "zeromq"
  :version "0.1.1"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :licence "LGPLv3"
  :description "Zero MQ 2 bindings"
  :depends-on (:cffi :trivial-garbage #-windows :iolib.syscalls)
  :serial t
  :components ((:file "package")
               (:file "meta")
               (:file "zeromq")
               (:file "zeromq-api")))
