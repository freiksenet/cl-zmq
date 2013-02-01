;; Copyright (c) 2009, 2010, 2011 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(asdf:defsystem zeromq3
  :name "zeromq"
  :version "0.1.6"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :licence "LGPLv3"
  :description "Zero MQ 3 bindings"
  :depends-on (:cffi :trivial-garbage #-(or win32 windows) :iolib.syscalls)
  :serial t
  :components ((:file "package")
               (:file "meta")
               (:file "zeromq")
               (:file "zeromq-api")))
