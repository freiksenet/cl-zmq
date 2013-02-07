;; Copyright (c) 2009, 2010, 2011 Vitaly Mayatskikh <v.mayatskih@gmail.com>
;;
;; This file is part of CL-ZMQ.
;;
;; Vitaly Mayatskikh grants you the rights to distribute
;; and use this software as governed by the terms
;; of the Lisp Lesser GNU Public License
;; (http://opensource.franz.com/preamble.html),
;; known as the LLGPL.

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem zeromq
  :name "zeromq"
  :version "0.2.0"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :licence "LGPLv3"
  :description "Zero MQ 3 bindings"
  :depends-on (:cffi :trivial-garbage)
  :serial t
  :components
  ((:module "src"
    :serial t
    :components
    ((:file "package")
     (cffi-grovel:grovel-file "grovel")
     (:file "meta")
     (:file "zeromq")
     (:file "zeromq-api")))))
