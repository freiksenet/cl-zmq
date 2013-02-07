;; This file is part of CL-ZMQ.

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op 'cffi-grovel))

(asdf:defsystem zeromq
  :name "zeromq"
  :version "0.2.0"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :maintainer "Mikhail Novikov <freiksenet@gmail.com>"
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
