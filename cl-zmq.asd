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

(cl:eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op :cffi-grovel))

(defpackage #:cl-zmq-asd
  (:use :cl :asdf))

(in-package #:cl-zmq-asd)

(defsystem cl-zmq
  :name "cl-zmq"
  :version "0.1"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :licence "GPLv3"
  :description "Zero MQ bindings"
  :serial t
  :components ((:file "package")
	       (cffi-grovel:grovel-file "grovel" :depends-on ("package"))
               (:file "zmq" :depends-on ("grovel"))
               (:file "zmq-api" :depends-on ("zmq"))))
