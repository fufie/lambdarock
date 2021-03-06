;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; trivial-features.asd --- ASDF system definition.
;;;
;;; Copyright (C) 2007, Luis Oliveira  <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#-(or sbcl clisp allegro openmcl lispworks ecl cmu scl cormanlisp)
(error "Sorry, your Lisp is not supported.  Patches welcome.")

(defpackage #:trivial-features-asd
  (:use :cl :asdf))

(in-package :trivial-features-asd)

(defsystem trivial-features
  :description "Ensures consistent *FEATURES* across multiple CLs."
  :author "Luis Oliveira <loliveira@common-lisp.net>"
  :version "0.4"
  :licence "MIT"
  :components
  ((:module src
    :serial t
    :components
    (#+allegro    (:file "tf-allegro")
     #+clisp      (:file "tf-clisp")
     #+cmu        (:file "tf-cmucl")
     #+cormanlisp (:file "tf-cormanlisp")
     #+ecl        (:file "tf-ecl")
     #+lispworks  (:file "tf-lispworks")
     #+openmcl    (:file "tf-openmcl")
     #+sbcl       (:file "tf-sbcl")
     #+scl        (:file "tf-scl")
     ))))

(defmethod perform ((o test-op) (c (eql (find-system 'trivial-features))))
  (operate 'load-op 'trivial-features-tests)
  (operate 'test-op 'trivial-features-tests))

;; vim: ft=lisp et
