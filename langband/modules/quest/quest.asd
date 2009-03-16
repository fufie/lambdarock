;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/quest/quest.asd - system-def for quest module
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :cl-user)

(defpackage :org.langband.quest.system
  (:use :cl :asdf))

(in-package :org.langband.quest.system)

(asdf:defsystem :lbmodule-quest
    :version "0.1.4"
    :components ((:file "package")
		 (:file "interface" :depends-on ("package"))
		 (:file "implementation" :depends-on ("interface"))
		 (:file "print" :depends-on ("interface")))
    :depends-on (langband-engine))

