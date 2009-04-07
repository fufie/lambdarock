;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: variants/evomyth/evomyth.asd - system-def for evomyth
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :cl-user)

;; we need certain flags
(eval-when (:execute :load-toplevel :compile-toplevel)
  #+(or cmu allegro sbcl lispworks)
  (pushnew :enough-support-for-langband *features*)
  #+(and clisp langband-development) ;; clisp not ready in debian yet
  (pushnew :enough-support-for-langband *features*)
  )

(defpackage :evomyth-system 
  (:use :cl :asdf))

(in-package :evomyth-system)

(asdf:defsystem :evomyth
    :version "0.0.0"
    :components ((:file "package")
		 (:file "base" :depends-on ("package"))
		 (:file "variant" :depends-on ("base"))
		 (:file "levels" :depends-on ("variant"))
		 (:file "creatures" :depends-on ("variant"))
		 (:file "objects" :depends-on ("variant"))
		 (:file "player" :depends-on ("creatures"))
		 (:file "abilities" :depends-on ("variant"))
		 (:file "strategies" :depends-on ("variant"))
		 (:file "combat" :depends-on ("variant"))
		 (:file "attitude" :depends-on ("variant"))
		 (:file "print" :depends-on ("combat"))
		 (:file "keys" :depends-on ("print" "attitude"))
		 (:file "wizard" :depends-on ("keys"))
		 
		 )
    :depends-on (langband-engine lbmodule-dialogue lbmodule-quest))

#-enough-support-for-langband
(warn "Evomyth has not been tested with '~a ~a', skips compilation."
      (lisp-implementation-type)
      (lisp-implementation-version))

