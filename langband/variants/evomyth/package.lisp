;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: variants/evomyth/package.lisp - package def for evomyth

|#

(in-package :cl-user)

(defpackage :org.langband.evomyth
  (:nicknames :lb-evo :evo :lbc)
  (:use :common-lisp :org.langband.engine
	:org.langband.dialogue))
