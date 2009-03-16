;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: variants/contraband/package.lisp - package def for contraband

|#

(in-package :cl-user)

(defpackage :org.langband.contraband
  (:nicknames :lb-contra :contra :lbc)
  (:use :common-lisp :org.langband.engine
	:org.langband.dialogue
	:org.langband.quest))
