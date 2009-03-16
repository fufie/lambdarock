;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/quest/package.lisp - package def for dialogue module

|#

(in-package :cl-user)

(defpackage :org.langband.quest
  (:nicknames :lb-quest :quest)
  (:use :common-lisp :org.langband.engine)
  (:export #:*variant-class*
	   #:make-coord-event
	   #:coord-event-x
	   #:coord-event-y
	   #:coord-event-quest
	   #:coord-event-trigger
	   #:quest
	   #:quest.desc
	   #:quest.id
	   #:quest.parent
	   #:quest.state
	   #:quest.step
	   #:quest.title
	   #:quest-condition
	   #:defquest
	   #:find-quest
	   #:quest-event
	   #:quest-available?
	   #:quest-status
	   #:init-quest
	   #:advance-quest
	   #:finish-quest

	   ;; may be moved
	   #:*coord-events*
	   ))
