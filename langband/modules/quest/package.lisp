;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: modules/quest/package.lisp - package def for dialogue module

|#

(in-package :cl-user)

(defpackage :org.langband.quest
  (:nicknames :lb-quest :quest)
  (:use :common-lisp :org.langband.engine)
  (:export #:*variant-class*
	   #:quest
	   #:quest.desc
	   #:quest.giver
	   #:quest.id
	   #:quest.parent
	   #:quest.state
	   #:quest.step
	   #:quest.steps
	   #:quest.taker
	   #:quest.title
	   #:quest-condition
	   #:defquest
	   #:find-quest
	   #:doing-quest?
	   #:done-quest?
	   #:quest-event
	   #:quest-available?
	   #:quest-status
	   #:init-quest
	   #:advance-quest
	   #:finish-quest
	   #:print-quests

	   ;; may be moved
	   #:id
	   #:steps
	   #:desc
	   #:title
	   ))
