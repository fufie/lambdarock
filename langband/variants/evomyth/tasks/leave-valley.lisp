;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/tasks/leave-valley.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defquest leave-the-valley ()
  :id "leave-valley"
  :title "Leave the Valley"
  :desc "You need to leave the valley to find a partner.")

;; this one is a bit bigger so we'll use a full method
(defmethod finish-quest ((variant evomyth)
			 (quest leave-the-valley) quest-taker)

  (call-next-method)
  (modify-xp! quest-taker 100)

  (ask-for-update! quest-taker '[bonuses])

  quest)