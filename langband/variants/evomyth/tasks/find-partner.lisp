;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/tasks/find-partner.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defquest find-partner ()
  :id "find-partner"
  :title "Find a partner"
  :desc "To ensure the survival of mankind you need to find a partner of the opposite sex.")

(defmethod finish-quest ((variant evomyth)
			 (quest find-partner) quest-taker)

  (call-next-method)
  (modify-xp! quest-taker 1000)

  (ask-for-update! quest-taker '[bonuses])

  quest)