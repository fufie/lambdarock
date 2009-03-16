;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#||

DESC: variants/evomyth/people/grandpa.lisp 
Copyright (c) 2009 - Stig Erik Sandoe

||#

(in-package :org.langband.evomyth)

(define-conversation (player npc)
    (:id "grandparent")
  (:text "Hello ~A, how can I help you?" (if (is-female? player) "granddaughter" "grandson"))
  (:option (:text "How can I get out of the valley?")
	   (:node (:text "On the other side of the lake is a prickly forest.  Inside it you will find an old tree.  Inside the old tree is a shaft down to a cave under us.  Somehwere in the cave there is an exit to the outside world.  My grandparents came here many many winters ago to escape the dangers of the outside world, hoping we could thrive here.  But now that there are no ~A anymore, you need to venture out to search for a ~A."  (if (is-female? player) "men" "women") (if (is-female? player) "man" "woman"))
		  (:dest :back "I see."))))


(define-conversation (player npc)
    (:id "grandpa")
  (:include "grandparent")
  (:text "~A, how can I help?" (player.name player))
  (:quit-option "Please excuse me, I'm off."))


(define-conversation (player npc)
    (:id "grandma")
  (:include "grandparent")
  (:text "~A, how can I help?" (player.name player))
  (:quit-option "Please excuse me, I'm off."))



