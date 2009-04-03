;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/combat.lisp - combat factors
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-attack-description '<hit> "hits you")
(define-attack-description '<beg> "begs you for money")
(define-attack-description '<touch> "touches you")
(define-attack-description '<claw> "claws you")
(define-attack-description '<bite> "bites you")
(define-attack-description '<wail> "wails at you")
(define-attack-description '<gaze> "gazes at you")
(define-attack-description '<butt> "butts you")
(define-attack-description '<kick> "kicks you")
(define-attack-description '<spore> "releases spores at you")
(define-attack-description '<engulf> "engulfs you")
(define-attack-description '<insult> "insults you") ;; randomise
(define-attack-description '<moan> "moans at you") ;; randomise
(define-attack-description '<spit> "spits on you")
(define-attack-description '<crush> "crushes you")
(define-attack-description '<crawl> "crawls on you")
(define-attack-description '<sting> "stings you")
(define-attack-description '<drool> "drools on you")

(define-monster-attack '<hurt>
    :power 60
    :hit-effect (attack-effect (attacker target the-attack damage)
		    ;; alter for ac
		    (deduct-hp! target damage)
		    damage))
