;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/character.lisp - character settings
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

;; all these are temporary, just to get things running!!

(define-character-race "man" "Man"
  :symbol '<man>
  :desc "You are the last man alive."
  
  :xp-extra 0
  :hit-dice 10
  :base-age 18
  :mod-age '(1 . 10)
  :base-status 0

  ;; metric
  :m-height 170 :m-height-mod 15
  :f-height 160 :f-height-mod 15
  :m-weight 80  :m-weight-mod 20
  :f-weight 68  :f-weight-mod 15
  
  :classes t)

(define-character-class "hunter" "Hunter"
  :symbol '<hunter>
  :desc "You hunt and kill things for food and everything else you need."
  
  :titles '("Newbie Hunter" "Junior Hunter" "Aspiring Hunter"
	    "Spy" "Spy" "Spy"
	    "Spy" "Spy" "Spy" "James Bond")
  
  :starting-equipment '((obj :id "short-bow")
			(obj :id "stone-dagger")
			(obj :id "fur-cap")
			(obj :id "fur-vest")
			(obj :id "fur-boots")
			(obj :id "arrow" :amount 20)
			)

  ;; total points 95
  :skills '((<leatherwork> 10)
	    (<monster-lore> 15)
	    (<object-lore> 5)
	    (<intuition> 10)
	    (<unarmed> 5)
	    (<blades> 10)
	    (<bludgeoning> 5)
	    (<archery> 15)
	    (<light-armour> 5)
	    (<evasion> 15))
  )

(define-character-stat '<str> "strength"
  :abbreviation "Str" :number 0 :bit-flag #x01 :positive-desc "strong" :negative-desc "weak")

(define-character-stat '<agi> "agility"
  :abbreviation "Agi" :number 1 :bit-flag #x02 :positive-desc "agile" :negative-desc "clumsy")

(define-character-stat '<con> "constitution"
  :abbreviation "Con" :number 2 :bit-flag #x04 :positive-desc "healthy" :negative-desc "sickly")

(define-character-stat '<dis> "discipline"
  :abbreviation "Dis" :number 3 :bit-flag #x08 :positive-desc "focused" :negative-desc "weak-willed")

(define-character-stat '<rea> "reasoning"
  :abbreviation "Rea" :number 4 :bit-flag #x10 :positive-desc "clever" :negative-desc "dumb")

(define-character-stat '<pre> "presence"
  :abbreviation "Pre" :number 5 :bit-flag #x20 :positive-desc "celebrity" :negative-desc "anonymous")
