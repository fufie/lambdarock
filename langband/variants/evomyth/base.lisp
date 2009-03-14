;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/base.lisp - the base variant class for evomyth
Copyright (c) 2000-2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.evomyth)

(defconstant +base-hitdice+ 10)

(defclass evomyth (variant)
  ((dawn-time     :initarg :dawntime   :initform 0    :accessor variant.dawn)
   (twilight-time :initarg :twilight   :initform 6000 :accessor variant.twilight)
   (town-seed         :initform nil  :accessor variant.town-seed)
   (used-scroll-names :initform (make-hash-table :test #'equal) :accessor variant.used-scroll-names)
   
   (gold-table     :initarg :gold-table
		   :initform nil
		   :accessor variant.gold-table)
   (spells         :initarg :spells
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spells)

   (spellbooks     :initarg :spellbooks
		   :initform (make-hash-table :test #'equal)
		   :accessor variant.spellbooks)

   ;; this length should be calculated accurately to allow for iteration
   (skills :initform (make-array 30 :initial-element nil)
	   :accessor variant.skills)


   (quests :initform (make-hash-table :test #'equal)
	   :accessor variant.quests)
   
   ;; 50 is max-level
   (max-charlevel :initform 50)
   ;; put xp-table here right away
   (xp-table :initform #1A(     10       25       45       70      100
			       140      200      280      380      500
			       650      850     1100     1400     1800
			      2300     2900     3600     4400     5400
			      6800     8400    10200    12500    17500
			     25000    35000    50000    75000   100000
			    150000   200000   275000   350000   450000
			    550000   700000   850000  1000000  1250000
			   1500000  1800000  2100000  2400000  2700000
			   3000000  3500000  4000000  4500000  5000000))

   (legal-effects :initarg :legal-effects
                  :initform '(:quaff :read :eat :create :add-magic :use)
                  :accessor variant.legal-effects)
;;   (object-effects :initarg :object-effects :initform (make-hash-table :test #'equal)
;;		   :accessor variant.object-effects)

   ))

(defclass evo/monster-kind (monster-kind)
  ((picture :accessor monster.picture
	    :initform nil)))

(defclass npc (active-monster)
  ((attitude :accessor npc.attitude
	     :initform 0
	     :documentation "Attitude towards the player, (negative=bad, 0, positive=good), nil means not init'ed.")
   ))
	     
(defstruct (evo/skill (:conc-name evo/skill.))
  id
  slot
  alias
  desc
  idx
  cost)
  

(defclass evo/town (themed-level)
  ((id     :initform "town-level")
   (symbol :initform 'town-level)
   (stores        :initarg :stores     :initform nil  :accessor level.stores)
   (num-stores    :initarg :num-stores :initform 8    :accessor level.num-stores)
   (home-num      :initarg :home-num   :initform 7    :accessor level.home-num))

  (:documentation "Evomyth has a special (two) town-level with
stores and special behaviour.  The class is used for dispatching."))

(defclass warehouse-level (themed-level)
  ((id :initform "warehouse")
   (symbol :initform 'warehouse)
   ))


(defclass quest ()
  ((id    :accessor quest.id    :initform nil :initarg :id
	  :documentation "A string id.")
   (title :accessor quest.title
	  :initform nil
	  :initarg :title
	  :documentation "A title to use when presenting the quest.")
   (desc  :accessor quest.desc  :initform nil
	  :documentation "A description of the quest to put on a quest page.")
   (state :accessor quest.state :initform :not-started
	  :documentation "what is the current state of the quest?")
   (step  :accessor quest.step  :initform :init
	  :documentation "specifies at what step we are at.. :init and :finish being special values.")
   (steps :accessor quest.steps :initform nil
	  :documentation "steps within a quest, typically pointers to subquests.")
   (giver :accessor quest.giver :initform nil
	  :documentation "Who gave this quest.")
   (taker :accessor quest.taker :initform nil
	  :documentation "Who is doing this quest")
   (parent :accessor quest.parent :initform nil
	   :documentation "If it is a subquest, PARENT should point to the parent quest.")
   ))

(define-condition quest-problem (error)
  ((id   :initarg :id   :reader problem.id)
   (desc :initarg :desc :reader problem.desc)))

(defgeneric get-melee-weapon (creature))
  
(defgeneric quest-available? (variant quest quest-giver quest-taker)
  (:documentation "Checks if a quest can be taken by (ie 'is available for') the quest-taker."))

(defgeneric quest-status (variant quest quest-taker)
  (:documentation "Returns the status of the quest.. :active, :not-started, :success, :failure being some possible
returned results."))
  
(defgeneric init-quest (variant quest quest-giver quest-taker)
  (:documentation "Initialisation of the quest, which does the init of all settings."))

(defgeneric advance-quest (variant quest quest-taker &key from to giver)
  (:documentation "Advances a quest to the next step, which might be the end."))

(defgeneric finish-quest (variant quest quest-taker)
  (:documentation "Cleanup actions for the quest."))

(defgeneric print-armour-class (variant player setting)
  (:documentation "Prints armour class in left frame."))

;;; define relevant object-types for evomyth

(define-object-type weapon :key <weapon>)
(define-object-type melee-weapon :is weapon)
(define-object-type long-blade :key <long-blade> :is melee-weapon)
(define-object-type short-blade :key <short-blade> :is melee-weapon)

(define-object-type armour :key <armour>
	      :kind-slots ((armour-skill  :accessor object.armour-skill  :initform '<light>)
			   (armour-bulk   :accessor object.armour-bulk   :initform 0))
              :aobj-slots ((armour-rating :accessor aobj.armour-rating   :initform 0)))

(define-object-type headgear :is armour :key <headgear>)
(define-object-type cloak :is armour :key <cloak>)
(define-object-type body-armour :is armour :key <body-armour>)
(define-object-type gloves :is armour :key <gloves>)
(define-object-type legwear :is armour :key <legwear>)
(define-object-type boots :is armour :key <boots>)
(define-object-type shield :is armour :key <shield>)

(define-object-type letter :key <letter>)
(define-object-type ring :key <ring>)
(define-object-type neckwear :key <neckwear>)
(define-object-type amulet :is neckwear :key <amulet>)

(define-object-type container :key <container>)

(defvar *evomyth-images* #(
#| 0 |# "" 
        "" 
        "" 
        (engine-gfx "tiles/dg_armor32.png") 
        (engine-gfx "tiles/dg_effects32.png") 
#| 5 |# (engine-gfx "tiles/dg_food32.png") 
        (engine-gfx "tiles/dg_classm32.png") 
        (engine-gfx "tiles/dg_humans32.png") 
        (engine-gfx "tiles/upd_jewels.png")
        (engine-gfx "tiles/dg_magic32.png") 
#| 10 |#(engine-gfx "tiles/dg_misc32.png") 
        (engine-gfx "tiles/dg_potions32.png") 
        (engine-gfx "tiles/dg_wands32.png") 
        (engine-gfx "tiles/dg_weapons32.png") 
        (engine-gfx "tiles/dg_people32.png") 
#| 15 |#(engine-gfx "tiles/dg_dragon32.png") 
        (engine-gfx "tiles/dg_monster132.png") 
        (engine-gfx "tiles/dg_monster232.png") 
        (engine-gfx "tiles/dg_monster332.png") 
        (engine-gfx "tiles/dg_monster432.png") 
#| 20 |#(engine-gfx "tiles/dg_monster532.png") 
        (engine-gfx "tiles/dg_monster632.png") 
        (engine-gfx "tiles/dg_monster732.png") 
        (engine-gfx "tiles/dg_undead32.png") 
        (engine-gfx "tiles/dg_uniques32.png") 
#| 25 |#(engine-gfx "tiles/dg_dungeon32.png") 
        (engine-gfx "tiles/dg_grounds32.png") 
        (engine-gfx "tiles/dg_extra132.png") 
        (engine-gfx "tiles/dg_town032.png") 
        (engine-gfx "tiles/dg_town132.png") 
#| 30 |#(engine-gfx "tiles/dg_town232.png") 
        (engine-gfx "tiles/dg_town332.png") 
        (engine-gfx "tiles/dg_town432.png") 
        (engine-gfx "tiles/dg_town532.png") 
        (engine-gfx "tiles/dg_town632.png") 
#| 35 |#(engine-gfx "tiles/dg_town732.png") 
        (engine-gfx "tiles/dg_town832.png")
        (engine-gfx "tiles/dg_town932.png")
	(engine-gfx "tiles/buttons.png")
	""		      
#| 40 |#(engine-gfx "tiles/crosshair.png")
	(engine-gfx "tiles/summer.png")
	(variant-gfx "tiles/greendress.png")
	(variant-gfx "tiles/various.png")
	(variant-gfx "tiles/alphabet.png") ;; temporary one for runesystem
#| 45 |#(variant-gfx "tiles/keyrow.png") ;; temporary one for runesystem
	(variant-gfx "tiles/red-pointers.png") ;; temporary one for runesystem
	(variant-gfx "tiles/clothing.png")		      
	))


;; path tweaking needed!!!
(defun make-evomyth-obj ()
  (make-instance 'evomyth
		 :id "evomyth"
		 :name "Evomyth"
		 :num-version 7
		 :stat-length 6

		 :gfx-path
		 #+langband-development
		 "./variants/evomyth/data/graphics/"
		 #-langband-development
		 "/var/games/evomyth/data/graphics/"
		 
		 :config-path
		 #+langband-development
		 "./variants/evomyth/"
		 #-langband-development
		 "/var/games/evomyth/"))


(register-variant& "evomyth" #'make-evomyth-obj
		   :desc "Evomyth is all about rum, smuggling and pretty girls.")

#||
;; see config/skills.lisp
(defclass evo/skills ()
  ((smithing        :accessor skills.smithing        :initform 0)
   (leatherwork     :accessor skills.leatherwork     :initform 0)
   (woodcraft       :accessor skills.woodcraft       :initform 0)
   (gemcutting      :accessor skills.gemcutting      :initform 0)
   (mining          :accessor skills.mining          :initform 0)
   (appraising      :accessor skills.appraising      :initform 0)
   (trade           :accessor skills.trade           :initform 0)
   (conversation    :accessor skills.conversation    :initform 0)
   (monster-lore    :accessor skills.monster-lore    :initform 0)
   (object-lore     :accessor skills.object-lore     :initform 0)
   (history         :accessor skills.history         :initform 0)
   (intuition       :accessor skills.intuition       :initform 0)
   (languages       :accessor skills.languages       :initform 0)
   (animal-handling :accessor skills.animal-handling :initform 0)
   (riding          :accessor skills.riding          :initform 0)
   (tracking        :accessor skills.tracking        :initform 0)
   (alchemy         :accessor skills.alchemy         :initform 0)
   (magic-devices   :accessor skills.magic-devices   :initform 0)
   (pick-pocket     :accessor skills.pick-pocket     :initform 0)
   (security        :accessor skills.security        :initform 0)
   (mechanical      :accessor skills.mechanical      :initform 0)
   ;; combat
   (unarmed         :accessor skills.unarmed         :initform 0)
   (short-blades    :accessor skills.short-blades    :initform 0)
   (long-blades     :accessor skills.long-blades     :initform 0)
   (polearms        :accessor skills.polearms        :initform 0)
   (bludgeoning     :accessor skills.bludgeoning     :initform 0)
   (archery         :accessor skills.archery         :initform 0)
   (shield          :accessor skills.shield          :initform 0)
   (heavy-armour    :accessor skills.heavy-armour    :initform 0)
   (light-armour    :accessor skills.light-armour    :initform 0)
   (evasion         :accessor skills.evasion         :initform 0)
   ))
||#
