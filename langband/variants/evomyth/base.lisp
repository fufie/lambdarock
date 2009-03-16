;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/base.lisp - the base variant class for evomyth
Copyright (c) 2000-2003, 2009 - Stig Erik Sandoe

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
  

(defclass evo/valley (themed-level)
  ((id :initform "valley")
   (symbol :initform 'valley)
   ))


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
(define-object-type blade :key <blade> :is melee-weapon)

(define-object-type ranged-weapon :is weapon)
(define-object-type bow :key <bow> :is ranged-weapon)


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

