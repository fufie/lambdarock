;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: classes.lisp - The major classes and structs for langband
Copyright (c) 2002-2004, 2009 - Stig Erik Sandoe

||#

(in-package :org.langband.engine)

;;; Classes in langband are the first-class objects, structs are considered
;;; for really simple "classes" but might be turned into classes later.
;;; The DUNGEON struct is a struct in the hope that it'll make it faster,
;;; as the DUNGEON is frequently accessed

;;; currently in no specified order

(defclass activatable ()
  ((activated :reader activated? :initform nil))
  (:documentation "Mixin-class for activation of objects,
may be removed later for efficiency-reasons.  It enforces a
protocol that allows activated? to be set automagically after
a succesful ACTIVATE-OBJECT."))

(defclass variant (activatable)
  ((id        :accessor get-id
	      :initform "lithping"
	      :initarg :id)
   
   (name      :accessor variant.name
	      :initform "lithping"
	      :initarg :name)

   (version   :accessor variant.version
	      :initform "1.0"
	      :documentation "A string describing the version, useful for displaying."
	      :initarg :version)

   (num-version :accessor variant.num-version
		:initform 100
		:initarg :num-version
		:documentation "A never-displayed version-number that code
can use for compatibility checks, savegames and internal use.  version is
for display, num-version for active-use. u16b should be enough.")
   
   (config-path :accessor variant.config-path
		:initform nil
		:initarg :config-path
		:documentation "where are the configuration-files?")
   
   (gfx-path :accessor variant.gfx-path
	     :initform nil
	     :initarg :gfx-path
	     :documentation "What is the path to variant specific graphics?")

   ;; the rest can be done lazily

   (genders   :accessor variant.genders
	      :initform '()
	      :documentation "List of legal genders for players and monsters.")
   
   (races     :accessor variant.races
	      :initform (make-hash-table :test #'equal))
   
   (classes   :accessor variant.classes
	      :initform (make-hash-table :test #'equal))

   (effects   :accessor variant.effects
	      :initform '()
	      :documentation "List of legal effects and effects to handle for variant.")
   
   (elements  :accessor variant.elements
	      :initform '()
	      :documentation "List of legal elements and elements to handle for variant.")
   
   (turn      :accessor variant.turn
	      :initform 0
	      :initarg :turn)

   (turn-events :accessor variant.turn-events
		:initform (make-hash-table :test #'equal))


   ;; a level builder is a constructor that must be funcalled
   ;; the key is the level-id
   (level-builders :accessor variant.level-builders
		   :initform (make-hash-table :test #'equal))

   (floor-types :accessor variant.floor-types
		:initform (make-hash-table :test #'equal))

   (room-types  :accessor variant.room-types
		:initform (make-hash-table :test #'equal))

     
   (max-depth      :accessor variant.max-depth
		   :initform 128)
     
   (max-charlevel  :accessor variant.max-charlevel
		   :initform 50)
     
   (xp-table  :accessor variant.xp-table
	      ;; maybe have a default? or maybe not
	      ;; it should be an array of size max-charlevel
	      :initform nil)

   (stats :accessor variant.stats
	  :initform nil)
   
   (stat-length :accessor variant.stat-length
		:initarg :stat-length
		:initform 0)
   
   ;; these are just monster-types.. not actual monsters
   (monsters :accessor variant.monsters
	     :initform (make-hash-table :test #'equal)
	     :documentation "these are just monster-types.. not active monsters.")

   (objects :accessor variant.objects
	    :initform (make-hash-table :test #'equal)
	    :documentation "these are just object-types.. not active objects.")

   (monsters-by-level :accessor variant.monsters-by-level
		      :initform (make-hash-table :test #'equal)
		      :documentation "these are monster-types organised by levels.")

   (objects-by-level :accessor variant.objects-by-level
		     :initform (make-hash-table :test #'equal)
		     :documentation "these are object-types organised by levels.")
   
   (traps :accessor variant.traps
	  :initform (make-hash-table :test #'equal)
	  :documentation "A table with trap-types.")

   (doors :accessor variant.doors
	  :initform (make-hash-table :test #'equal)
	  :documentation "A table with door-types.")

   
   (filters :accessor variant.filters
	    :initform (make-hash-table :test #'equal))
     
   (flavour-types :accessor variant.flavour-types
		  :initform (make-hash-table :test #'equal))

   (house-types :accessor variant.house-types
		:initform (make-hash-table))
     
   (house-owners :accessor variant.house-owners
		 :initform (make-hash-table))

   (attack-descriptions :accessor variant.attack-descriptions
			:initform (make-hash-table :test #'eq))
   
   (attack-types :accessor variant.attack-types
		 :initform (make-hash-table :test #'eq))

   (visual-effects :accessor variant.visual-effects
		   :initform (make-hash-table :test #'equal))
   
   (visual-states :accessor variant.visual-states
		  :initform '()
		  :documentation "The various states that can be shown for the player.")
  
   (day-length      :accessor variant.day-length
		    :initform 10000)

   (help-topics :accessor variant.help-topics
		:initform (make-hash-table :test #'equal))

   (settings :accessor variant.settings
	     :initform (make-hash-table :test #'equal) ;; maybe #'eq is enough?
	     :documentation "table with settings for various parts of the code, see later")

   (event-types :accessor variant.event-types
		:initform (make-hash-table :test #'equal) ;; maybe #'eq is enough?
		:documentation "table with known events that can occur.")

   (worn-item-slots :accessor variant.worn-item-slots
		    :initform nil)

   (images :accessor variant.images
	   :initform nil
	   :documentation "An array of relevant images to a variant.")
   
   (quests :accessor variant.quests
	   :initform nil)

   (strategies :accessor variant.strategies
	       :initform (make-hash-table :test #'eq)
	       :documentation "Maps a symbol to a constructor-function")
	   
   ;; this one is crucial, with lowercase string-keys it stores information that
   ;; is easy to check and save/load
   (information :accessor variant.information
		:initform (make-hash-table :test #'equal))
   
   ))

(defstruct (worn-item-slot (:copier nil)
			   (:predicate nil))
  key
  desc
  types
  hidden)

(defstruct (dungeon-coord (:conc-name coord.)
			  (:predicate nil)
			  (:copier nil))
;;  (floor 0 :type u16b)
  (floor nil)
  (flags 0 :type u16b)  ;; info-flag in angband
  (objects nil)
  (monsters nil)
  (decor nil)
  (repaint nil)
  )

;; Remember to update [save|load|checking].lisp when updating this one
(defstruct (dungeon (:conc-name dungeon.)
		    (:predicate nil)
		    (:copier nil))
  
  (depth 0 :type fixnum)  ; just a fixnum

  (height 0 :type u-fixnum) ;; height of table below
  (width  0 :type u-fixnum) ;; width of table below

  (table nil)


  ;; enable these two later if the need should arise
;;  (up-stairs-p nil)
;;  (down-stairs-p nil)
  (action-queue nil)
  (monsters nil)
  (objects nil)
  (rooms nil)
  (decor nil) ;; list of all decor
  (active nil)
  (triggers nil) ;; can't be used yet
  )

#||
(defclass settings ()
  ((name   :accessor setting.name
	   :initform "No-name"
	   :initarg :name)
   (events :accessor setting.events
	   :initform nil
	   :initarg nil)))
||#

(defclass effect ()
  ((symbol   :reader effect.symbol   :initarg :symbol)
   (name     :reader effect.name     :initarg :name)
   (bit-flag :reader effect.bit-flag :initarg :bit-flag)
   (number   :reader effect.number   :initarg :number)
   ))

;;; effects
;; fast, slow, blind, prot-evil, shielded, afraid, cut, stun, blessed,
;; hero, super-hero, berserk, poisoned, slow-digest, invulnerable,
;; hallucinate, confused, paralysed, telepathy, invisibility, see-inv,
;; random-teleport, hold-life, ... 


(defclass element ()
  ((symbol   :reader element.symbol   :initarg :symbol)
   (name     :reader element.name     :initarg :name)
   (bit-flag :reader element.bit-flag :initarg :bit-flag)
   (number   :reader element.number   :initarg :number)
   ))


(defclass creature-attribute ()
  ((name  :accessor attr.name  :initform ""  :initarg :name)
   (key   :accessor attr.key   :initform nil :initarg :key)
   (type  :accessor attr.type  :initform nil :initarg :type)
   (desc  :accessor attr.desc  :initform ""  :initarg :desc)
   (value :accessor attr.value :initform 0   :initarg :value)
   (value-type    :accessor attr.value-type
		  :initform 'boolean
		  :initarg :value-type)
   (default-value :accessor attr.default-value
                  :initform 0
		  :initarg :default-value)
   ))

(defclass temp-creature-attribute (creature-attribute)
  ((duration       :accessor attr.duration
		   :initform 0
		   :initarg :duration
		   :documentation "How long duration has the effect.")
   (turned-on-msg  :accessor attr.turned-on-msg
		   :initform nil
		   :initarg :turned-on-msg
		   :documentation "A string that is printed when the attribute/state is turned on.")
   (turned-off-msg :accessor attr.turned-off-msg
		   :initform nil
		   :initarg :turned-off-msg
		   :documentation "A string that is printed when the attribute/state is turned off.")
   (update-fun     :accessor attr.update-fun
		   :initform nil
		   :initarg :update-fun
		   :documentation "Function that is called whenever duration/state is altered.")
   (on-update      :accessor attr.on-update
	           :initform nil
	           :initarg :on-update
	           :documentation "If function exists, it is called after the state is changed off/on.")
   ))
  

(defclass misc-player-info ()
  ((age    :accessor playermisc.age    :initform 0)
   (status :accessor playermisc.status :initform 0)
   (height :accessor playermisc.height :initform 0)
   (weight :accessor playermisc.weight :initform 0))
  (:documentation "A helper-class for the player-object."))


(defclass player-abilities ()
  ((armour-rating   :accessor get-armour-rating
		    :initform 0
		    :documentation "integer, >= 0")
   (armour-modifier :accessor get-armour-modifier
		    :initform 0
		    :documentation "integer")
   (tohit-modifier  :accessor get-tohit-modifier
		    :initform 0
		    :documentation "integer")
   (damage-modifier :accessor get-damage-modifier
		    :initform 0
		    :documentation "integer"))
  
  (:documentation "A helper-class for the player-object."))

(defclass character-stat ()
  ((symbol        :accessor stat.symbol
		  :initarg :symbol)
   (name          :accessor stat.name
		  :initform ""
		  :initarg :name)
   (abbreviation  :accessor stat.abbreviation
		  :initform ""
		  :initarg :abbreviation)
   (positive-desc :accessor stat.positive-desc
		  :initform ""
		  :initarg :positive-desc)
   (negative-desc :accessor stat.negative-desc
		  :initform ""
		  :initarg :negative-desc)
   (number        :accessor stat.number
		  :initarg :number)
   (bit-flag      :accessor stat.bit-flag
		  :initarg :bit-flag)
   (fields        :accessor stat.fields
		  :initform nil)
   (data          :accessor stat.data
		  :initform nil
		  :initarg :data)
   ))

;; this is a hack!
(defstruct (stat-field (:copier nil))
  lower
  upper
  data)

;; might be removed later for optimisation reasons
(defclass creature (activatable)
  ((current-hp  :accessor current-hp
		:initform 0)
   (maximum-hp  :accessor maximum-hp
		:initform 0)
   (loc-x   :accessor location-x
	    :initform nil) ;;+illegal-loc-x+)
   (loc-y   :accessor location-y
	    :initform nil) ;;+illegal-loc-y+)

   (display-x  :accessor display-x
	       :initform nil) ;;+illegal-loc-x+)
   
   (display-y  :accessor display-y
	       :initform nil) ;;+illegal-loc-y+)
   
   (x-offset :accessor x-offset
	     :initform 0
	     :documentation "pixel offset on x-axis when doing actual painting.")
   
   (y-offset :accessor y-offset
	     :initform 0
	     :documentation "pixel offset on y-axis when doing actual painting.")

   (alive?  :accessor creature-alive?
	    :initform t)

   (gfx-sym ;;:accessor gfx-sym
	    :initform nil ;; (text-paint-value +term-white+ #\X)
	    :documentation "The gfx symbol for the player.")
   
   (text-sym ;;:accessor text-sym
	     :initform nil ;;(text-paint-value +term-white+ #\X)
	     :documentation "The gfx symbol for the player.")

   
   ))


(defclass player (creature)
  (
    ;; === Need Special saving ===
  
   (name   :accessor player.name   :initform nil :documentation "The name of the player.")
   (class  :accessor player.class  :initform nil :documentation "The character-class of the player.")
   (race   :accessor player.race   :initform nil :documentation "Pointer to the player-race.")
   (gender :accessor player.gender :initform nil :documentation "Pointer to the player-gender.")
   
   (base-stats    :accessor player.base-stats
		  :initform nil
		  :documentation "An array with the base stats")
   (current-statmods :accessor player.cur-statmods
		     :initform nil
		     :documentation "An array with the diff (possibly drained or raised values) of the base stats")

   (hp-table      :accessor player.hp-table
		  :initform nil
		  :documentation "An array of hitpoints gained each character-level. Note: should be saved.")
   
   (equipment     :accessor player.equipment
		  :initform nil
		  :documentation "A pointer to the items worn.")
   
   ;; add save-code for this as well
   (misc          :accessor player.misc
		  :initform nil
		  :documentation "An object with misc info about the player character.")

   
   (dead-from     :accessor player.dead-from
		  :initform ""
		  :documentation "Who killed the player?")

   (monster-knowledge :accessor player.monster-knowledge
		      :initform (make-hash-table :test #'equal)
		      :documentation "Knowledge about monsters.")
   
   (object-knowledge :accessor player.object-knowledge
		     :initform (make-hash-table :test #'equal)
		     :documentation "Knowledge about objects.")
   
   
   ;; === Directly savable to binary ===
   
   (view-x :accessor player.view-x :initform +illegal-loc-x+);; wx
   (view-y :accessor player.view-y :initform +illegal-loc-y+);; wy
   
   (depth       :accessor player.depth     :initform 0)
   (max-depth   :accessor player.max-depth :initform 0)
   
   (maximum-xp  :accessor player.maximum-xp  :initform 0)
   (current-xp  :accessor player.current-xp  :initform 0)
   (fraction-xp :accessor player.fraction-xp :initform 0) 
   
   ;; current-hp is in creature
   (fraction-hp :accessor player.fraction-hp :initform 0)
   
   (current-mana  :accessor current-mana         :initform 0)
   (fraction-mana :accessor player.fraction-mana :initform 0)
   
   (gold        :accessor player.gold   :initform 0)
   (satiation   :accessor player.satiation   :initform (1- +food-full+))
   (energy      :accessor player.energy :initform 0)
   
   ;; === The remaining values can be calculated from the above ===
   
   (power-lvl :accessor player.power-lvl
	      :initform 1
	      :documentation "can be calculated from cur-xp")
   (max-level :accessor player.max-level
	      :initform 1
	      :documentation "can be calculated from max-xp")

   (max-mana  :accessor maximum-mana
	      :initform 0
	      :documentation "can be calculated")
   (xp-table  :accessor player.xp-table
	      :initform nil
	      :documentation "can be calculated")
   
   (energy-use :accessor player.energy-use
	       :initform 0
	       :documentation "is just a temp-variable")
   (leaving?   :accessor player.leaving?
	       :initform nil) ;; need to save it?
   (speed      :accessor player.speed
	       :initform +speed-base+)  ;; does this change?
   

   (burden       :accessor player.burden       :initform 0
		 :documentation "Calculated value for how much player carries.")
   (light-radius :accessor get-light-radius :initform 0)

   ;; need infravision and see-inv here, because these are accessed in inner loops
   ;; and should be fast.  the engine also uses them.  
   (infravision :accessor player.infravision
		:initform 0
		:documentation "How far does infravision reach?  0 for no infravision.")
   (see-invisible :accessor player.see-invisible
		  :initform 0
		  :documentation "How far does 'see invisible' reach?  0 for no see-inv.")
   (inventory   :accessor get-creature-inventory
		:initform nil
		:documentation "quick variable to equipment.backpack.content")
   
   (skills      :accessor player.skills
		:initform nil
		:documentation "Meaning depends entirely on variant, engine will not touch this.")
   
   (modbase-stats :accessor player.modbase-stats
		  :initform nil
		  :documentation "This is the modified base stats (base + race + class + eq)")
   (active-stats :accessor player.active-stats
		 :initform nil
		 :documentation "This is the current active stat-value, it's
value is calculated by: (base + curstatmods + race + class + eq)")

   (perceived-abilities :accessor player.perceived-abilities
			:initform nil
			:documentation "A player-abilities object with perceived abilties.")
   (actual-abilities :accessor player.actual-abilities
		     :initform nil
		     :documentation "A player-abilities object with actual abilties.")
   

   ;; an array with index for each element, each element is an integer which tells the power of the resist
   (resistance :accessor get-resistance-table
	       :initform nil
	       :documentation "What does the player resist and how much?
object is an array with index for each element, each element is an integer which tells the power of the resist")

   (stat-sustains :accessor get-stat-sustains
		  :initform 0
		  :documentation "bit-flag based on registered stats.")
   
   ;; should not be touched by engine
   (calculated-attributes :accessor player.calc-attrs
			  :initform nil
			  :documentation "Should be a hash-table with calculated attributes.")
   (temp-attributes :accessor player.temp-attrs
		    :initform nil
		    :documentation "Should be a hash-table with temporary attributes.")

   (target :accessor player.target
	   :initform nil
	   :documentation "Who/what is the player targeting?")
   
   ))

(defclass attack-type ()
  ((key        :accessor attack-type.key
	       :initarg :key
	       :initform nil)
   (power      :accessor attack-type.power
	       :initarg :power
	       :initform 0)
   (hit-effect :accessor attack-type.hit-effect
	       :initarg :hit-effect
	       :initform nil))
  (:documentation "Represents a type of attack, typically by a monster."))

(defclass attack ()
  ((kind     :accessor attack.kind
	     :initarg :kind
	     :initform nil)
   (dmg-type :accessor attack.dmg-type
	     :initarg :dmg-type
	     :initform nil)
   (damage   :accessor attack.damage
	     :initarg :damage
	     :initform nil))
  (:documentation "Representation for a monster-attack."))
   

(defclass active-object (activatable)
  ((kind        :accessor aobj.kind
		:initarg :kind
		:initform nil)
   (inscription :accessor aobj.inscr
		:initform "")
   (number      :accessor aobj.number
		:initarg :number
		:initform 1)
   (contains    :accessor aobj.contains
		:initform nil)
   (events      :accessor aobj.events
		:initform nil)

   (identify    :accessor aobj.identify
		:initform 0
		:documentation "Bitfield that says how known the object is, see the +ident-*+ flags.")

   (sanctity    :accessor get-sanctity
		:initform 0
		:documentation "See +sanctity-*+ flags. 0 is normal.")
   
   (marked :accessor aobj.marked
	   :initform nil
	   :documentation "boolean whether the object has been marked.")

   (speed-modifier  :accessor aobj.speed-modifier
		    :initform 0)
   (armour-rating   :accessor get-armour-rating
		    :initform 0)
   (armour-modifier :accessor get-armour-modifier
		    :initform 0)
   (damage-dice     :accessor get-damage-dice
		    :initform 0)
   (number-of-damage-dice :accessor get-number-of-damage-dice
			  :initform 0)
   (tohit-modifier  :accessor get-tohit-modifier
		    :initform 0)
   (damage-modifier :accessor get-damage-modifier
		    :initform 0)

   (loc-x       :accessor location-x
		:initform +illegal-loc-x+)
   (loc-y       :accessor location-y
		:initform +illegal-loc-y+)
   (display-x  :accessor display-x
	       :initform nil) ;;+illegal-loc-x+)
   
   (display-y  :accessor display-y
	       :initform nil) ;;+illegal-loc-y+)
   
   (x-offset :accessor x-offset
	     :initform 0
	     :documentation "pixel offset on x-axis when doing actual painting.")
   
   (y-offset :accessor y-offset
	     :initform 0
	     :documentation "pixel offset on y-axis when doing actual painting.")
   
   (gfx-sym  :initform nil)
   (text-sym  :initform nil)
   ))



(defclass active-monster (creature)
  ((kind    :accessor amon.kind
	    :initarg :kind
	    :initform nil)
   (speed   :accessor get-creature-speed
	    :initform 0)
   (energy  :accessor get-creature-energy
	    :initform 0)
   (mana    :accessor get-creature-mana
	    :initform 0)
   (seen    :accessor amon.seen-by-player?
	    :initform nil)
   (distance :accessor amon.distance
	     :documentation "Distance from monster to player."
	     :initform 666)
   (vis-flag :accessor amon.vis-flag
	     :initform 0) ;; visibility flag
   

   (temp-attributes :accessor amon.temp-attrs
		    :initform (make-hash-table :test #'eq)
		    :documentation "Should be a hash-table with temporary attributes.")

   (strategies :accessor amon.strategies
	       :initform '()
	       :documentation "An ordered list of strategies the monster can choose.")
   
   (tactics    :accessor amon.tactics
	       :initform '()
	       :documentation "An unordered list of tactics the monster knows and can use.")
   (inventory  :accessor get-creature-inventory
	       :initform nil
	       :documentation "A pointer to an item-container.")
   ))



(defclass old-player-info ()
  ((stats       :accessor old.stats       :initform nil)
   (abilities   :accessor old.abilities   :initform nil)
   (see-inv     :accessor old.see-inv     :initform 0)
   (speed       :accessor old.speed       :initform 0)
   ;; move to variant later?
   (heavy-weapon :accessor old.heavy-weapon :initform nil)
   (heavy-bow    :accessor old.heavy-bow    :initform nil)
   (icky-weapon  :accessor old.icky-weapon  :initform nil)
   (telepathy    :accessor old.telepathy    :initform nil)
   )
  (:documentation "A class-object to fill with values
during early update of a player's bonuses.  It will
later be sent to the check after the player-update
to see what other parts of the system must be altered.
It's meant to be extended by variants."))
   

(defclass treasure-drop ()
  ((chance  :initarg :chance
	    :initform 1
	    :accessor drop.chance)
   
   (amount  :initarg :amount
	    :initform 1
	    :accessor drop.amount
	    :documentation "either positive integer or (cons int int)")
   (quality :initarg :quality
	    :initform :normal
	    :accessor drop.quality)
   (type    :initarg :type
	    :initform :any
	    :accessor drop.type)
   ))

(bt:define-binary-struct (hs-entry (:conc-name hs-entry.)
				   (:copier nil)
				   (:predicate nil)) ()
    
    (version nil) ;; string
    (variant nil) ;; string
    
    (name nil)    ;; string
    (race nil)    ;; string-id
    (class nil)   ;; string-id
    (gender nil)     ;; string
    (cause-of-death nil) ;; string
    
    ;; directly savable
    (xp          0 :bt bt:u32)
    (max-xp      0 :bt bt:u32)
    (level       0 :bt bt:u16)
    (depth       0 :bt bt:u16)
    (max-depth   0 :bt bt:u16)
    (turn        0 :bt bt:u32)
    (gold        0 :bt bt:u32)
    (score       0 :bt bt:u32)  
    
    (date        0 :bt u64) ;; time of death
    )

;; for each recorded effect for an object there should be such an entry
(defstruct (effect-entry (:copier nil))
  type
  fun
  energy-use)


;; check the initform values here more thoroughly
(defclass monster-kind ()
  ((id        :initarg :id
	      :accessor get-id
	      :initform ""
	      :documentation "Should be a legal (string) id.")
   (numeric-id :accessor monster.numeric-id ;; possibly remove later
	       :initarg :numeric-id
	       :initform nil)
   (name      :initarg :name
	      :accessor monster.name
	      :initform "")
   ;; possibly move to unique monster
   (title     :initarg :title
	      :accessor monster.title
	      :initform nil
	      :documentation "Some monsters have fancy titles.")
   (desc      :accessor monster.desc      :initform "") ;; string 
   
   (gfx-sym :accessor gfx-sym
	    :initform (text-paint-value +term-red+ #\M)
	    :documentation "The gfx symbol for the monster.")
   
   (text-sym :accessor text-sym
	     :initform (text-paint-value +term-red+ #\M)
	     :documentation "The textual symbol for the monster.")

   (alignment :accessor monster.alignment :initform nil) ;; symbols/list
   (type      :accessor monster.type      :initform nil) ;; symbols/list
   (locations :accessor alloc-locations   :initform '()) ;; list of conses (depth . rarity)
   (hitpoints :accessor monster.hitpoints :initform nil) ;; cons or a number I guess
   (armour    :accessor monster.armour    :initform nil) ;; integer
   (power-lvl :accessor monster.power-lvl :initform 1) ;; positive integer
   (speed     :accessor monster.speed     :initform 0) ;; positive integer
   (xp        :accessor monster.xp        :initform 0) ;; positive integer
   (gender    :accessor monster.gender    :initform nil) ;; symbol? 

   (alertness  :accessor monster.alertness  :initform 0) ;; how sleepy
   (vision     :accessor monster.vision     :initform 0) ;; how far can it see?
   (attacks    :accessor monster.attacks    :initform '()) ;; a list
   (treasures  :accessor monster.treasures  :initform '()) ;; a list

   (resists           :accessor get-resists         :initform 0) ;; bit field
   (immunities        :accessor get-immunities      :initform 0) ;; bit field
   (vulnerabilities   :accessor get-vulnerabilities :initform 0) ;; bit field

   (abilities         :accessor monster.abilities    :initform '())
   ;; format seems to be (frequency spab spab ...)
   (special-abilities :accessor monster.sp-abilities :initform '())
   (strategies        :accessor monster.strategies   :initform '())

   ;; fix later
   (in-group :accessor monster.in-group :initform nil)
   )) 

(defclass unique-monster (monster-kind)
  ((already-dead :initarg :already-dead :accessor monster.already-dead :initform nil))
  (:documentation "A unique monster has this class."))


(defclass object-kind ()
    ((id         :accessor get-id
		 :initarg :id
		 :initform nil)
   
     (numeric-id :accessor object.numeric-id
		 :initarg :numeric-id
		 :initform nil)
   
     (name       :accessor object.name
		 :initarg :name
		 :initform nil)
   
     (gfx-sym    :accessor gfx-sym
		 :initform 0
		 :documentation "A precoded 24-bit bitfield specifying which graphical symbol to use.")
     
     (text-sym   :accessor text-sym
		 :initform 0
		 :documentation "A precoded 24-bit bitfield specifying whichtextual symbol to use.")

     (power-lvl  :accessor object.power-lvl
		 :initform 0
		 :documentation "A non-negative integer denoting how powerful the object is.")
     
     (locations  :accessor alloc-locations
		 :initform '()
		 :documentation "A list of conses on the form (depth . chance)")
   
     (weight     :accessor object.weight
		 :initform 0
		 :documentation "Non-negative integer, each about 50g.")
   
     (cost       :accessor object.cost
		 :initform 0
		 :documentation "Non-negative integer, denoting one unit of the currency.")

     (flags      :accessor object.flags
		 :initform '()
		 :documentation "List of symbols, may be the empry list.")
   
     (easy-know   :accessor object.easy-know
		  :initform nil ;; boolean
		  :documentation "Is it easy to understand the use of the object?")
     
     (aware :accessor object.aware
	    :initform nil ;; boolean
	    :documentation "The player is 'aware' of the item's effects")
   
     (tried      :accessor object.tried
		 :initform nil
		 :documentation "The player has 'tried' one of the items")
   
     (flavour    :accessor object.flavour
		 :initform nil
		 :documentation "The flavour is either nil or a cons (desc . colour).")

     (sort-value :accessor object.sort-value
		 :initform 0
		 :documentation "Non-negative integer denoting where the object will be in an object-list.")
   
     (events     :accessor object.events
		 :initform nil
		 :documentation "should be a list of conses (event . function-obj)")

     (effects   :accessor object.effects
		:initform nil
		:documentation "Is a list of effect-entry objects.")

     (the-kind  :accessor object.the-kind
		:initarg :the-kind
		:initform nil)

     (text-colour :accessor object.text-colour
		  :initform +term-l-blue+
		  :documentation "Colour used in textual descriptions of the object.")

     ;; == moved over from game-values
     ;; refers to vulnerabilities of the item
     (vulnerabilities :accessor get-vulnerabilities
		      :initform 0
		      :documentation "The value is tied to registered elements.")
     (stat-sustains :accessor get-stat-sustains
		    :initform 0
		    :documentation "The value is tied to registered stats.")
     (stat-modifiers :accessor get-stat-modifiers
		     :initform '())

     (light-radius  :accessor get-light-radius
		    :initform 0
		    :documentation "Value is non-negative integer.")
     (speed-modifier :accessor object.speed-modifier
		     :initform 0)
     (abilities     :accessor object.abilities
		    :initform '())
     ;; refers to immunities it confers to user
     (immunities    :accessor get-immunities
		    :initform 0
		    :documentation "The value is tied to registered elements.")
     ;; refer to an item's immunities to elements (not conferred)
     (ignores       :accessor get-ignores
		    :initform 0
		    :documentation "The value is tied to registered elements.")
     ;; refers to both what the item resists and what it confers
     (resists       :accessor get-resists
		    :initform 0
		    :documentation "The value is tied to registered elements.")
     (armour-rating   :accessor get-armour-rating
		      :initform 0)
     (armour-modifier :accessor get-armour-modifier
		      :initform 0)
     (damage-dice     :accessor get-damage-dice
		      :initform 0)
     (number-of-damage-dice :accessor get-number-of-damage-dice
			    :initform 0)
     (tohit-modifier  :accessor get-tohit-modifier
		      :initform 0)
     (damage-modifier :accessor get-damage-modifier
		      :initform 0)
     ))


(defclass character-race ()
  ((id            :accessor get-id
		  :initarg :id
		  :initform "")
   (symbol        :accessor race.symbol
		  :initarg :symbol
		  :initform nil)
   (name          :accessor race.name
		  :initarg :name
		  :initform "unknown")
   (desc          :accessor race.desc
		  :initarg :desc
		  :initform "not described")
   (base-age      :initform 20
		  :accessor race.base-age
		  :documentation "An integer specifying base starting age for a player of this race.")
   (mod-age       :initform 0
		  :accessor race.mod-age
		  :documentation "A flexible object modifying starting age for a player.")
   (base-status   :initform 0
		  :accessor race.base-status
		  :documentation "An integer specifying base starting status for a player of this race.")
   (mod-status    :initform 0
		  :accessor race.mod-status
		  :documentation "A flexible object modifying starting status for a player.")
   
   (m-height      :initform 170
		  :documentation "Base height for males of the race.")
   (m-height-mod  :initform 15
		  :documentation "Normalised difference in height for males.")
   (f-height      :initform 160
		  :documentation "Base height for females of the race.")
   (f-height-mod  :initform 15
		  :documentation "Normalised difference in height for females.")
   (m-weight      :initform 80
		  :documentation "Base weight for males of the race.")
   (m-weight-mod  :initform 20
		  :documentation "Normalised difference in weight for males.")
   (f-weight      :initform 68
		  :documentation "Base height for females of the race.")
   (f-weight-mod  :initform 15
		  :documentation "Normalised difference in height for females.")
   
   (xp-extra      :accessor race.xp-extra      :initform 0)
   (hit-dice      :accessor race.hit-dice      :initform 10)
   (stat-changes  :accessor race.stat-changes  :initform '())
   (stat-sustains :accessor get-stat-sustains
		  :initform 0
		  :documentation "bitfield based on legal stats.") 
   (abilities     :accessor race.abilities     :initform '()) ;; split in two?
   (resists       :accessor get-resists       :initform 0
		  :documentation "Integer with bit-flags, not array.")
   (immunities    :accessor get-immunities
		  :initform 0
		  :documentation "Integer with bit-flags, not array.")
   (vulnerabilities :accessor get-vulnerabilities
		    :initform 0
		    :documentation "Integer with bit-flags, not array.") 
   (classes       :accessor race.classes       :initform '())
   (start-eq      :accessor race.start-eq      :initform '())
   (skills        :accessor race.skills        :initform '()))
  (:documentation "Representation for a character race."))


(defclass character-class ()
  ((id            :accessor get-id
		  :initarg :id
		  :initform nil)
   (symbol        :accessor class.symbol
		  :initarg :symbol
		  :initform nil)
   (name          :accessor class.name
		  :initarg :name
		  :initform nil)
   (desc          :accessor class.desc
		  :initarg desc
		  :initform nil)
   (mod-age       :initform 0
		  :accessor class.mod-age
		  :documentation "A flexible object modifying starting age for a player.")
   (mod-status    :initform 0
		  :accessor class.mod-status
		  :documentation "A flexible object modifying starting status for a player.")
   (hit-dice      :accessor class.hit-dice      :initform 0)
   (xp-extra      :accessor class.xp-extra      :initform 0)
   (stat-changes  :accessor class.stat-changes  :initform nil)
   (stat-sustains :accessor get-stat-sustains
		  :initform 0
		  :documentation "an integer with bit-flags.") 
   (resists       :accessor get-resists       :initform 0
		  :documentation "Integer with bit-flags, not array.")
   (immunities    :accessor get-immunities
		  :initform 0
		  :documentation "Integer with bit-flags, not array.")
   (vulnerabilities :accessor get-vulnerabilities
		    :initform 0
		    :documentation "Integer with bit-flags, not array.") 
   (abilities     :accessor class.abilities     :initform '())
   (titles        :accessor class.titles        :initform nil)
   (starting-eq   :accessor class.start-eq      :initform nil)
   (skills        :accessor class.skills        :initform nil))
  (:documentation "Information about a character class."))


(defclass gender ()
  ((id        :accessor get-id
	      :initform nil
	      :initarg :id) ;; saves of players should use id, not symbol
   (symbol    :accessor gender.symbol
	      :initform nil
	      :initarg :symbol)
   (name      :accessor gender.name
	      :initform "Freak"
	      :initarg :name)
   (win-title :accessor gender.win-title
	      :initform "Winner"
	      :initarg :win-title)
   ))


(defclass floor-type ()
  ((id         :accessor floor.id         :initform nil :initarg :id)
   (name       :accessor floor.name       :initform nil :initarg :name)
   (numeric-id :accessor floor.numeric-id :initform -1  :initarg :numeric-id)
   
   (gfx-sym    :accessor gfx-sym       :initform 0   :initarg :gfx-sym)
   (text-sym   :accessor text-sym      :initform 0   :initarg :text-sym)
   (mimic      :accessor floor.mimic   :initform nil :initarg :mimic)
   (flags      :accessor floor.flags   :initform 0   :initarg :flags)
   ))

(defclass decor ()
  ((id        :accessor decor.id       :initform nil :initarg :id)
   (name      :accessor decor.name     :initform nil :initarg :name)
   (type      :accessor decor.type     :initform nil :initarg :type)
   (visible?  :accessor decor.visible? :initform t   :initarg :visible?)
   (loc-x     :accessor location-x
	      :initarg :loc-x
	      :initform nil)
   (loc-y     :accessor location-y
	      :initarg :loc-y
	      :initform nil)
   (events     :accessor decor.events
	       :initarg :events
	       :initform nil
	       :documentation "should be a list of conses (event . function-obj)")

   ))

(defclass room-type ()
  ((id        :accessor room-type.id
	      :initarg :id
	      :initform nil)
   
   (type      :accessor room-type.type
	      :initarg :type
	      :initform 'room-type)
     
   (name      :accessor room-type.name
	      :initarg :name
	      :initform "room")

   (parent  :accessor room-type.parent
	    :initarg :parent
	    :initform 'room-type)
   
   (constructor :accessor room-type.constructor
		:initarg :constructor
		:initform nil)

   (builder :accessor room-type.builder
	    :initarg :builder
	    :initform nil)
     
   (size-mod  :accessor room-type.size-mod
	      :initarg :size-mod
	      :initform #1A(0 0 0 0 0))
     
   (min-level :accessor room-type.min-level
	      :initarg :min-level
	      :initform 1)
   ))

(defclass active-room (activatable)
  ((type      :accessor room.type
	      :initarg :type
	      :initform nil)
   (loc-x     :accessor location-x
	      :initarg :loc-x
	      :initform +illegal-loc-x+)
   (loc-y     :accessor location-y
	      :initarg :loc-y
	      :initform +illegal-loc-y+)
   ))


(defclass level (activatable)
  ((id      :accessor level.id      :initarg :id      :initform "level")
   (symbol  :accessor level.symbol  :initarg :symbol  :initform 'level)
   (dungeon :accessor level.dungeon :initarg :dungeon :initform nil)
   (rating  :accessor level.rating  :initarg :rating  :initform 0)
   (depth   :accessor level.depth   :initarg :depth   :initform 0))
  (:documentation "A representation of a level.  Meant to be subclassed."))


(defclass random-level (level)
  ((id     :initform "random-level")
   (symbol :initform 'random-level)))
   


(defclass themed-level (level)
  ((id :initform "themed-level")
   (symbol :initform 'themed-level)))


(defclass l-event ()
  ((id            :reader event.id
		  :initform nil
		  :initarg :id
		  :documentation "A string id for the event that can be saved I think.")
   
   (type          :reader event.type
		  :initform nil
		  :initarg :type
		  :documentation "correspond to EVENT-TYPES")
   
   ;; the function when called should return T when ok and NIL when not ok
   (function      :reader event.function
		  :initform nil
		  :initarg :function
		  :documentation "the function/funcallable object when called should return T when ok and NIL when not ok")
   
   (state         :reader event.state
		  :initform nil
		  :initarg :state)
   
   (return-action :reader event.return
		  :initform :remove-event
		  :initarg :return)
   ))

(defclass trap-type ()
  ((id        :accessor trap.id
	      :initform ""
	      :initarg :id
	      :documentation "string-id")
   (name      :accessor trap.name
	      :initform ""
	      :initarg :name
	      :documentation "displayable name")
   (gfx-sym   :accessor gfx-sym
	      :initform 0
	      :documentation "Graphical symbol for this trap.")

   (text-sym  :accessor text-sym
	      :initform 0
	      :documentation "Textual symbol for this trap.")

   (effect    :accessor trap.effect
	      :initform nil
	      :initarg :effect
	      :documentation "a funcallable object")
   (min-depth :accessor trap.min-depth
	      :initform 0
	      :initarg :min-depth
	      :documentation "minimum depth it can be generated at")
   (max-depth :accessor trap.max-depth
	      :initform nil
	      :initarg :max-depth
	      :documentation "maximum depth it can be generated at")
   (rarity    :accessor trap.rarity
	      :initform 1
	      :initarg :rarity
	      :documentation "the rarity of the trap")))

(defclass door-type ()
  ((id        :accessor door.id
	      :initform ""
	      :initarg :id
	      :documentation "string-id")
   (name      :accessor door.name
	      :initform ""
	      :initarg :name
	      :documentation "displayable name")
   (gfx-sym   :accessor gfx-sym
	      :initform 0
	      :documentation "Graphical symbol for this door.")
   (text-sym  :accessor text-sym
	      :initform 0
	      :documentation "Textual symbol for this door.")

   ;; hackish
   (cave-flags-on :initform 0
		  :documentation "Flags to turn on in a cave when decor is on.")
   (cave-flags-off :initform 0
		   :documentation "Flags to turn on in a cave when decor is on.")
   
   ))


(defclass active-trap (decor)
  ((visible? :initform nil))) ;; by default not visible initially

(defclass active-door (decor)
  ((visible?  :initform nil) ;; by default not secret
   (lock      :accessor door.lock    :initform 0) ;; by default no lock on the door
   (stuck     :accessor door.stuck   :initform 0) ;; by default not stuck
   (broken?   :accessor is-broken?   :initform nil) ;; by default not broken
   (closed?   :accessor door.closed? :initform nil) ;; by default it's open
   ))


;;; === Equipment-classes

(defclass item-table ()
  ((cur-size :accessor items.cur-size :initarg :cur-size :initform 0))
  (:documentation "abstract interface for all item-tables."))

(defclass items-on-floor (item-table)
  ((obj-list :accessor items.objs
	     :initform nil)
   (dungeon  :accessor items.dungeon
	     :initarg :dungeon
	     :initform nil)
   (loc-x    :accessor location-x
	     :initarg :loc-x
	     :initform +illegal-loc-x+);; invalid value
   (loc-y    :accessor location-y
	     :initarg :loc-y
	     :initform +illegal-loc-y+))
    
  (:documentation "Represents the items on the floor."))

(defclass items-in-container (item-table)
  ((obj-arr  :accessor items.objs     :initarg :objs     :initform nil)
   (max-size :accessor items.max-size :initarg :max-size :initform 5))
  (:documentation "A container for other objects, ie a backpack."))

(defclass items-worn (item-table)
  ((obj-arr       :accessor items.objs     :initarg :objs     :initform nil))
  (:documentation "What is worn."))  

(defclass items-in-house (items-in-container)
  ((max-size :initform +store-item-limit+))
  (:documentation "What is in a house."))
  
(defclass items-in-store (items-in-house)
  ()
  (:documentation "What is in a store."))

;;; End equipment-classes

;;; Stuff related to buildings
(defclass house (activatable)
  ((id     :accessor house.id     :initform nil :initarg :id)
   (name   :accessor house.name   :initform nil :initarg :name)
   
   (owner  :accessor house.owner :initform nil :initarg :owner)
   ;; the current items
   (items  :accessor house.items :initform nil :initarg :items)
   ))

(defclass store (house)
  ((id     :accessor store.id     :initform nil :initarg :id)
   (name   :accessor store.name   :initform nil :initarg :name)
   (number :accessor store.number :initform nil :initarg :number)
     
   (sells        :accessor store.sells        :initform nil)
   (will-buy     :accessor store.will-buy     :initform nil)
   (turnover     :accessor store.turnover     :initform +store-turnover+)
   (min-items    :accessor store.min-items    :initform +store-minimum-items+)
   (max-items    :accessor store.max-items    :initform +store-maximum-items+)
   (item-limit   :accessor store.item-limit   :initform +store-item-limit+)
   (object-depth :accessor store.object-depth :initform 5   :initarg :object-depth)

   (possible-owners :accessor store.possible-owners
		    :initform nil
		    :initarg :possible-owners)


   ;; the dynamic data-part
   (items        :accessor store.items        :initform nil :initarg :items)

   ))

(defclass owner ()
  ((id         :accessor owner.id         :initform nil :initarg :id)
   (name       :accessor owner.name       :initform nil :initarg :name)))
 
(defclass store-owner (owner)
  ((purse      :accessor owner.purse      :initform nil :initarg :purse)
   (max-greed  :accessor owner.max-greed  :initform nil :initarg :max-greed)
   (min-greed  :accessor owner.min-greed  :initform nil :initarg :min-greed)
   (haggle-num :accessor owner.haggle-num :initform nil :initarg :haggle-num)
   (tolerance  :accessor owner.tolerance  :initform nil :initarg :tolerance)
   (race       :accessor owner.race       :initform nil :initarg :race)
   (picture    :accessor owner.picture    :initform nil :initarg :picture)
   ))


;;; end buildings

;;; Stream-wrappers

(defclass l-readable-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))
(defclass l-binary-stream ()
  ((the-stream :accessor lang.stream :initform nil :initarg :stream)))

;;; end stream-wrappers

(defclass flavour ()
  ((name      :accessor flavour.name
	      :initarg :name
	      :initform nil)
   (gfx-sym   :accessor gfx-sym
	      :initarg :gfx-sym
	      :initform 0) ;; number
   (text-sym  :accessor text-sym
	      :initarg :text-sym
	      :initform 0) ;; number
   ))


(defclass flavour-type ()
  ((symbol    :accessor flavour-type.symbol
	      :initarg :symbol
	      :initform nil)
   (gfx-sym   :accessor flavour-type.gfx-sym
	      :initarg :gfx-sym
	      :initform 0)
   (text-sym  :accessor flavour-type.text-sym
	      :initarg :text-sym
	      :initform 0)
   (table     :accessor flavour-type.table
	      :initarg :table
	      :initform (make-hash-table :test #'equal)) ;; this will be changed to a vector later
   (unused-flavours  :accessor flavour-type.unused-flavours
		     :initarg :unused-flavours
		     :initform '())
   (generator-fn :accessor flavour-type.generator-fn
		 :initarg :generator-fn
		 :initform nil) ;; generator-function should return a cons
  ))

(defclass object-knowledge ()
  ((id :accessor get-id
       :initarg :id
       :documentation "Id for object we know something about."
       :initform nil)
   
   (flags :accessor object.flags
	  :initarg :flags
	  :documentation "Flags for object we know something about."
	  :initform 0)
   ))

(defclass monster-knowledge ()
  ((id :accessor get-id
       :initarg :id
       :documentation "Id for monster we know something about."
       :initform nil)
   
   (flags :accessor monster.flags
	  :initarg :flags
	  :documentation "Flags the monster has."
	  :initform '())
   
   (num-killed :accessor monster.num-killed
	       :initarg :killed
	       :initform 0
	       :documentation "How many have you killed?")

   ;;; element related info
   (resists :accessor get-resists
	    :initform 0
	    :documentation "bitfield as everything else.")
   
   (vulnerabilities :accessor get-vulnerabilities
		    :initform 0
		    :documentation "bitfield as everything else.")

   (immunities :accessor get-immunities
	       :initform 0
	       :documentation "bitfield as everything else.")

   (tried-elm :accessor monster.tried-elm
	      :initform 0
	      :documentation "which elemental attacks have been tried on monster.
bitfield as everything else.")
   
   ))

(defclass ui-theme ()
  ((key :accessor theme.key
	:initarg :key
	:documentation "string key for the theme."
	:initform "")
   (font :accessor theme.font
	 :initarg :font
	 :documentation "The default font to use in the font, when no other font is specified."
	 :initform nil)
   (system :accessor theme.system
	   :initarg :system
	   :documentation "Which system should the theme be used on (a string)."
	   :initform nil)

   (windows :accessor theme.windows
	    :initarg :windows
	    :documentation "A list of subwindows handled by the theme."
	    :initform '())
   
   ))


(defclass visual-projectile ()
  
  ((id             :initform nil
		   :accessor projectile.id
		   :initarg :id)
   (gfx-path       :initform nil
	           :accessor projectile.gfx-path)
   (text-path      :initform nil
	           :accessor projectile.text-path)
   (gfx-impact     :initform 0
	           :accessor projectile.gfx-impact)
   (text-impact    :initform 0
		   :accessor projectile.text-impact)
   (gfx-explosion  :initform 0
		   :accessor projectile.gfx-explosion)
   (text-explosion :initform 0
		   :accessor projectile.text-explosion)
   (gfx-beam       :initform 0
		   :accessor projectile.gfx-beam
		   :documentation "gfx code for beam.")
   (text-beam      :initform 0
		   :accessor projectile.text-beam
		   :documentation "text code for beam."))
  
  (:documentation "Class to keep information about visualisation of projectiles."))

(defclass visual-state ()
  ((key      :accessor visual-state.key      :initform nil)
   (desc     :accessor visual-state.desc     :initform nil)
   (priority :accessor visual-state.priority :initform 10)
   (active   :accessor visual-state.active   :initform nil)
   (gfx-sym  :accessor gfx-sym               :initform 0)
   ))

(defclass window ()
  ((id         :accessor window.id
	       :initarg :id
	       :initform "window"
	       :documentation "A string id for the window.")
   
   (num-id     :accessor window.num-id
	       :initarg :num-id
	       :initform -1
	       :documentation "The numeric id, or index for the window, can be used for array lookups.")
   
   (name        :accessor window.name
		:initarg :name
		:documentation "The name/var for this window. Should be a string"
		:initform nil)
   
   (x-offset :accessor window.x-offset
	     :initform 0
	     :documentation "The x-offset in pixels on the underlying display area.")
   
   (y-offset :accessor window.y-offset
	     :initform 0
	     :documentation "The y-offset in pixels on the underlying display area.")
   
   (height     :accessor window.height
	       :initarg :height
	       :initform -1
	       :documentation "Height of window in tiles. Rows.")
   
   (width      :accessor window.width
	       :initarg :width
	       :initform -1
	       :documentation "Width of window in tiles. Columns.")
   
   (pixel-height :accessor window.pixel-height
		 :initform -1
		 :documentation "The max height of the window in pixels.")
   
   (pixel-width  :accessor window.pixel-width
		 :initform -1
		 :documentation "The max width of the window in pixels.")

   (tile-width  :accessor window.tile-width
		:initarg :tile-width
		:documentation "The width of an individual tile."
		:initform -1)

   (tile-height :accessor window.tile-height
		:initarg :tile-height
		:documentation "The height of an individual tile."
		:initform -1)


   
   (data       :accessor window.data
	       :initarg :data
	       :initform nil
	       :documentation "The actual window data, probably an x,y,z array.")
   
   (flagmap    :accessor window.flagmap
	       :initarg :flagmap
	       :initform nil
	       :documentation "An x,y map with flags for various tiles, e.g if it is updated.")
   
   (flags      :accessor window.flags
	       :initarg :flags
	       :initform 0
	       :documentation "Any bitflags needed to describe the window.")

   (repaint?   :accessor window.repaint? ;; slightly hackish, don't depend on it yet
	       :initarg :repaint?
	       :initform nil
	       :documentation "Does the window need a refresh/repaint?")

   
   (visible?   :accessor window.visible?
	       :initarg :visible?
	       :initform nil
	       :documentation "Is the window currently shown/visible?")

   (gfx-tiles?  :accessor window.gfx-tiles?
		:initarg :gfx-tiles?
		:documentation "Is the subwindow using graphical tiles or should output be ascii?"
		:initform nil)
   
   (font       :accessor window.font
	       :initarg :font
	       :initform nil
	       :documentation "Filename to the font.")
   
   (backgroundfile :accessor window.backgroundfile
		   :initform nil
		   :documentation "Either NIL, or a filename for the background image.")

   (background :accessor window.background
	       :initform nil
	       :documentation "Either NIL, or an index to the background image.")

   (disabled?   :accessor window.disabled?
		:initarg :disabled?
		:initform nil
		:documentation "Is the window currently disabled?")

   
   ))

(defclass selectable-ui-object ()
  ((input-char :accessor selectable.char
	       :initarg :input-char
	       :initform nil
	       :documentation "what character may be used to select an alternative.")
   (mouse-topx :accessor selectable.topx
	       :initarg :topx
	       :initform 10000
	       :documentation "what is the top x coord in the mouse-selctable rectangle.")
   (mouse-topy :accessor selectable.topy
	       :initarg :topy
	       :initform 10000
	       :documentation "what is the top y coord in the mouse-selctable rectangle.")
   (mouse-botx :accessor selectable.botx
	       :initarg :botx
	       :initform -1
	       :documentation "what is the bottom x coord in the mouse-selectable rectangle.")
   (mouse-boty :accessor selectable.boty
	       :initarg :boty
	       :initform -1
	       :documentation "what is the bottom y coord in the mouse-selectable rectangle.")

   (highlighted? :accessor selectable.highlighted?
		 :initarg :highlighted?
		 :initform nil
		 :documentation "is the current selectable alternative highlighted?")
   
   (text       :accessor selectable.text
	       :initarg :text
	       :initform nil
	       :documentation "the text on the selectable object, NIL if no text.")

   (text-colour      :accessor selectable.text-colour
	             :initarg :text-colour
	             :initform +term-white+
	             :documentation "the text colour on the selectable object.")

   (text-colour-hi   :accessor selectable.text-colour-hi
		     :initarg :text-colour-hi
		     :initform +term-l-red+
		     :documentation "the text colour on the selectable object when hi-lighted.")
   
   (button-colour    :accessor selectable.button-colour
		     :initarg :button-colour
		     :initform :green
		     :documentation "the text button on the selectable object.")

   (button-colour-hi :accessor selectable.button-colour-hi
		     :initarg :button-colour-hi
		     :initform :red
		     :documentation "the button colour on the selectable object when hi-lighted.")

   
   (tile       :accessor selectable.tile
	       :initarg :tile
	       :initform nil
	       :documentation "the graphical tile on the selectable object, NIL if no graphical tile.")

   
   
   ))

;;; Other structs

(defstruct (game-obj-table (:conc-name gobj-table.)
			   (:predicate nil)
			   (:copier nil))
  (obj-table nil) ;; hash-table with all possible objects for the setting
  (alloc-table nil) 
  (obj-table-by-lvl nil))


(defstruct (help-topic (:conc-name help-topic.)
		       (:predicate nil)
		       (:copier nil))
  id
  key
  name
  data)


(defstruct (alloc-entry (:conc-name alloc.)
			(:predicate nil)
			(:copier nil))
  (obj nil)
  (index nil)
  (depth nil)
  (prob1 nil)
  (prob2 nil)
  (prob3 nil))

(defstruct (dun-data (:conc-name dun-data.)
		     (:predicate nil)
		     (:copier nil))
  (room-centres nil)
  (doors nil)
  (walls nil)
  (tunnels nil)
  (row-rooms nil)
  (col-rooms nil)
  (room-map nil)
  (crowded nil))


(defstruct (message (:conc-name message.)
		    (:predicate nil)
		    (:copier nil))
  (text nil)
  (attr nil)
  (noise :none)
  )

(defstruct (target (:conc-name target.)
		   (:copier nil))
  (obj nil)
  (x -1)
  (y -1))

;; total bytesize = 104
(defstruct (saveheader (:conc-name saveheader.)
		       (:predicate nil)
		       (:copier nil))
  (major 83) ;; byte
  (minor 97) ;; byte
  (patch 118) ;; byte
  (extra 102) ;; byte, above numbers to implement ARFC 002 + extension
  (engine-num-version -1) ;; u16b
  (variant-num-version -1) ;; u16b, tags to check who uses them
  (variant-id "none") ;; id of variant (will take 24 bytes)
  (status -1) ;; what is the status of the savefile (u16)
  (desc "") ;; should be a description and will take 64 bytes in the header
  (block-num -1)) ;; number of blocks in file (u16)

;; total bytesize (except data) = 28   
(defstruct (saveblock (:conc-name saveblock.)
		      (:predicate nil)
		      (:copier nil))
  (vendor-tag 1337) ;; langband code (u32b)
  (type -1) ;; what kind of data, savefile-constants (u16b)
  (version -1) ;; the version counter for the engine/variant (u16b)
  (len -1) ;; length of the block (u32b)
  (checksum -1) ;; a checksum for the buffer, can be (u128b)
  (data nil)) ;; pointer to the data (length in bytes is len above)

(defstruct (keyboard-event (:conc-name kbd-event.)
			   (:predicate nil)
			   (:copier nil))
  (key nil)
  (shift nil)
  (alt nil)
  (ctrl nil))

(defstruct (mouse-event (:conc-name mouse-event.)
			(:predicate nil)
			(:copier nil))
  (button nil)
  (x nil)
  (y nil))

(defstruct (input-event (:conc-name input-event.)
			;;(:predicate nil)
			(:copier nil))
  (type nil)
  (keypress nil)
  (mouseclick nil))

;;; end structs

(defclass visual-event ()
  ((id        :accessor visevent.id        :initarg :id        :initform "7")
   (flags     :accessor visevent.flags     :initarg :flags     :initform 0)
   (mode      :accessor visevent.mode      :initarg :mode      :initform :fresh)
   (window    :accessor visevent.window    :initarg :window    :initform nil)
   (current-x :accessor visevent.current-x :initarg :current-x :initform 0)
   (current-y :accessor visevent.current-y :initarg :current-y :initform 0)
   (blocking? :accessor visevent.blocking? :initarg :blocking? :initform nil)
   (data      :accessor visevent.data      :initarg :data      :initform nil)
   ))

(defgeneric init-visual-event (evt dungeon player cur-tick))
(defgeneric trigger-visual-event (evt dungeon player cur-tick))
(defgeneric finalise-visual-event (evt dungeon player cur-tick))

(defclass sprite-movement (visual-event)
  ((object      :accessor visevent.object      :initarg :object      :initform nil)
   (animation   :accessor visevent.animation   :initarg :animation   :initform nil)
   (move-speed  :accessor visevent.move-speed  :initarg :move-speed  :initform 0)
   (last-x      :accessor visevent.last-x      :initarg :last-x      :initform 0)
   (last-y      :accessor visevent.last-y      :initarg :last-y      :initform 0)
   (last-move-x :accessor visevent.last-move-x :initarg :last-move-x :initform 0)
   (last-move-y :accessor visevent.last-move-y :initarg :last-move-y :initform 0)
   (source-x    :accessor visevent.source-x    :initarg :source-x    :initform 0)
   (source-y    :accessor visevent.source-y    :initarg :source-y    :initform 0)
   (target-x    :accessor visevent.target-x    :initarg :target-x    :initform 0)
   (target-y    :accessor visevent.target-y    :initarg :target-y    :initform 0)
   (angle       :accessor visevent.angle       :initarg :angle       :initform 0)
   ))

(defstruct (animation (:conc-name anim.))
  (id "anim")
  (current 0);; index in path
  (number 1)
  (path '())
  (next-change 0)
  (change-interval 0))

(defclass walking-movement (sprite-movement)
  ((move-speed :initform +walk-speed+)
   (blocking? :initform t)
   ))

(defclass thrown-object-movement (sprite-movement)
  ((move-speed :initform (* 3 +walk-speed+))
   (blocking? :initform t)
   ))

(defclass missile-movement (sprite-movement)
  ((move-speed      :initform (* 3 +walk-speed+))
   (blocking?       :initform t)
   (shooter         :accessor visevent.shooter        :initform nil)
   (missile-weapon  :accessor visevent.missile-weapon :initform nil)
   ))

(defclass message-handler ()
  ((cur-msg-col   :initform  0
		  :accessor msghandler.cur-msg-col
		  :documentation "At what column in the message-frame is the message-printer.")
   (cur-msg-row   :initform  0
		  :accessor msghandler.cur-msg-row
		  :documentation "At what row in the message-frame is the message-printer.")
   (cur-max-col   :initform 60
		  :accessor msghandler.cur-max-col
		  :documentation "What is the maximum coloumn in the message-frame.")
   (state         :initform :ready
		  :accessor msghandler.state
		  :documentation "State of the message-system.") ;; :ready or :pause
   (incoming-msgs :initform '()
		  :accessor msghandler.incoming-msgs
		  :documentation "A list of incoming messages that must be displayed.")
   (shown-msgs    :initform '()
		  :accessor msghandler.shown-msgs
		  :documentation "A list of messages already shown."))
  (:documentation "This is an 'abstract' class for handling game-messages that
should be presented to the user.  How these are presented may differ from
player to player so it's put in a class for further tweaking by variants.
The langband engine provides two working subclasses, a -more- based one, and
one message handler that lets messages flow freely.  Adding a filter should also
be doable."))

(defclass message-handler-more (message-handler)
  ()
  (:documentation "A message-handler that requires you to use -more- for every line,
traditional angband-style.  Good for absolute control or if you've
just got one line of messages in the message-window."))

(defclass message-handler-flow (message-handler)
  ()
  (:documentation "A message-handler that lets messages just 'flow' by, requiring
no user-attention.  Less choppy gameplay and if you have a few lines in the message
window you're unlikely to miss much."))

(defstruct (field-printer (:copier nil)
			  (:conc-name field-printer.))
  key
  col
  row
  window-key
  handler)

