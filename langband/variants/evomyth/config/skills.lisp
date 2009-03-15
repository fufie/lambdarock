(in-package :org.langband.evomyth)

(define-skill "smithing" 'smithing '<smithing>
	      :desc "Ability to fix your metal weapons or armour, and even make your own weapons and armour."
	      :index 0
	      )
(define-skill "leatherwork" 'leatherwork '<leatherwork>
	      :desc "Allows you to fix your leather sack, armour, pouch, shoes, etc."
	      :index 1
	      )

(define-skill "woodcraft" 'woodcraft '<woodcraft>
	      :desc "Abilitiy to carve out bows, know weak spots of wooden doors, fix broken wagons, etc."
	      :index 2
	      )

(define-skill "monster-lore" 'monster-lore '<monster-lore>
	      :desc "Know about monsters and foes, their special attacks and their weak spots.  Increases the chance of critical hits in combat."
	      :index 3
	      )

(define-skill "object-lore" 'object-lore '<object-lore>
	      :desc "Know the history of objects you find, the legends and even simple stuff like figuring out the purpose and use of the object."
	      :index 4
	      )

(define-skill "intuition" 'intuition '<intuition>
	      :desc "A weird skill that lets you pick mysteriously know of hidden dangers, when people are lying, events that might need your attention, etc."
	      :index 5
	      )

(define-skill "animal-handling" 'animal-handling '<animal-handling>
	      :desc "Ability to calm down animals, control a caravan, understand animal behaviour and to know the weak spots of animals."
	      :index 6
	      )
(define-skill "alchemy" 'alchemy '<alchemy>
	      :desc "Find and identify herbs, know effects of herbs, be able to make potions and to identify potions."
	      :index 7
	      )

;; combat related skills

(define-skill "unarmed" 'unarmed '<unarmed>  
	      :desc "Ability to fight with bare hands and feet (martial artists, monks, boxers)."
	      :index 8
	      )

(define-skill "blades" 'blades '<blades> 
	      :desc "Ability to use blades in combat."
	      :index 9
	      )
   
(define-skill "bludgeoning" 'bludgeoning '<bludgeoning> 
	      :desc "Ability to use bludgeoning weapons in combat."
	      :index 10
	      )

(define-skill "archery" 'archery '<archery> 
	      :desc "Ability to use bows, crossbows and thrown weapons."
	      :index 11
	      )

(define-skill "light-armour" 'light-armour '<light-armour> 
	      :desc "Ability to wear light armour efficiently in combat."
	      :index 12
	      )
   
(define-skill "evasion" 'evasion '<evasion>  
	      :desc "Ability to read an opponent and evade attacks altogteher."
	      :index 13
	      )
