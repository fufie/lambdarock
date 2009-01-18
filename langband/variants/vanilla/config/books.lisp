;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/books.lisp - spellbooks for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; ===  mage-books

(define-object-kind "magic-beginner" "Magic for Beginners"
  :numeric-id 330
  :gfx-sym (tile-paint-value 9 0)
  :text-sym (text-paint-value +term-l-red+ #\?)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 30
  :cost 25
  :sort-value 6900
  :the-kind '<spellbook>
  :spells '("magic-missile"
	    "mage-detect-monsters"
	    "phase-door"
	    "light-area"
	    "treasure-detection"
	    "mage-cure-light-wounds"
	    "object-detection"
	    "find-traps/doors"
	    "stinking-cloud")
  :damage 1) 

(define-object-kind "conjurings-and-tricks" "Conjurings and Tricks"
  :numeric-id 331
  :gfx-sym (tile-paint-value 9 1)
  :text-sym (text-paint-value +term-l-red+ #\?)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 30
  :cost 100
  :sort-value 6901
  :the-kind '<spellbook>
  :spells '("confuse-monster"
	    "lightning-bolt"
	    "trap/door-destruction"
	    "sleep-1"
	    "cure-poison"
	    "mage-teleport-self"
	    "spear-of-light"
	    "frost-bolt"
	    "stone-to-mud")
  :damage 1) 

(define-object-kind "incantations" "Incantation and Illusions"
  :numeric-id 332
  :gfx-sym (tile-paint-value 9 2)
  :text-sym (text-paint-value +term-l-red+ #\?)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 30
  :cost 400
  :sort-value 6902
  :the-kind '<spellbook>
  :spells '("mage-satisfy-hunger"
	    "recharge-item-1"
	    "sleep-2"
	    "polymorph-other"
	    "identify"
	    "sleep-3"
	    "fire-bolt"
	    "slow-monster")
  :damage 1) 

(define-object-kind "sorcery-evocations" "Sorcery and Evocations"
  :numeric-id 333
  :gfx-sym (tile-paint-value 9 3)
  :text-sym (text-paint-value +term-l-red+ #\?)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 30
  :cost 800
  :sort-value 6903
  :the-kind '<spellbook>
  :spells '("frost-ball"
	    "recharge-item-2"
	    "mage-teleport-other"
	    "haste-self"
	    "fire-ball"
	    "mage-word-of-destruction"
	    "xenocide-1")
  :damage 1) 


(define-object-kind "resistance-scarab" "Resistance of Scarabtarices"
  :numeric-id 379
  :gfx-sym (tile-paint-value 9 4)
  :text-sym (text-paint-value +term-red+ #\?)
  :power-lvl 40
  :locations '((40 . 1))
  :weight 30
  :cost 5000
  :sort-value 6904
  :the-kind '<spellbook>
  :spells '("resist-fire"
	    "resist-cold"
	    "resist-acid"
	    "resist-poison"
	    "resistance")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "mordenkainen-escapes" "Mordenkainen's Escapes"
  :numeric-id 380
  :gfx-sym (tile-paint-value 9 5)
  :text-sym (text-paint-value +term-red+ #\?)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 30
  :cost 10000
  :sort-value 6905
  :the-kind '<spellbook>
  :spells '("door-creation"
	    "stair-creation"
	    "mage-teleport-level"
	    "mage-earthquake"
	    "mage-word-of-recall")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "keleks-grimoire" "Kelek's Grimoire of Power"
  :numeric-id 381
  :gfx-sym (tile-paint-value 9 6)
  :text-sym (text-paint-value +term-red+ #\?)
  :power-lvl 60
  :locations '((60 . 1))
  :weight 30
  :cost 30000
  :sort-value 6906
  :the-kind '<spellbook>
  :spells '("mage-detect-evil"
	    "detect-enchantment"
	    "recharge-item-3"
	    "xenocide-2"
	    "mass-xenocide")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "tensers-transformations" "Tenser's transformations"
  :numeric-id 382
  :gfx-sym (tile-paint-value 9 7)
  :text-sym (text-paint-value +term-red+ #\?)
  :power-lvl 80
  :locations '((80 . 2))
  :weight 30
  :cost 50000
  :sort-value 6907
  :the-kind '<spellbook>
  :spells '("heroism"
	    "shield"
	    "berserker"
	    "essence-of-speed"
	    "globe-of-invulnerability")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "raals-tome" "Raal's Tome of Destruction"
  :numeric-id 383
  :gfx-sym (tile-paint-value 9 8)
  :text-sym (text-paint-value +term-red+ #\?)
  :power-lvl 100
  :locations '((100 . 4))
  :weight 30
  :cost 100000
  :sort-value 6908
  :the-kind '<spellbook>
  :spells '("acid-bolt"
	    "cloud-kill"
	    "acid-ball"
	    "ice-storm"
	    "meteor-swarm"
	    "mana-storm")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))


;;; ===  priest-books

(define-object-kind "beginner-handbook" "Beginners Handbook"
  :numeric-id 334
  :gfx-sym (tile-paint-value 9 9)
  :text-sym (text-paint-value +term-l-green+ #\?)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 30
  :cost 25
  :sort-value 7000
  :the-kind '<prayerbook>
  :spells '("priest-detect-evil"
	    "priest-cure-light-wounds"
	    "bless"
	    "remove-fear"
	    "call-light"
	    "find-traps"
	    "detect-doors/stairs"
	    "slow-poison")
  :damage 1) 

(define-object-kind "words-wisdom" "Words of Wisdom"
  :numeric-id 335
  :gfx-sym (tile-paint-value 9 10)
  :text-sym (text-paint-value +term-l-green+ #\?)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 30
  :cost 100
  :sort-value 7001
  :the-kind '<prayerbook>
  :spells '("scare-monster"
	    "portal"
	    "cure-serious-wounds"
	    "chant"
	    "sanctuary"
	    "priest-satisfy-hunger"
	    "remove-curse"
	    "resist-heat-and-cold")
  :damage 1) 

(define-object-kind "chants-blessings" "Chants and Blessings"
  :numeric-id 336
  :gfx-sym (tile-paint-value 9 11)
  :text-sym (text-paint-value +term-l-green+ #\?)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 30
  :cost 300
  :sort-value 7002
  :the-kind '<prayerbook>
  :spells '("neutralize-poison"
	    "orb-of-draining"
	    "cure-critical-wounds"
	    "sense-invisible"
	    "protection-from-evil"
	    "priest-earthquake"
	    "sense-surroundings"
	    "cure-mortal-wounds"
	    "turn-undead")
  :damage 1) 

(define-object-kind "exorcism-dispelling" "Exorcism and Dispelling"
  :numeric-id 337
  :gfx-sym (tile-paint-value 9 12)
  :text-sym (text-paint-value +term-l-green+ #\?)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 30
  :cost 900
  :sort-value 7003
  :the-kind '<prayerbook>
  :spells '("prayer"
	    "dispel-undead"
	    "heal"
	    "dispel-evil"
	    "glyph-of-warding"
	    "holy-word")
  :damage 1) 


(define-object-kind "ethereal-openings" "Ethereal openings"
  :numeric-id 384
  :gfx-sym (tile-paint-value 9 13)
  :text-sym (text-paint-value +term-green+ #\?)
  :power-lvl 40
  :locations '((40 . 1))
  :weight 30
  :cost 5000
  :sort-value 7004
  :the-kind '<prayerbook>
  :spells '("blink"
	    "priest-teleport-self"
	    "priest-teleport-other"
	    "priest-teleport-level"
	    "priest-word-of-recall"
	    "alter-reality")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "godly-insights" "Godly Insights"
  :numeric-id 385
  :gfx-sym (tile-paint-value 9 14)
  :text-sym (text-paint-value +term-green+ #\?)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 30
  :cost 10000
  :sort-value 7005
  :the-kind '<prayerbook>
  :spells '("priest-detect-monsters"
	    "detection"
	    "perception"
	    "probing"
	    "clairvoyance")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "purifications" "Purifications and Healing"
  :numeric-id 386
  :gfx-sym (tile-paint-value 9 15)
  :text-sym (text-paint-value +term-green+ #\?)
  :power-lvl 60
  :locations '((60 . 1))
  :weight 30
  :cost 30000
  :sort-value 7006
  :the-kind '<prayerbook>
  :spells '("cure-serious-wounds-2"
	    "cure-mortal-wounds-2"
	    "healing"
	    "restoration"
	    "remembrance")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "holy-infusions" "Holy Infusions"
  :numeric-id 387
  :gfx-sym (tile-paint-value 9 16)
  :text-sym (text-paint-value +term-green+ #\?)
  :power-lvl 80
  :locations '((80 . 2))
  :weight 30
  :cost 50000
  :sort-value 7007
  :the-kind '<prayerbook>
  :spells '("unbarring-ways"
	    "recharging"
	    "dispel-curse"
	    "enchant-weapon"
	    "enchant-armour"
	    "elemental-brand")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "wrath-of-god" "Wrath of God"
  :numeric-id 388
  :gfx-sym (tile-paint-value 9 17)
  :text-sym (text-paint-value +term-green+ #\?)
  :power-lvl 100
  :locations '((100 . 4))
  :weight 30
  :cost 100000
  :sort-value 7008
  :the-kind '<prayerbook>
  :spells '("dispel-undead-2"
	    "dispel-evil-2"
	    "banishment"
	    "priest-word-of-destruction"
	    "annihilation")
  :damage 1
  :ignores '(<cold> <fire> <electricity> <acid>))
