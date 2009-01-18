;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defconstant +common-backpack-size+ 23)

(define-object-kind 
    "backpack" "backpack" :numeric-id 750
    :text-sym (text-paint-value +term-white+ #\&)
    :gfx-sym (tile-paint-value +tilefile-misc+ 0) ;; improve later
    :power-lvl 10 :weight nil
    :cost 1200 :the-kind '<container>
    :on-create #'(lambda (item)
		   (let ((container (make-container +common-backpack-size+)))
		     (setf (aobj.contains item) container)
		     t))
    )


(register-information& "status-roll" 100 ;; what's the roll of status
		       "status-cap" 100) ;; what's max status

(register-information& "which-town" "vanilla")
;;(register-information& "which-town" "bartertown")

;;; the various elements you can face in Vanilla
(define-element '<fire>        "fire"        :bit-flag #x00000001 :number 0)
(define-element '<acid>        "acid"        :bit-flag #x00000002 :number 1)
(define-element '<electricity> "electricity" :bit-flag #x00000004 :number 2)
(define-element '<cold>        "cold"        :bit-flag #x00000008 :number 3)
(define-element '<poison>      "poison"      :bit-flag #x00000010 :number 4)
(define-element '<darkness>    "darkness"    :bit-flag #x00000020 :number 5)
(define-element '<light>       "solar light" :bit-flag #x00000040 :number 6)
(define-element '<blindness>   "blindness"   :bit-flag #x00000080 :number 7)
(define-element '<disenchant>  "disenchant"  :bit-flag #x00000100 :number 8)
(define-element '<shards>      "shards"      :bit-flag #x00000200 :number 9)
(define-element '<confusion>   "confusion"   :bit-flag #x00000400 :number 10)
(define-element '<nexus>       "nexus"       :bit-flag #x00000800 :number 11)
(define-element '<sound>       "sound"       :bit-flag #x00001000 :number 12)
(define-element '<nether>      "nether"      :bit-flag #x00002000 :number 13)
(define-element '<gravity>     "gravity"     :bit-flag #x00004000 :number 14)
(define-element '<chaos>       "chaos"       :bit-flag #x00008000 :number 15)
(define-element '<fear>        "fear"        :bit-flag #x00010000 :number 16)
(define-element '<sleep>       "sleep"       :bit-flag #x00020000 :number 17)
(define-element '<plasma>      "plasma"      :bit-flag #x00040000 :number 18)
(define-element '<mana>        "mana"        :bit-flag #x00080000 :number 19)
(define-element '<water>       "water"       :bit-flag #x00100000 :number 20)
(define-element '<time>        "time"        :bit-flag #x00200000 :number 21)
(define-element '<inertia>     "inertia"     :bit-flag #x00400000 :number 22)
(define-element '<force>       "force"       :bit-flag #x00800000 :number 23)
(define-element '<holiness>    "holiness"    :bit-flag #x01000000 :number 24)
(define-element '<erosion>     "erosion"     :bit-flag #x02000000 :number 25)
;;(define-element '<weak-light>  "sun light"   :bit-flag #x04000000 :number 26)
;;(define-element '<polymorph>   "polymorph"   :bit-flag #x08000000 :number 27) ;; getting out of hand

;; various effects for the players and the monsters in vanilla

(define-effect '<telepathy>       "telepathy"           :number 0  :bit-flag #x0000000001)
(define-effect '<hold-life>       "hold life"           :number 1  :bit-flag #x0000000002)
(define-effect '<see-invisible>   "see invisible"       :number 2  :bit-flag #x0000000004)
(define-effect '<blessed>         "blessed"             :number 3  :bit-flag #x0000000008)
(define-effect '<heroic>          "heroic"              :number 4  :bit-flag #x0000000010)
(define-effect '<free-action>     "free-action"         :number 5  :bit-flag #x0000000020) 
(define-effect '<berserk>         "berserk"             :number 6  :bit-flag #x0000000040)
(define-effect '<slow-digest>     "slow digestion"      :number 7  :bit-flag #x0000000080)
(define-effect '<regenerate>      "regenerate"          :number 8  :bit-flag #x0000000100)
(define-effect '<prot-from-evil>  "protected from evil" :number 9  :bit-flag #x0000000200)
(define-effect '<shielded>        "shielded"            :number 10 :bit-flag #x0000000400)
(define-effect '<hasted>          "hasted"              :number 11 :bit-flag #x0000000800)
(define-effect '<invulnerable>    "invulnerable"        :number 12 :bit-flag #x0000001000)
(define-effect '<infravision>     "infravision"         :number 13 :bit-flag #x0000002000)
(define-effect '<feather-fall>    "feather-fall"        :number 14 :bit-flag #x0000004000)
(define-effect '<glowing>         "glowing"             :number 15 :bit-flag #x0000008000)
(define-effect '<earthquake>      "earthquake"          :number 16 :bit-flag #x0000010000)
(define-effect '<blessed-blade>   "blessed blade"       :number 17 :bit-flag #x0000020000)
(define-effect '<aggravates>      "aggravates"          :number 18 :bit-flag #x0000040000)
(define-effect '<random-teleport> "random teleport"     :number 19 :bit-flag #x0000080000)
(define-effect '<drain-xp>        "drain xp"            :number 20 :bit-flag #x0000100000)
(define-effect '<slowed>          "slowed"              :number 21 :bit-flag #x0000200000)
(define-effect '<blindness>       "blindness"           :number 22 :bit-flag #x0000400000)
(define-effect '<paralysed>       "paralysed"           :number 23 :bit-flag #x0000800000)
(define-effect '<confusion>       "confusion"           :number 24 :bit-flag #x0001000000)
(define-effect '<fear>            "fear/afraid"         :number 25 :bit-flag #x0002000000)
(define-effect '<hallucinate>     "hallucinate"         :number 26 :bit-flag #x0004000000)
(define-effect '<poisoned>        "poisoned"            :number 27 :bit-flag #x0008000000)
(define-effect '<cut>             "cut"                 :number 28 :bit-flag #x0010000000)
(define-effect '<stun>            "stun"                :number 29 :bit-flag #x0020000000)
(define-effect '<recalling>       "recalling"           :number 30 :bit-flag #x0040000000)
(define-effect '<resist-acid>     "resist acid"         :number 31 :bit-flag #x0080000000)
(define-effect '<resist-elec>     "resist electricity"  :number 32 :bit-flag #x0100000000)
(define-effect '<resist-fire>     "resist fire"         :number 33 :bit-flag #x0200000000)
(define-effect '<resist-cold>     "resist cold"         :number 34 :bit-flag #x0400000000)
(define-effect '<resist-poison>   "resist poison"       :number 35 :bit-flag #x0800000000)
(define-effect '<sleeping>        "sleeping"            :number 36 :bit-flag #x1000000000)

;;; move this later
(define-visual-state :blind 1
  :desc "Blind"
  :gfx-sym (tile-paint-value +tilefile-states+ 0))

(define-visual-state :paralysed 2
  :desc "Paralysed"
  :gfx-sym (tile-paint-value +tilefile-people+ 4))

(define-visual-state :afraid 4
  :desc "Afraid"
  :gfx-sym (tile-paint-value +tilefile-undeads+ 23))

(define-visual-state :poisoned 5
  :desc "Poisoned"
  :gfx-sym (tile-paint-value +tilefile-states+ 2))

(define-visual-state :confused 6
  :desc "Confused"
  :gfx-sym (tile-paint-value +tilefile-states+ 1))

(define-visual-state :hungry 7
  :desc "Hungry"
  :gfx-sym (tile-paint-value +tilefile-misc+ 102))

(define-visual-state :can-study 9
  :desc "Can study more spells"
  :gfx-sym (tile-paint-value +tilefile-magic+ 7))


(define-variant-graphics ()
    '(
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
	(engine-gfx "tiles/buttons2.png")
	(engine-gfx "tiles/8x16-buttons.png")
#| 40 |#(engine-gfx "tiles/crosshair.png")
	(engine-gfx "tiles/states.png")
        (engine-gfx "tiles/dungeon_floors.png")
        (engine-gfx "tiles/hobbitanim.png")
        (engine-gfx "tiles/backgrounds.png")
#| 45 |#(engine-gfx "tiles/townlook.png")
      ))
