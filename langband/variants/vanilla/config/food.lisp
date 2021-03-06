;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/food.lisp - eatable objects for vanilla variant
Copyright (c) 2002 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

(define-object-kind "mushroom-blindness" "blindness"
  :numeric-id 1
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 1
  :cost 0
  :sort-value 6001
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<blindness> :add (+ 200 (random 200)))
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-paranoia" "paranoia"
  :numeric-id 2
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 1
  :cost 0
  :sort-value 6002
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<fear> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-confusion" "confusion"
  :numeric-id 3
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 1
  :cost 0
  :sort-value 6003
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<confusion> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-hallucination" "hallucination"
  :numeric-id 4
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 0
  :sort-value 6004
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<hallucinate> :add (+ 250 (random 250)))
	      (possible-identify! player item))
	    :used)
  :food-value 500) 

(define-object-kind "mushroom-cure-poison" "cure poison"
  :numeric-id 5
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 60
  :sort-value 6012
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<poisoned> :new-value nil)
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-cure-blindness" "cure blindness"
  :numeric-id 6
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 50
  :sort-value 6013
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<blindness> :new-value nil)
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-cure-paranoia" "cure paranoia"
  :numeric-id 7
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 25
  :sort-value 6014
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<fear> :new-value nil)
	      (possible-identify! player item))
	    :used)

  :food-value 500)

(define-object-kind "mushroom-cure-confusion" "cure confusion"
  :numeric-id 8
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 50
  :sort-value 6015
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<confusion> :new-value nil)
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-weakness" "weakness"
  :numeric-id 9
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 0
  :sort-value 6006
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<str> '<reduce>)
	    (possible-identify! player item)
	    :used)
  
  :food-value 500)

(define-object-kind "mushroom-unhealth" "unhealth"
  :numeric-id 10
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 1
  :cost 50
  :sort-value 6010
  :the-kind '<mushroom>
  :food-value 500
  :damage "10d10")

(define-object-kind "mushroom-restore-con" "restore constitution"
  :numeric-id 11
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 1
  :cost 350
  :sort-value 6018
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (update-player-stat! player '<con> '<restore>)
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-restoring" "restoring"
  :numeric-id 12
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 20
  :locations '((20 . 8) (30 . 4) (40 . 1))
  :weight 1
  :cost 1000
  :sort-value 6019
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (update-player-stat! player '<str> '<restore>)
	    (update-player-stat! player '<dex> '<restore>)
	    (update-player-stat! player '<con> '<restore>)
	    (update-player-stat! player '<int> '<restore>)
	    (update-player-stat! player '<wis> '<restore>)
	    (update-player-stat! player '<chr> '<restore>)
	    (possible-identify! player item)
	    :used)
  :food-value 500)

(define-object-kind "mushroom-stupidity" "stupidity"
  :numeric-id 13
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 1
  :cost 0
  :sort-value 6008
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<int> '<reduce>)
	    (possible-identify! player item)
	    :used)
  
  :food-value 500)

(define-object-kind "mushroom-naivety" "naivety"
  :numeric-id 14
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 1
  :cost 0
  :sort-value 6009
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<wis> '<reduce>)
	    (possible-identify! player item)
	    :used)
  :food-value 500)

(define-object-kind "mushroom-poison" "poison"
  :numeric-id 15
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 5
  :locations '((5 . 1) (5 . 1))
  :weight 1
  :cost 0
  :sort-value 6000
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<poisoned> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-sickness" "sickness"
  :numeric-id 16
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 1
  :cost 0
  :sort-value 6007
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    ;; add damage and desc
	    (update-player-stat! player '<con> '<reduce>)
	    (possible-identify! player item)
	    :used)
  :food-value 500)

(define-object-kind "mushroom-paralysis" "paralysis"
  :numeric-id 17
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 1
  :cost 0
  :sort-value 6005
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (modify-creature-state! player '<paralysed> :add (+ 10 (random 10)))
	      (possible-identify! player item))
	    :used)
  
  :food-value 500)

(define-object-kind "mushroom-restore-str" "restore strength"
  :numeric-id 18
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 1
  :cost 350
  :sort-value 6017
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (update-player-stat! player '<con> '<restore>)
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "mushroom-disease" "disease"
  :numeric-id 19
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 1
  :cost 50
  :sort-value 6011
  :the-kind '<mushroom>
  :on-eat #'%dummy-eat-fun
  :food-value 500
  :damage "10d10")

(define-object-kind "mushroom-cure-serious" "cure serious wounds"
  :numeric-id 20
  :text-sym (text-paint-value +term-dark+ #\,)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 2
  :cost 75
  :sort-value 6016
  :the-kind '<mushroom>
  :on-eat (object-effect (dungeon player item)
	    (when (heal-creature! player (roll-dice 4 8))
	      (possible-identify! player item))
	    :used)
  :food-value 500)

(define-object-kind "food-ration" "& ration~ of food"
  :numeric-id 21
  :text-sym (text-paint-value +term-l-umber+ #\,)
  :gfx-sym (tile-paint-value 5 4)
  :power-lvl 0
  :locations '((0 . 1) (5 . 1) (10 . 1))
  :weight 10
  :cost 3
  :sort-value 6035
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 5000)

(define-object-kind "biscuit" "& hard biscuit~"
  :numeric-id 22
  :text-sym (text-paint-value +term-l-umber+ #\,)
  :gfx-sym (tile-paint-value 5 2)
  :power-lvl 0
  :weight 2
  :cost 1
  :sort-value 6032
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 500)

(define-object-kind "beef-jerky" "& strip~ of beef jerky"
  :numeric-id 23
  :text-sym (text-paint-value +term-umber+ #\,)
  :gfx-sym (tile-paint-value 5 3)
  :power-lvl 0
  :weight 2
  :cost 2
  :sort-value 6033
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 1500)

(define-object-kind "slime-mold" "& slime mold~"
  :numeric-id 24
  :text-sym (text-paint-value +term-green+ #\,)
  :gfx-sym (tile-paint-value 5 5)
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 2
  :sort-value 6036
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 3000)

(define-object-kind "elvish-bread" "& piece~ of elvish waybread"
  :numeric-id 25
  :text-sym (text-paint-value +term-l-blue+ #\,)
  :gfx-sym (tile-paint-value 5 6)
  :power-lvl 5
  :locations '((5 . 1) (10 . 1) (20 . 1))
  :weight 3
  :cost 10
  :sort-value 6037
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 7500)

(define-object-kind "pint-ale" "& pint~ of fine ale"
  :numeric-id 26
  :gfx-sym (tile-paint-value 5 0)
  :text-sym (text-paint-value +term-yellow+ #\,)
  :power-lvl 0
  :weight 5
  :cost 1
  :sort-value 6038
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 500)

(define-object-kind "pint-wine" "& pint~ of fine wine"
  :numeric-id 27
  :gfx-sym (tile-paint-value 5 1)
  :text-sym (text-paint-value +term-red+ #\,)
  :power-lvl 0
  :weight 10
  :cost 2
  :sort-value 6039
  :the-kind '<food>
  :on-eat #'%dummy-eat-fun
  :food-value 1000)
