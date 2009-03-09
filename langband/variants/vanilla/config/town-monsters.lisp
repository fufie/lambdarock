;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/town-monsters.lisp - town-monsters for vanilla variant
Copyright (c) 2000-2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify ;
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or ;
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; === Note ===
;;; vanilla-specific treats
;;; :depth is translated to power-lvl slot
;;; for locations slot you get (depth . rarity)

(define-monster-kind "urchin" "filthy street urchin"
  :numeric-id  1
  :desc "He looks squalid and thoroughly revolting."
  :text-sym (text-paint-value +term-l-dark+ #\t)
  :gfx-sym (tile-paint-value 14 0)
  :depth 0
  :rarity 2
  :hitpoints '(1 . 4)
  :armour 1
  :speed 110
  :abilities '(<open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 40
  :vision 4
  :attacks '((<touch> :type <eat-gold> :damage nil)
	     (<beg> :type nil :damage nil))
  :gender '<male>) 

(define-monster-kind "rogue-squint-eyed" "squint eyed rogue"
  :numeric-id  10
  :desc "A hardy, street-wise crook that knows an easy catch when it sees one."
  :text-sym (text-paint-value +term-blue+ #\t)
  :gfx-sym (tile-paint-value 14 7)
  :alignment '<evil>
  :depth 0
  :rarity 1
  :hitpoints '(2 . 8)
  :armour 8
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 99
  :vision 10
  :attacks '((<touch> :type <eat-item> :damage nil)
	     (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5))
  :gender '<male>) 

(define-monster-kind "singing-drunk" "singing, happy drunk"
  :numeric-id  11
  :desc "He makes you glad to be sober."
  :text-sym (text-paint-value +term-yellow+ #\t)
  :gfx-sym (tile-paint-value 14 8)
  :depth 0
  :rarity 1
  :hitpoints '(2 . 3)
  :armour 1
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 0
  :vision 10
  :attacks '((<beg> :type nil :damage nil))
  :treasures '((<drop-chance> 3/5) <only-drop-gold>)
  :gender '<male>) 

(define-monster-kind "aimless-merchant" "aimless looking merchant"
  :numeric-id  12
  :desc "The typical ponce around town, with purse jingling, and looking for more  amulets of adornment to buy."
  :text-sym (text-paint-value +term-orange+ #\t)
  :gfx-sym (tile-paint-value 14 9)
  :depth 0
  :rarity 1
  :hitpoints '(3 . 3)
  :armour 1
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 255
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop-chance> 3/5) <only-drop-gold>)
  :gender '<male>) 

(define-monster-kind "mercenary-mean-looking" "mean looking mercenary"
  :numeric-id  13
  :desc "No job is too low for him."
  :text-sym (text-paint-value +term-red+ #\t)
  :gfx-sym (tile-paint-value 14 10)
  :alignment '<evil>
  :depth 0
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 20
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 250
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 9/10))
  :gender '<male>) 

(define-monster-kind "scarred-veteran" "battle scarred veteran"
  :numeric-id  14
  :desc "He doesn't take to strangers kindly."
  :text-sym (text-paint-value +term-l-blue+ #\t)
  :gfx-sym (tile-paint-value 14 11)
  :depth 0
  :rarity 1
  :hitpoints '(7 . 8)
  :armour 30
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 250
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop-chance> 9/10))
  :gender '<male>) 

(define-monster-kind "cat-scrawny" "scrawny cat"
  :numeric-id  2
  :desc "A skinny little furball with sharp claws and a menacing look."
  :text-sym (text-paint-value +term-l-umber+ #\f)
  :gfx-sym (tile-paint-value 19 54)
  :type '(<animal>)
  :depth 0
  :rarity 3
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :abilities '((<random-mover> 1/4))
  :alertness 10
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (1 . 1)))) 

(define-monster-kind "dog-scruffy" "scruffy little dog"
  :numeric-id  3
  :desc "A thin flea-ridden mutt, growling as you get close."
  :text-sym (text-paint-value +term-l-umber+ #\C)
  :gfx-sym (tile-paint-value 18 72)
  :type '(<animal>)
  :depth 0
  :rarity 3
  :hitpoints '(1 . 3)
  :armour 1
  :speed 110
  :abilities '((<random-mover> 1/4))
  :alertness 5
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 1)))) 

(define-monster-kind "idiot-blubbering" "blubbering idiot"
  :numeric-id  5
  :desc "He tends to blubber a lot."
  :text-sym (text-paint-value +term-l-white+ #\t)
  :gfx-sym (tile-paint-value 14 2)
  :depth 0
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :abilities '(<pick-up-item> (<random-mover> 1/4))
  :alertness 0
  :vision 6
  :attacks '((<drool> :type nil :damage nil))
  :gender '<male>) 

(define-monster-kind "boiled-wretch" "boil-covered wretch"
  :numeric-id  6
  :desc "Ugly doesn't begin to describe him."
  :text-sym (text-paint-value +term-green+ #\t)
  :gfx-sym (tile-paint-value 14 3)
  :depth 0
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 0
  :vision 6
  :attacks '((<drool> :type nil :damage nil))
  :gender '<male>) 

(define-monster-kind "idiot-village" "village idiot"
  :numeric-id  7
  :desc "Drooling and comical, but then, what do you expect?"
  :text-sym (text-paint-value +term-l-green+ #\t)
  :gfx-sym (tile-paint-value 14 4)
  :depth 0
  :rarity 1
  :hitpoints '(4 . 4)
  :armour 1
  :speed 120
  :abilities '(<pick-up-item> (<random-mover> 1/4))
  :alertness 0
  :vision 6
  :attacks '((<drool> :type nil :damage nil))
  :gender '<male>) 

(define-monster-kind "beggar" "pitiful looking beggar"
  :numeric-id  8
  :desc "You just can't help feeling sorry for him."
  :text-sym (text-paint-value +term-l-umber+ #\t)
  :gfx-sym (tile-paint-value 14 5)
  :depth 0
  :rarity 1
  :hitpoints '(1 . 4)
  :armour 1
  :speed 110
  :abilities '(<open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 40
  :vision 10
  :attacks '((<beg> :type nil :damage nil))
  :gender '<male>) 

(define-monster-kind "mangy-leper" "mangy looking leper"
  :numeric-id  9
  :desc "You feel it isn't safe to touch him."
  :text-sym (text-paint-value +term-umber+ #\t)
  :gfx-sym (tile-paint-value 14 6)
  :depth 0
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :abilities '(<open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 50
  :vision 10
  :attacks '((<beg> :type nil :damage nil))
  :gender '<male>) 
