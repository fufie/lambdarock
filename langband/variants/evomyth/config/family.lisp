;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/family.lisp - family members
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-monster-kind "grandpa" "Grandpa"
  :numeric-id  1001
  :gfx-sym (tile-paint-value 14 10)
  :desc "He's your old grandpa and all you have left."
  :text-sym (text-paint-value +term-red+ #\p)
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique>)
  :diet '<omnivore>
  :picture '(variant-gfx "people/perpetro.png")
  :gender '<male>)

(define-monster-kind "grandma" "Grandma"
  :numeric-id  1002
  :gfx-sym (tile-paint-value 14 3)
  :desc "She's your old grandma and all you have left."
  :text-sym (text-paint-value +term-red+ #\p)
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique>)
  :diet '<omnivore>  
  :picture '(variant-gfx "people/perpetro.png")
  :gender '<female>)