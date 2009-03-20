;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/animals.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-monster-kind "horned-grazer" "Horned Grazer"
  :numeric-id  2000
  :gfx-sym (tile-paint-value 19 10)
  :desc "A small herbivore that mainly has fresh grass on its diet.  It has very tasty and nutritious meat.  Can be dangerous when threatened, and has a fairly thick hide."
  :text-sym (text-paint-value +term-green+ #\g)
  :depth 0
  :rarity 1
  :power-lvl 2
  :hitpoints '(2 . 10)
  :armour 50
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '()
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :strategies '(<avoid-carnivore> <avoid-player> <fight-if-cornered>)
  :type '(<herbivore>)
  :gender '<female>)
