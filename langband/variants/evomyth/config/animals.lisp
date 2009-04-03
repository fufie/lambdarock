;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/animals.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-monster-kind "horned-grazer" "Horned Grazer"
  :numeric-id  2000
  :gfx-sym (tile-paint-value 19 50)
  :desc "A small herbivore that mainly has fresh grass on its diet.  It has very tasty and nutritious meat.  Can be dangerous when threatened, and has a fairly thick hide."
  :text-sym (text-paint-value +term-green+ #\g)
  :depth 0
  :rarity 1
  :power-lvl 2
  :hitpoints '(2 . 10)
  :armour 50
  :speed 120
  :abilities '()
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :strategies '((<avoid> <carnivore> <omnivore> <player>) (<fight> <if-cornered>))
  :type '(<mammal>)
  :diet '<herbivore>
  :gender '<female>)

(define-monster-kind "sabretooth-mouse" "Sabretooth Mouse"
  :numeric-id  2001
  :gfx-sym (tile-paint-value 16 24)
  :desc "A small, but vicious beast that avoids people.  They are omnivores and can kill most small animals.  The meat can be quite tasty although a bit stringy.."
  :text-sym (text-paint-value +term-white+ #\m)
  :depth 0
  :rarity 1
  :power-lvl 2
  :hitpoints '(2 . 10)
  :armour 20
  :speed 110
  :abilities '()
  :alertness 150
  :vision 8
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :strategies '((<avoid> <carnivore> <omnivore> <player>) (<fight> <if-cornered>))
  :type '(<mammal>)
  :diet '<herbivore>
  :gender '<female>)
