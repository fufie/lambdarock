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
  :attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :strategies '((<avoid> <carnivore> <omnivore> <player>) (<fight> <cornered>))
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
  :attacks '((<bite> :type <hurt> :damage (2 . 4)))
  :strategies '((<avoid> <carnivore> <omnivore> <player>) (<fight> <cornered> <attacked>))
  :type '(<mammal>)
  :diet '<herbivore>
  :gender '<female>)

(define-monster-kind "wild-hound" "Wild Hound"
  :numeric-id  2002
  :gfx-sym (tile-paint-value 18 72)
  :desc "A rough and wild hound, which usually appears in packs.  Can be extremely dangerous in larger packs."
  :text-sym (text-paint-value +term-umber+ #\C)
  :depth 0
  :rarity 1
  :power-lvl 6
  :hitpoints '(6 . 6)
  :armour 30
  :speed 120
  :xp 30
  :abilities '()
  :alertness 150
  :vision 30
  :attacks '((<bite> :type <hurt> :damage (1 . 6)))
  :strategies '(<fight>)
  :type '(<mammal>)
  :diet '<carnivore>
  :gender '<male>)
