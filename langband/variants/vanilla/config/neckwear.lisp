;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/neckwear.lisp - all sorts of neckwear/amulets
Copyright (c) 2002-2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(define-object-kind "amulet-wis" "wisdom"
  :numeric-id 163
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 20
  :locations '((20 . 1))
  :weight 3
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4306
  :the-kind '<amulet>
  :stat-modifiers '((<wis> +1)))

(define-object-kind "amulet-chr" "charisma"
  :numeric-id 164
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 20
  :locations '((20 . 1))
  :weight 3
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4307
  :the-kind '<amulet>
  :stat-modifiers '((<chr> +1)))

(define-object-kind "amulet-searching" "searching"
  :numeric-id 165
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 30
  :locations '((30 . 4))
  :weight 3
  :cost 600
  :flags '(<hide-type>)
  :sort-value 4305
  :the-kind '<amulet>
  :skill-modifiers '(<search>))

(define-object-kind "amulet-teleport" "teleportation"
  :numeric-id 166
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 15
  :locations '((15 . 1))
  :weight 3
  :cost 0
  :flags '(<easy-know> <curse>)
  :sort-value 4301
  :the-kind '<amulet>
  :abilities '(<random-teleport>)) 

(define-object-kind "amulet-slow-digest" "slow digestion"
  :numeric-id 167
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 15
  :locations '((15 . 1))
  :weight 3
  :cost 200
  :flags '(<easy-know>)
  :sort-value 4303
  :the-kind '<amulet>
  :abilities '(<slow-digestion>))

(define-object-kind "amulet-resist-acid" "resist acid"
  :numeric-id 168
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 20
  :locations '((20 . 1))
  :weight 3
  :cost 300
  :flags '(<easy-know>)
  :sort-value 4304
  :the-kind '<amulet>
  :ignores '(<acid>)
  :resists '(<acid>))

(define-object-kind "amulet-adornment" "adornment"
  :numeric-id 169
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 15
  :locations '((15 . 1))
  :weight 3
  :cost 20
  :flags '(<easy-know>)
  :sort-value 4302
  :the-kind '<amulet>)

(define-object-kind "amulet-magi" "the magi"
  :numeric-id 171
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 50
  :locations '((50 . 4))
  :weight 3
  :cost 30000
  :sort-value 4308
  :the-kind '<amulet>
  :skill-modifiers '(<search>)
  :abilities '(<see-invisible> <free-action>)
  :ac-modifier 3
  :ignores '(<cold> <fire> <electricity> <acid>))


(define-object-kind "amulet-doom" "doom"
  :numeric-id 172
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 50
  :locations '((50 . 1))
  :weight 3
  :cost 0
  :flags '(<hide-type> <curse>)
  :sort-value 4300
  :the-kind '<amulet>
  :stat-modifiers '((<chr> -1) (<con> -1) (<dex> -1)
		    (<wis> -1) (<int> -1) (<str> -1)))
