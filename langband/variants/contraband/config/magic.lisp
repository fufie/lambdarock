;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/magic.lisp - magic defs
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(define-technique 'agnosco "perceive/learn")
(define-technique 'amplio "improve/enlarge")
(define-technique 'arcesso "conjure/summon")
(define-technique 'creo "create from nothing")
(define-technique 'deleo "destroy/damage/hurt")
(define-technique 'mutatio "move/control/change")
(define-technique 'novo "restore/make whole")
(define-technique 'tutis "protect/defend")

(define-domain 'aeris "air/weather")
(define-domain 'aqua "water/acid")
(define-domain 'arbor "plants/herbs/food")
(define-domain 'bestia "beasts/monsters/animals")
(define-domain 'hominid "humans/human-like creatures")
(define-domain 'ignis "fire")
(define-domain 'imago "images/senses")
(define-domain 'luna "moon/reflection/darkness")
(define-domain 'mortis "death")
(define-domain 'sol "sun/light")
(define-domain 'terra "earth/metal")

(define-effect-zone 'self "self")
(define-effect-zone 'touch "touch")
(define-effect-zone 'arrow "arrow")
(define-effect-zone 'beam "beam")
(define-effect-zone 'sphere "sphere")
(define-effect-zone 'storm "storm")
(define-effect-zone 'in-sight "in sight of caster")

(define-rune-material 'wood)
(define-rune-material 'stone)
(define-rune-material 'silver)
(define-rune-material 'dragon-tooth)

;; these will change
(defconstant +cantrip-difficulty+ 5)
(defconstant +easy-spell-difficulty+ 10)
(defconstant +normal-spell-difficulty+ 15)
;;(defconstant +normal-spell-difficulty+ 15)
