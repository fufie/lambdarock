;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/strategies.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

;;; Things to avoid
(define-strategy "avoid-carnivore" '<avoid-carnivore>
  '(avoidance-strategy :avoid-diet <carnivore>))
(define-strategy "avoid-herbivore" '<avoid-herbivore>
  '(avoidance-strategy :avoid-diet <herbivore>))
(define-strategy "avoid-omnivore" '<avoid-omnivore>
  '(avoidance-strategy :avoid-diet <omnivore>))

(define-strategy "avoid-player" '<avoid-player>
  '(avoidance-strategy :avoid-type <player>))

;;; Things to fight
(define-strategy "fight" '<fight>
  '(fight-strategy :when-to-fight t)) ;; always

(define-strategy "fight-if-cornered" '<fight-if-cornered>
  '(fight-strategy :when-to-fight creature-feels-cornered?))
