;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/effects.lisp - definition of magic effects
Copyright (c) 2000-2002,2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)


;;; colours aren't necessarily the same as in vanilla angband.. these colours
;;; are more tweaked for graphics.

;; light blue
(define-spell-effect "magic-missile"
    :gfx-beam (tile-paint-value +effect-file+ 51)
    :text-beam (text-paint-value +term-l-blue+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 63)
    :text-ball (text-paint-value +term-l-blue+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 101)
    :text-orb (text-paint-value +term-l-blue+ #\*)
    :gfx-bolts (gfx-bolt-array 24)
    :text-bolts (text-bolt-array +term-l-blue+))

;; yellow
(define-spell-effect "electricity"
    :gfx-beam (tile-paint-value +effect-file+ 54)
    :text-beam (text-paint-value +term-yellow+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 66)
    :text-ball (text-paint-value +term-yellow+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 99)
    :text-orb (text-paint-value +term-yellow+ #\*)
    :gfx-bolts (gfx-bolt-array 36)
    :text-bolts (text-bolt-array +term-yellow+))

;; red
(define-spell-effect "fire"
    :gfx-beam (tile-paint-value +effect-file+ 48)
    :text-beam (text-paint-value +term-red+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 60)
    :text-ball (text-paint-value +term-red+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 96)
    :text-orb (text-paint-value +term-red+ #\*)
    :gfx-bolts (gfx-bolt-array 12)
    :text-bolts (text-bolt-array +term-red+))

;; green
(define-spell-effect "acid"
    :gfx-beam (tile-paint-value +effect-file+ 49)
    :text-beam (text-paint-value +term-green+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 61)
    :text-ball (text-paint-value +term-green+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 97)
    :text-orb (text-paint-value +term-green+ #\*)
    :gfx-bolts (gfx-bolt-array 16)
    :text-bolts (text-bolt-array +term-green+))

;; white
(define-spell-effect "cold"
    :gfx-beam (tile-paint-value +effect-file+ 56)
    :text-beam (text-paint-value +term-white+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 70)
    :text-ball (text-paint-value +term-white+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 101)
    :text-orb (text-paint-value +term-white+ #\*)
    :gfx-bolts (gfx-bolt-array 0)
    :text-bolts (text-bolt-array +term-white+))

;; brown
(define-spell-effect "poison"
    :gfx-beam (tile-paint-value +effect-file+ 52)
    :text-beam (text-paint-value +term-umber+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 64)
    :text-ball (text-paint-value +term-umber+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 97)
    :text-orb (text-paint-value +term-umber+ #\*)
    :gfx-bolts (gfx-bolt-array 28)
    :text-bolts (text-bolt-array +term-umber+))

;; orange
(define-spell-effect "light"
    :gfx-beam (tile-paint-value +effect-file+ 55)
    :text-beam (text-paint-value +term-orange+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 68)
    :text-ball (text-paint-value +term-orange+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 99)
    :text-orb (text-paint-value +term-orange+ #\*)
    :gfx-bolts (gfx-bolt-array 40)
    :text-bolts (text-bolt-array +term-orange+))

;; l-dark
(define-spell-effect "darkness"
    :gfx-beam (tile-paint-value +effect-file+ 58)
    :text-beam (text-paint-value +term-l-dark+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 71)
    :text-ball (text-paint-value +term-l-dark+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 97)
    :text-orb (text-paint-value +term-l-dark+ #\*)
    :gfx-bolts (gfx-bolt-array 8)
    :text-bolts (text-bolt-array +term-l-dark+))

;; brown
(define-spell-effect "erosion"
    :gfx-beam (tile-paint-value +effect-file+ 52)
    :text-beam (text-paint-value +term-umber+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 64)
    :text-ball (text-paint-value +term-umber+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 97)
    :text-orb (text-paint-value +term-umber+ #\*)
    :gfx-bolts (gfx-bolt-array 28)
    :text-bolts (text-bolt-array +term-umber+))

;; enhance, it's a hack, but should stick around I guess
(define-spell-effect "enhance"
    :gfx-beam (tile-paint-value +effect-file+ 51)
    :text-beam (text-paint-value +term-l-blue+ #\*)
    :gfx-ball (tile-paint-value +effect-file+ 63)
    :text-ball (text-paint-value +term-l-blue+ #\*)
    :gfx-orb (tile-paint-value +effect-file+ 101)
    :text-orb (text-paint-value +term-l-blue+ #\*)
    :gfx-bolts (gfx-bolt-array 24)
    :text-bolts (text-bolt-array +term-l-blue+))


(define-visual-projectile "arrow"
    :gfx-path (gfx-missile-array 108)
    :text-path (text-bolt-array +term-umber+))

(define-visual-projectile "bolt"
    :gfx-path (gfx-missile-array 116)
    :text-path (text-bolt-array +term-umber+))

(define-visual-projectile "stone"
    :gfx-path (make-array 10 :initial-element (tile-paint-value 13 54))
    :text-path (text-bolt-array +term-slate+))
