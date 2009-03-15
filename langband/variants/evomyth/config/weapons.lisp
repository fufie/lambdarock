;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/weapons.lisp - to kill nasty creeps
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)


(define-object-kind "stone-dagger" "& dagger~"
  :numeric-id 1001
  :gfx-sym (tile-paint-value +tilefile-weapons+ 3)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :locations '((0 . 1) (5 . 1) (10 . 1) (20 . 1))
  :weight 12
  :cost 10
  :power-lvl 1
  :flags '(<show-modififers>)
  :sort-value 1001
  :the-kind '<blade>
  :material '<stone>
  :damage "1d4")

(define-object-kind "short-bow" "& short bow~"
  :numeric-id 73
  :gfx-sym (tile-paint-value +tilefile-weapons+ 43)
  :text-sym (text-paint-value +term-l-umber+ #\})
  :power-lvl 3
  :locations '((3 . 1))
  :weight 30
  :cost 50
  :multiplier 2
  :flags '(<show-modififers>)
  :sort-value 2812
  :material '<wood>
  :the-kind '<bow>)

(define-object-kind "arrow" "& arrow~"
  :numeric-id 78
  :gfx-sym (tile-paint-value +tilefile-weapons+ 49)
  :text-sym (text-paint-value +term-l-umber+ #\{)
  :power-lvl 3
  :locations '((3 . 1) (15 . 1))
  :weight 2
  :cost 1
  :flags '(<show-modififers>)
  :visual-effect "arrow"
  :sort-value 2601
  :the-kind '<ammo>
  :vulnerabilities '(<fire> <plasma> <acid>)
  :material '<wood>
  :damage "d4")
