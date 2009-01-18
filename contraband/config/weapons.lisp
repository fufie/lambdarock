;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/weapons.lisp - to kill nasty creeps
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)


(define-object-kind "dagger" "& dagger~"
  :numeric-id 1001
  :gfx-sym (tile-paint-value +tilefile-weapons+ 3)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :locations '((0 . 1) (5 . 1) (10 . 1) (20 . 1))
  :weight 12
  :cost 10
  :power-lvl 1
  :flags '(<show-modififers>)
  :sort-value 1001
  :the-kind '<short-blade>
  :damage "1d4")

(define-object-kind "short-sword" "& short sword~"
  :numeric-id 1002
  :gfx-sym (tile-paint-value 13 8)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :locations '((5 . 1))
  :weight 80
  :cost 90
  :power-lvl 3
  :flags '(<show-modififers>)
  :sort-value 1002
  :the-kind '<short-blade>
  :damage "1d7")
