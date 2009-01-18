;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/floors.lisp - floor-types for vanilla variant
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; === door types

(define-door-type "closed-door" "closed door"
  :numeric-id 201
  :text-sym (text-paint-value +term-l-umber+ #\+)
  :cave-flags-on +cave-wall+
  :gfx-sym (tile-paint-value 25 3))

(define-door-type "open-door" "open door"
  :numeric-id 202
  :text-sym (text-paint-value +term-l-umber+ #\')
  :cave-flags-off +cave-wall+
  :gfx-sym (tile-paint-value 25 4))

(define-door-type "destroyed-door" "destroyed door"
  :numeric-id 203
  :text-sym (text-paint-value +term-l-umber+ #\')
  :cave-flags-off +cave-wall+
  :gfx-sym (tile-paint-value 25 5))

;;; === floors

(define-floor-type "stone-building" "stone building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 65
  :gfx-sym (tile-paint-value 34 1)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))


(define-floor-type "permanent-outer-wall" "permanent outer wall"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 66
  :gfx-sym (tile-paint-value 29 1)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))


(define-floor-type "cave-wall" "cave wall"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 67
  :gfx-sym (tile-paint-value 25 76)
  :flags #.(logior +floor-flag-wall+ +floor-flag-use-light-effect+))

(define-floor-type "rubble" "pile of rubble"
  :numeric-id 70
  :text-sym (text-paint-value +term-white+ #\:)
  :flags +floor-flag-wall+
  :gfx-sym (tile-paint-value 10 55))

(define-floor-type "normal-floor" "normal floor"
  :numeric-id 91
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 0))


(define-floor-type "room-floor" "room floor"
  :numeric-id 81
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-items+
		   +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 10))



(define-floor-type "nothing" "nothing"
  :numeric-id 73
  :text-sym (text-paint-value +term-white+ #\Space)
  :flags 0
  :gfx-sym (tile-paint-value 0 0))

(define-floor-type "room-wall" "room wall"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 74
  :gfx-sym (tile-paint-value 25 63)
  :flags #.(logior +floor-flag-wall+ +floor-flag-use-light-effect+))

(define-floor-type "inside-room-wall" "inside room wall"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 76
  :gfx-sym (tile-paint-value 25 63)
  :flags #.(logior +floor-flag-wall+ +floor-flag-use-light-effect+))

(define-floor-type "stair-up" "staircase up"
  :text-sym (text-paint-value +term-white+ #\<)
  :numeric-id 77
  :gfx-sym (tile-paint-value 25 21)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+
		   +floor-flag-exit-upwards+))

(define-floor-type "stair-down" "staircase down"
  :text-sym (text-paint-value +term-white+ #\>)
  :numeric-id 78
  :gfx-sym (tile-paint-value 25 22)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+
		   +floor-flag-exit-downwards+))

;;; === shop doors (vanilla specific)
(define-floor-type "shop1" "general store"
  :text-sym (text-paint-value +term-l-umber+ #\1)
  :numeric-id 701
  :gfx-sym (tile-paint-value +tilefile-town+ 21)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop2" "armour"
  :text-sym (text-paint-value +term-l-umber+ #\2)
  :numeric-id 702
  :gfx-sym (tile-paint-value +tilefile-town+ 22)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop3" "weapons"
  :text-sym (text-paint-value +term-l-umber+ #\3)
  :numeric-id 703
  :gfx-sym (tile-paint-value +tilefile-town+ 23)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop4" "temple"
  :text-sym (text-paint-value +term-l-umber+ #\4)
  :numeric-id 704
  :gfx-sym (tile-paint-value +tilefile-town+ 24)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop5" "alchemist"
  :text-sym (text-paint-value +term-l-umber+ #\5)
  :numeric-id 705
  :gfx-sym (tile-paint-value +tilefile-town+ 25)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop6" "magic shop"
  :text-sym (text-paint-value +term-l-umber+ #\6)
  :numeric-id 706
  :gfx-sym (tile-paint-value +tilefile-town+ 26)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop7" "black market"
  :text-sym (text-paint-value +term-l-umber+ #\7)
  :numeric-id 707
  :gfx-sym (tile-paint-value +tilefile-town+ 27)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

(define-floor-type "shop8" "home"
  :text-sym (text-paint-value +term-l-umber+ #\8)
  :numeric-id 708
  :gfx-sym (tile-paint-value +tilefile-town+ 28)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))

;; === town floors

(define-floor-type "town-building-toproof" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 711
  :gfx-sym (tile-paint-value +tilefile-town+ 1)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

(define-floor-type "town-building-toproof-border" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 712
  :gfx-sym (tile-paint-value +tilefile-town+ 7)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

(define-floor-type "town-building-botroof" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 713
  :gfx-sym (tile-paint-value +tilefile-town+ 2)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

(define-floor-type "town-building-roof" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 714
  :gfx-sym (tile-paint-value +tilefile-town+ 11)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

(define-floor-type "town-building-wall" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 715
  :gfx-sym (tile-paint-value +tilefile-town+ 9)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

(define-floor-type "town-building-wall-plank" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 716
  :gfx-sym (tile-paint-value +tilefile-town+ 8)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))


(define-floor-type "town-building-wall-window" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 717
  :gfx-sym (tile-paint-value +tilefile-town+ 6)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))


(define-floor-type "town-building-door-top" "town building"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 718
  :gfx-sym (tile-paint-value +tilefile-town+ 0)
  :flags #.(logior +floor-flag-wall+
		   +floor-flag-permanent+))

;; this is really a door, not really used
(define-floor-type "town-building-door-bottom" "shop door"
  :text-sym (text-paint-value +term-white+ #\#)
  :numeric-id 719
  :gfx-sym (tile-paint-value +tilefile-town+ 10)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-creatures+))
;;  :flags #.(logior +floor-flag-wall+ +floor-flag-permanent+))


(define-floor-type "town-floor" "ground"
  :numeric-id 721
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value +tilefile-town+ 15))

(define-floor-type "town-floor-with-tree" "ground with bushes"
  :numeric-id 722
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value +tilefile-town+ 16))

(define-floor-type "town-floor-with-fountain" "ground with fountain"
  :numeric-id 723
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value +tilefile-town+ 17))

;;; === blood-sport

(define-floor-type "1-blood-normal-floor" "normal floor"
  :numeric-id 802
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 1))

(define-floor-type "2-blood-normal-floor" "normal floor"
  :numeric-id 803
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 2))

(define-floor-type "3-blood-normal-floor" "normal floor"
  :numeric-id 804
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 3))

(define-floor-type "4-blood-normal-floor" "normal floor"
  :numeric-id 805
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-use-light-effect+
		   +floor-flag-allow-items+ +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 4))

(define-floor-type "1-blood-room-floor" "room floor"
  :numeric-id 812
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-items+
		   +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 11))


(define-floor-type "2-blood-room-floor" "room floor"
  :numeric-id 813
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-items+
		   +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 12))


(define-floor-type "3-blood-room-floor" "room floor"
  :numeric-id 814
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-items+
		   +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 13))

(define-floor-type "4-blood-room-floor" "room floor"
  :numeric-id 815
  :text-sym (text-paint-value +term-white+ #\.)
  :flags #.(logior +floor-flag-floor+ +floor-flag-allow-items+
		   +floor-flag-allow-creatures+)
  :gfx-sym (tile-paint-value 42 14))
