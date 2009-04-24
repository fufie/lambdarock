;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/floors.lisp - floor-types for evomyth variant
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

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
