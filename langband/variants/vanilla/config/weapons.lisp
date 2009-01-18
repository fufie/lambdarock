;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/weapons.lisp - weapons for vanilla variant
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-object-kind "broken-dagger" "& broken dagger~"
  :numeric-id 30
  :gfx-sym (tile-paint-value +tilefile-weapons+ 1)
  :text-sym (text-paint-value +term-l-dark+ #\|)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 5
  :cost 1
  :flags '(<show-modififers>)
  :sort-value 3201
  :the-kind '<sword>
  :damage "1d1 (-2,-4)")

(define-object-kind "bastard-sword" "& bastard sword~"
  :numeric-id 31
  :gfx-sym (tile-paint-value +tilefile-weapons+ 14)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 140
  :cost 350
  :flags '(<show-modififers>)
  :sort-value 3221
  :the-kind '<sword>
  :damage "3d4")

(define-object-kind "scimitar" "& scimitar~"
  :numeric-id 32
  :gfx-sym (tile-paint-value +tilefile-weapons+ 11)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 130
  :cost 250
  :flags '(<show-modififers>)
  :sort-value 3218
  :the-kind '<sword>
  :damage "2d5")

(define-object-kind "tulwar" "& tulwar~"
  :numeric-id 33
  :gfx-sym (tile-paint-value +tilefile-weapons+ 9)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 100
  :cost 200
  :flags '(<show-modififers>)
  :sort-value 3215
  :the-kind '<sword>
  :damage "2d4")

(define-object-kind "broad-sword" "& broad sword~"
  :numeric-id 34
  :gfx-sym (tile-paint-value +tilefile-weapons+ 12)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 10
  :locations '((10 . 1) (15 . 1))
  :weight 150
  :cost 255
  :flags '(<show-modififers>)
  :sort-value 3216
  :the-kind '<sword>
  :damage "2d5")

(define-object-kind "short-sword" "& short sword~"
  :numeric-id 35
  :gfx-sym (tile-paint-value +tilefile-weapons+ 8)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 80
  :cost 90
  :flags '(<show-modififers>)
  :sort-value 3210
  :the-kind '<sword>
  :damage "1d7")

(define-object-kind "chaos-blade" "& blade~ of chaos"
  :numeric-id 36
  :gfx-sym (tile-paint-value +tilefile-weapons+ 17)
  :text-sym (text-paint-value +term-violet+ #\|)
  :power-lvl 70
  :locations '((70 . 8))
  :weight 180
  :cost 4000
  :flags '(<show-modififers>)
  :sort-value 3230
  :the-kind '<sword>
  :damage "6d5"
  :resists '(<chaos> <confusion>))

(define-object-kind "two-h-sword" "& two-handed sword~"
  :numeric-id 37
  :gfx-sym (tile-paint-value +tilefile-weapons+ 15)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 30
  :locations '((30 . 1) (40 . 1))
  :weight 200
  :cost 775
  :flags '(<show-modififers>)
  :sort-value 3225
  :the-kind '<sword>
  :damage "3d6")

(define-object-kind "dirk" "& dirk~"
  :numeric-id 38
  :gfx-sym (tile-paint-value +tilefile-weapons+ 4)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 3
  :locations '((3 . 1))
  :weight 30
  :cost 25
  :flags '(<show-modififers>)
  :sort-value 3205
  :the-kind '<sword>
  :damage "1d5")

(define-object-kind "cutlass" "& cutlass~"
  :numeric-id 39
  :gfx-sym (tile-paint-value +tilefile-weapons+ 10)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 110
  :cost 85
  :flags '(<show-modififers>)
  :sort-value 3212
  :the-kind '<sword>
  :damage "1d7")

(define-object-kind "exec-sword" "& executioner's sword~"
  :numeric-id 40
  :gfx-sym (tile-paint-value +tilefile-weapons+ 15)
  :text-sym (text-paint-value +term-red+ #\|)
  :power-lvl 40
  :locations '((40 . 1))
  :weight 260
  :cost 850
  :flags '(<show-modififers>)
  :sort-value 3228
  :the-kind '<sword>
  :damage "4d5")

(define-object-kind "katana" "& katana~"
  :numeric-id 41
  :gfx-sym (tile-paint-value +tilefile-weapons+ 14)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 120
  :cost 400
  :flags '(<show-modififers>)
  :sort-value 3220
  :the-kind '<sword>
  :damage "3d4")

(define-object-kind "long-sword" "& long sword~"
  :numeric-id 42
  :gfx-sym (tile-paint-value +tilefile-weapons+ 13)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 10
  :locations '((10 . 1) (20 . 1))
  :weight 130
  :cost 300
  :flags '(<show-modififers>)
  :sort-value 3217
  :the-kind '<sword>
  :damage "2d5")

(define-object-kind "dagger" "& dagger~"
  :numeric-id 43
  :gfx-sym (tile-paint-value +tilefile-weapons+ 3)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 0
  :locations '((0 . 1) (5 . 1) (10 . 1) (20 . 1))
  :weight 12
  :cost 10
  :flags '(<show-modififers>)
  :sort-value 3204
  :the-kind '<sword>
  :damage "1d4")

(define-object-kind "rapier" "& rapier~"
  :numeric-id 44
  :gfx-sym (tile-paint-value +tilefile-weapons+ 5)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 40
  :cost 42
  :flags '(<show-modififers>)
  :sort-value 3207
  :the-kind '<sword>
  :damage "1d6")

(define-object-kind "sabre" "& sabre~"
  :numeric-id 45
  :gfx-sym (tile-paint-value +tilefile-weapons+ 7)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 50
  :flags '(<show-modififers>)
  :sort-value 3211
  :the-kind '<sword>
  :damage "1d7")

(define-object-kind "small-sword" "& small sword~"
  :numeric-id 46
  :gfx-sym (tile-paint-value +tilefile-weapons+ 6)
  :text-sym (text-paint-value +term-l-white+ #\|)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 75
  :cost 48
  :flags '(<show-modififers>)
  :sort-value 3208
  :the-kind '<sword>
  :damage "1d6")

(define-object-kind "broken-sword" "& broken sword~"
  :numeric-id 47
  :gfx-sym (tile-paint-value +tilefile-weapons+ 2)
  :text-sym (text-paint-value +term-l-dark+ #\|)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 30
  :cost 2
  :flags '(<show-modififers>)
  :sort-value 3202
  :the-kind '<sword>
  :damage "1d2 (-2,-4)")

;; == end swords

(define-object-kind "ball-and-chain" "& ball-and-chain~"
  :numeric-id 48
  :gfx-sym (tile-paint-value +tilefile-weapons+ 25)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 150
  :cost 200
  :flags '(<show-modififers>)
  :sort-value 3006
  :the-kind '<hafted>
  :damage "2d4")

(define-object-kind "whip" "& whip~"
  :numeric-id 49
  :gfx-sym (tile-paint-value +tilefile-weapons+ 18)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 3
  :locations '((3 . 1))
  :weight 30
  :cost 30
  :flags '(<show-modififers>)
  :sort-value 3002
  :the-kind '<hafted>
  :damage "1d6")

(define-object-kind "flail" "& flail~"
  :numeric-id 50
  :gfx-sym (tile-paint-value +tilefile-weapons+ 22)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 150
  :cost 353
  :flags '(<show-modififers>)
  :sort-value 3013
  :the-kind '<hafted>
  :damage "2d6")

(define-object-kind "two-h-flail" "& two-handed flail~"
  :numeric-id 51
  :gfx-sym (tile-paint-value +tilefile-weapons+ 26)
  :text-sym (text-paint-value +term-yellow+ #\\)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 280
  :cost 590
  :flags '(<show-modififers>)
  :sort-value 3018
  :the-kind '<hafted>
  :damage "3d6")

(define-object-kind "morning-star" "& morning star~"
  :numeric-id 52
  :gfx-sym (tile-paint-value +tilefile-weapons+ 23)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 150
  :cost 396
  :flags '(<show-modififers>)
  :sort-value 3012
  :the-kind '<hafted>
  :damage "2d6")

(define-object-kind "mace" "& mace~"
  :numeric-id 53
  :gfx-sym (tile-paint-value +tilefile-weapons+ 20)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 120
  :cost 130
  :flags '(<show-modififers>)
  :sort-value 3005
  :the-kind '<hafted>
  :damage "2d4")

(define-object-kind "quarterstaff" "& quarterstaff~"
  :numeric-id 54
  :gfx-sym (tile-paint-value +tilefile-weapons+ 21)
  :text-sym (text-paint-value +term-l-umber+ #\\)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 150
  :cost 200
  :flags '(<show-modififers>)
  :sort-value 3003
  :the-kind '<hafted>
  :damage "1d9")

(define-object-kind "war-hammer" "& war hammer~"
  :numeric-id 55
  :gfx-sym (tile-paint-value +tilefile-weapons+ 19)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 120
  :cost 225
  :flags '(<show-modififers>)
  :sort-value 3008
  :the-kind '<hafted>
  :damage "3d3")

(define-object-kind "lead-mace" "& lead-filled mace~"
  :numeric-id 56
  :gfx-sym (tile-paint-value +tilefile-weapons+ 24)
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 180
  :cost 502
  :flags '(<show-modififers>)
  :sort-value 3015
  :the-kind '<hafted>
  :damage "3d4")

(define-object-kind "disruption-mace" "& mace~ of disruption"
  :numeric-id 57
  :gfx-sym (tile-paint-value +tilefile-weapons+ 27)
  :text-sym (text-paint-value +term-violet+ #\\)
  :power-lvl 80
  :locations '((80 . 8))
  :weight 400
  :cost 4300
  :flags '(<show-modififers>)
  :sort-value 3020
  :the-kind '<hafted>
  :slays '(<undead>)
  :damage "5d8")

(define-object-kind "lucerne-hammer" "& lucerne hammer~"
  :numeric-id 58
  :gfx-sym (tile-paint-value +tilefile-weapons+ 32)
  :text-sym (text-paint-value +term-l-blue+ #\\)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 120
  :cost 376
  :flags '(<show-modififers>)
  :sort-value 3010
  :the-kind '<hafted>
  :damage "2d5")

;; == end hafted weapons

(define-object-kind "beaked-axe" "& beaked axe~"
  :numeric-id 59
  :gfx-sym (tile-paint-value +tilefile-weapons+ 35)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 180
  :cost 408
  :flags '(<show-modififers>)
  :sort-value 3110
  :the-kind '<pole-arm>
  :damage "2d6")

(define-object-kind "glaive" "& glaive~"
  :numeric-id 60
  :gfx-sym (tile-paint-value +tilefile-weapons+ 37)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 190
  :cost 363
  :flags '(<show-modififers>)
  :sort-value 3113
  :the-kind '<pole-arm>
  :damage "2d6")

(define-object-kind "halberd" "& halberd~"
  :numeric-id 61
  :gfx-sym (tile-paint-value +tilefile-weapons+ 38)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 25
  :locations '((25 . 1))
  :weight 190
  :cost 430
  :flags '(<show-modififers>)
  :sort-value 3115
  :the-kind '<pole-arm>
  :damage "3d5")

(define-object-kind "awl-pike" "& awl-pike~"
  :numeric-id 62
  :gfx-sym (tile-paint-value +tilefile-weapons+ 30)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 160
  :cost 340
  :flags '(<show-modififers>)
  :sort-value 3104
  :the-kind '<pole-arm>
  :damage "1d8")

(define-object-kind "pike" "& pike~"
  :numeric-id 63
  :gfx-sym (tile-paint-value +tilefile-weapons+ 34)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 160
  :cost 358
  :flags '(<show-modififers>)
  :sort-value 3108
  :the-kind '<pole-arm>
  :damage "2d5")

(define-object-kind "spear" "& spear~"
  :numeric-id 64
  :gfx-sym (tile-paint-value +tilefile-weapons+ 28)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 36
  :flags '(<show-modififers>)
  :sort-value 3102
  :the-kind '<pole-arm>
  :damage "1d6")

(define-object-kind "trident" "& trident~"
  :numeric-id 65
  :gfx-sym (tile-paint-value +tilefile-weapons+ 29)
  :text-sym (text-paint-value +term-yellow+ #\/)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 70
  :cost 120
  :flags '(<show-modififers>)
  :sort-value 3105
  :the-kind '<pole-arm>
  :damage "1d8")

(define-object-kind "lance" "& lance~"
  :numeric-id 66
  :gfx-sym (tile-paint-value +tilefile-weapons+ 31)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 300
  :cost 230
  :flags '(<show-modififers>)
  :sort-value 3120
  :the-kind '<pole-arm>
  :damage "2d8")

(define-object-kind "great-axe" "& great axe~"
  :numeric-id 67
  :gfx-sym (tile-paint-value +tilefile-weapons+ 40)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 40
  :locations '((40 . 1))
  :weight 230
  :cost 500
  :flags '(<show-modififers>)
  :sort-value 3125
  :the-kind '<pole-arm>
  :damage "4d4")

(define-object-kind "battle-axe" "& battle axe~"
  :numeric-id 68
  :gfx-sym (tile-paint-value +tilefile-weapons+ 33)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 170
  :cost 334
  :flags '(<show-modififers>)
  :sort-value 3122
  :the-kind '<pole-arm>
  :damage "2d8")

(define-object-kind "lochaber-axe" "& lochaber axe~"
  :numeric-id 69
  :gfx-sym (tile-paint-value +tilefile-weapons+ 39)
  :text-sym (text-paint-value +term-l-dark+ #\/)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 250
  :cost 750
  :flags '(<show-modififers>)
  :sort-value 3128
  :the-kind '<pole-arm>
  :damage "3d8")

(define-object-kind "broad-axe" "& broad axe~"
  :numeric-id 70
  :gfx-sym (tile-paint-value +tilefile-weapons+ 36)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 160
  :cost 304
  :flags '(<show-modififers>)
  :sort-value 3111
  :the-kind '<pole-arm>
  :damage "2d6")

(define-object-kind "scythe" "& scythe~"
  :numeric-id 71
  :gfx-sym (tile-paint-value +tilefile-weapons+ 41)
  :text-sym (text-paint-value +term-slate+ #\/)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 250
  :cost 800
  :flags '(<show-modififers>)
  :sort-value 3117
  :the-kind '<pole-arm>
  :damage "5d3")

(define-object-kind "scythe-slicing" "& scythe~ of slicing"
  :numeric-id 72
  :gfx-sym (tile-paint-value +tilefile-weapons+ 42)
  :text-sym (text-paint-value +term-red+ #\/)
  :power-lvl 60
  :locations '((60 . 4))
  :weight 250
  :cost 3500
  :flags '(<show-modififers>)
  :sort-value 3130
  :the-kind '<pole-arm>
  :damage "8d4")

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
  :the-kind '<bow>)
  

(define-object-kind "long-bow" "& long bow~"
  :numeric-id 74
  :gfx-sym (tile-paint-value +tilefile-weapons+ 44)
  :text-sym (text-paint-value +term-l-umber+ #\})
  :power-lvl 10
  :locations '((10 . 1))
  :weight 40
  :cost 120
  :multiplier 2
  :flags '(<show-modififers>)
  :sort-value 2813
  :the-kind '<bow>)

(define-object-kind "light-xbow" "& light crossbow~"
  :numeric-id 75
  :gfx-sym (tile-paint-value +tilefile-weapons+ 45)
  :text-sym (text-paint-value +term-slate+ #\})
  :power-lvl 15
  :locations '((15 . 1))
  :weight 110
  :cost 140
  :multiplier 3
  :flags '(<show-modififers>)
  :sort-value 2823
  :the-kind '<bow>)

(define-object-kind "heavy-xbow" "& heavy crossbow~"
  :numeric-id 76
  :gfx-sym (tile-paint-value +tilefile-weapons+ 46)
  :text-sym (text-paint-value +term-slate+ #\})
  :power-lvl 30
  :locations '((30 . 1))
  :weight 200
  :cost 300
  :multiplier 4
  :flags '(<show-modififers>)
  :sort-value 2824
  :the-kind '<bow>)

(define-object-kind "sling" "& sling~"
  :numeric-id 77
  :gfx-sym (tile-paint-value +tilefile-weapons+ 47)
  :text-sym (text-paint-value +term-umber+ #\})
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 5
  :multiplier 2
  :flags '(<show-modififers>)
  :sort-value 2802
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
  :damage "d4")

(define-object-kind "seeker-arrow" "& seeker arrow~"
  :numeric-id 79
  :gfx-sym (tile-paint-value +tilefile-weapons+ 50)
  :text-sym (text-paint-value +term-l-green+ #\{)
  :power-lvl 55
  :locations '((55 . 2))
  :weight 2
  :cost 20
  :flags '(<show-modififers>)
  :visual-effect "arrow"
  :sort-value 2602
  :the-kind '<ammo>
  :vulnerabilities '(<fire> <plasma> <acid>)
  :damage "4d4")

(define-object-kind "bolt" "& bolt~"
  :numeric-id 80
  :gfx-sym (tile-paint-value +tilefile-weapons+ 51)
  :text-sym (text-paint-value +term-slate+ #\{)
  :power-lvl 3
  :locations '((3 . 1) (25 . 1))
  :weight 3
  :cost 2
  :flags '(<show-modififers>)
  :visual-effect "bolt"
  :sort-value 2701
  :the-kind '<ammo>
  :vulnerabilities '(<acid>)
  :damage "1d5")

(define-object-kind "seeker-bolt" "& seeker bolt~"
  :numeric-id 81
  :gfx-sym (tile-paint-value +tilefile-weapons+ 52)
  :text-sym (text-paint-value +term-l-blue+ #\{)
  :power-lvl 65
  :locations '((65 . 4))
  :weight 3
  :cost 25
  :flags '(<show-modififers>)
  :visual-effect "bolt"
  :sort-value 2702
  :the-kind '<ammo>
  :vulnerabilities '(<acid>)
  :damage "4d5")

(define-object-kind "round-pebble" "& rounded pebble~"
  :numeric-id 82
  :gfx-sym (tile-paint-value +tilefile-weapons+ 53)
  :text-sym (text-paint-value +term-slate+ #\{)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 4
  :cost 1
  :flags '(<show-modififers>)
  :visual-effect "stone"
  :sort-value 2500
  :the-kind '<ammo>
  :damage "1d2")

(define-object-kind "iron-shot" "& iron shot~"
  :numeric-id 83
  :gfx-sym (tile-paint-value +tilefile-weapons+ 54)
  :text-sym (text-paint-value +term-slate+ #\{)
  :power-lvl 3
  :locations '((3 . 1))
  :weight 5
  :cost 2
  :flags '(<show-modififers>)
  :visual-effect "stone"
  :sort-value 2501
  :the-kind '<ammo>
  :damage "1d3")

#||
(define-object-kind "mighty-hammer" "& mighty hammer~"
  :numeric-id 498
  :text-sym (text-paint-value +term-l-dark+ #\\)
  :power-lvl 15
  :weight 200
  :cost 1000
  :flags '(<instant-artifact> <show-modififers>)
  :sort-value 3050
  :the-kind '<hafted>
  :damage "3d9")
||#
