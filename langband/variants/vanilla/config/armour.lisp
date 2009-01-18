;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/armour.lisp - armour for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; == footwear

(define-object-kind "soft-leather-boots" "& pair~ of soft leather boots"
  :numeric-id 91
  :gfx-sym (tile-paint-value +tilefile-armour+ 20)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :power-lvl 3
  :locations '((3 . 1))
  :weight 20
  :cost 7
  :sort-value 3302
  :the-kind '<boots>
  :armour-rating 2
  :damage 1) 

(define-object-kind "hard-leather-boots" "& pair~ of hard leather boots"
  :numeric-id 92
  :gfx-sym (tile-paint-value +tilefile-armour+ 21)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :power-lvl 5
  :locations '((5 . 1))
  :weight 40
  :cost 12
  :sort-value 3303
  :the-kind '<boots>
  :armour-rating 3
  :damage 1) 

(define-object-kind "metal-boots" "& pair~ of metal shod boots"
  :numeric-id 93
  :gfx-sym (tile-paint-value +tilefile-armour+ 22)
  :text-sym (text-paint-value +term-slate+ #\])
  :power-lvl 20
  :locations '((20 . 1))
  :weight 80
  :cost 50
  :sort-value 3306
  :the-kind '<boots>
  :armour-rating 6
  :damage 1) 

;;; == headwear

(define-object-kind "hard-leather-cap" "& hard leather cap~"
  :numeric-id 94
  :gfx-sym (tile-paint-value +tilefile-armour+ 70)
  :text-sym (text-paint-value +term-umber+ #\])
  :power-lvl 3
  :locations '((3 . 1))
  :weight 15
  :cost 12
  :sort-value 3502
  :the-kind '<helmet>
  :armour-rating 2)

(define-object-kind "metal-cap" "& metal cap~"
  :numeric-id 95
  :gfx-sym (tile-paint-value +tilefile-armour+ 11)
  :text-sym (text-paint-value +term-slate+ #\])
  :power-lvl 10
  :locations '((10 . 1))
  :weight 20
  :cost 30
  :sort-value 3503
  :the-kind '<helmet>
  :armour-rating 3
  :damage 1) 

(define-object-kind "iron-helm" "& iron helm~"
  :numeric-id 96
  :gfx-sym (tile-paint-value +tilefile-armour+ 12)
  :text-sym (text-paint-value +term-slate+ #\])
  :power-lvl 20
  :locations '((20 . 1))
  :weight 75
  :cost 75
  :sort-value 3505
  :the-kind '<helmet>
  :armour-rating 5
  :damage "1d3") 

(define-object-kind "steel-helm" "& steel helm~"
  :numeric-id 97
  :gfx-sym (tile-paint-value +tilefile-armour+ 13)
  :text-sym (text-paint-value +term-l-white+ #\])
  :power-lvl 40
  :locations '((40 . 1))
  :weight 60
  :cost 200
  :sort-value 3506
  :the-kind '<helmet>
  :armour-rating 6
  :damage "1d3") 

(define-object-kind "iron-crown" "& iron crown~"
  :numeric-id 98
  :gfx-sym (tile-paint-value +tilefile-armour+ 15)
  :text-sym (text-paint-value +term-slate+ #\])
  :power-lvl 45
  :locations '((45 . 1))
  :weight 20
  :cost 500
  :sort-value 3610
  :the-kind '<crown>
  :damage 1) 

(define-object-kind "golden-crown" "& golden crown~"
  :numeric-id 99
  :gfx-sym (tile-paint-value +tilefile-armour+ 16)
  :text-sym (text-paint-value +term-yellow+ #\])
  :power-lvl 45
  :locations '((45 . 1))
  :weight 30
  :cost 1000
  :sort-value 3611
  :the-kind '<crown>
  :damage 1
  :ignores '(<acid>))

(define-object-kind "jewel-crown" "& jewel encrusted crown~"
  :numeric-id 100
  :gfx-sym (tile-paint-value +tilefile-armour+ 17)
  :text-sym (text-paint-value +term-violet+ #\])
  :power-lvl 50
  :locations '((50 . 1))
  :weight 40
  :cost 2000
  :sort-value 3612
  :the-kind '<crown>
  :damage 1
  :ignores '(<acid>))

(define-object-kind "massive-iron-crown" "& massive iron crown~"
  :numeric-id 499
  :text-sym (text-paint-value +term-l-dark+ #\])
  :power-lvl 44
  :weight 20
  :cost 1000
  :flags '(<instant-artifact>)
  :sort-value 3650
  :the-kind '<crown>
  :damage 1) 

;;; == bodywear

(define-object-kind "robe" "& robe~"
  :numeric-id 101
  :gfx-sym (tile-paint-value +tilefile-armour+ 34)
  :text-sym (text-paint-value +term-blue+ #\()
  :power-lvl 1
  :locations '((1 . 1) (50 . 1))
  :weight 20
  :cost 4
  :sort-value 3902
  :the-kind '<soft-body-armour>
  :armour-rating 2)

(define-object-kind "filthy-rag" "& filthy rag~"
  :numeric-id 102
  :gfx-sym (tile-paint-value +tilefile-armour+ 33)
  :text-sym (text-paint-value +term-l-dark+ #\()
  :power-lvl 0
  :locations '((0 . 1))
  :weight 20
  :cost 1
  :sort-value 3901
  :the-kind '<soft-body-armour>
  :armour-rating 1
  :armour-modifier -1)

(define-object-kind "soft-leather-armour" "soft leather armour~"
  :numeric-id 103
  :gfx-sym (tile-paint-value +tilefile-armour+ 38)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :power-lvl 3
  :locations '((3 . 1))
  :weight 80
  :cost 18
  :sort-value 3904
  :the-kind '<soft-body-armour>
  :armour-rating 4)

(define-object-kind "soft-studded-leather" "soft studded leather~"
  :numeric-id 104
  :gfx-sym (tile-paint-value +tilefile-armour+ 39)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :power-lvl 3
  :locations '((3 . 1))
  :weight 90
  :cost 35
  :sort-value 3905
  :the-kind '<soft-body-armour>
  :armour-rating 5
  :damage 1) 

(define-object-kind "hard-leather-armour" "hard leather armour~"
  :numeric-id 105
  :gfx-sym (tile-paint-value +tilefile-armour+ 40)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :power-lvl 5
  :locations '((5 . 1))
  :weight 100
  :cost 150
  :sort-value 3906
  :the-kind '<soft-body-armour>
  :armour-rating 6
  :damage "1d1 (-1,0)") 

(define-object-kind "hard-studded-leather" "hard studded leather~"
  :numeric-id 106
  :gfx-sym (tile-paint-value +tilefile-armour+ 41)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :power-lvl 10
  :locations '((10 . 1))
  :weight 110
  :cost 200
  :sort-value 3907
  :the-kind '<soft-body-armour>
  :armour-rating 7
  :damage "1d2 (-1,0)")

(define-object-kind "leather-scale-mail" "leather scale mail~"
  :numeric-id 107
  :gfx-sym (tile-paint-value +tilefile-armour+ 42)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :power-lvl 15
  :locations '((15 . 1))
  :weight 140
  :cost 450
  :sort-value 3911
  :the-kind '<soft-body-armour>
  :armour-rating 11
  :damage "1d1 (-1,0)") 

(define-object-kind "metal-scale-mail" "metal scale mail~"
  :numeric-id 108
  :gfx-sym (tile-paint-value +tilefile-armour+ 43)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 25
  :locations '((25 . 1))
  :weight 250
  :cost 550
  :sort-value 4003
  :the-kind '<hard-body-armour>
  :armour-rating 13
  :damage "1d4 (-2,0)")

(define-object-kind "chain-mail" "chain mail~"
  :numeric-id 109
  :gfx-sym (tile-paint-value +tilefile-armour+ 45)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 25
  :locations '((25 . 1))
  :weight 220
  :cost 750
  :sort-value 4004
  :the-kind '<hard-body-armour>
  :armour-rating 14
  :damage "1d4 (-2,0)")

(define-object-kind "rusty-chain-mail" "rusty chain mail~"
  :numeric-id 110
  :gfx-sym (tile-paint-value +tilefile-armour+ 44)
  :text-sym (text-paint-value +term-red+ #\[)
  :power-lvl 25
  :locations '((25 . 1))
  :weight 200
  :cost 550
  :sort-value 4001
  :the-kind '<hard-body-armour>
  :armour-rating 14
  :armour-modifier -8
  :damage "1d4 (-5,0)")

(define-object-kind "augm-chain-mail" "augmented chain mail~"
  :numeric-id 111
  :gfx-sym (tile-paint-value +tilefile-armour+ 47)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 270
  :cost 900
  :sort-value 4006
  :the-kind '<hard-body-armour>
  :armour-rating 16
  :damage "1d4 (-2,0)")

(define-object-kind "bar-chain-mail" "bar chain mail~"
  :numeric-id 112
  :gfx-sym (tile-paint-value +tilefile-armour+ 48)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 35
  :locations '((35 . 1))
  :weight 280
  :cost 950
  :sort-value 4008
  :the-kind '<hard-body-armour>
  :armour-rating 18
  :damage "1d4 (-2,0)")

(define-object-kind "metal-brigandine" "metal brigandine armour~"
  :numeric-id 113
  :gfx-sym (tile-paint-value +tilefile-armour+ 49)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 35
  :locations '((35 . 1))
  :weight 290
  :cost 1100
  :sort-value 4009
  :the-kind '<hard-body-armour>
  :armour-rating 19
  :damage "1d4 (-3,0)")

(define-object-kind "partial-plate" "partial plate armour~"
  :numeric-id 114
  :gfx-sym (tile-paint-value +tilefile-armour+ 50)
  :text-sym (text-paint-value +term-l-white+ #\[)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 260
  :cost 1200
  :sort-value 4012
  :the-kind '<hard-body-armour>
  :armour-rating 22
  :damage "1d6 (-3,0)")

(define-object-kind "metal-lamellar" "metal lamellar armour~"
  :numeric-id 115
  :gfx-sym (tile-paint-value +tilefile-armour+ 51)
  :text-sym (text-paint-value +term-l-white+ #\[)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 340
  :cost 1250
  :sort-value 4013
  :the-kind '<hard-body-armour>
  :armour-rating 23
  :damage "1d6 (-3,0)")

(define-object-kind "full-plate" "full plate armour~"
  :numeric-id 116
  :gfx-sym (tile-paint-value +tilefile-armour+ 52)
  :text-sym (text-paint-value +term-l-white+ #\[)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 380
  :cost 1350
  :sort-value 4015
  :the-kind '<hard-body-armour>
  :armour-rating 25
  :damage "2d4 (-3,0)")

(define-object-kind "ribbed-plate" "ribbed plate armour~"
  :numeric-id 117
  :gfx-sym (tile-paint-value +tilefile-armour+ 53)
  :text-sym (text-paint-value +term-l-white+ #\[)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 380
  :cost 1500
  :sort-value 4018
  :the-kind '<hard-body-armour>
  :armour-rating 28
  :damage "2d4 (-3,0)")


(define-object-kind "adamantite-plate" "adamantite plate mail~"
  :numeric-id 118
  :gfx-sym (tile-paint-value +tilefile-armour+ 56)
  :text-sym (text-paint-value +term-l-green+ #\[)
  :power-lvl 75
  :locations '((75 . 8))
  :weight 420
  :cost 20000
  :sort-value 4030
  :the-kind '<hard-body-armour>
  :armour-rating 40
  :damage "2d4 (-4,0)"
  :ignores '(<acid>))

(define-object-kind "mithril-plate" "mithril plate mail~"
  :numeric-id 119
  :gfx-sym (tile-paint-value +tilefile-armour+ 54)
  :text-sym (text-paint-value +term-l-blue+ #\[)
  :power-lvl 60
  :locations '((60 . 4))
  :weight 300
  :cost 15000
  :sort-value 4025
  :the-kind '<hard-body-armour>
  :armour-rating 35
  :damage "2d4 (-3,0)"
  :ignores '(<acid>))

(define-object-kind "mithril-chain-mail" "mithril chain mail~"
  :numeric-id 120
  :gfx-sym (tile-paint-value +tilefile-armour+ 55)
  :text-sym (text-paint-value +term-l-blue+ #\[)
  :power-lvl 55
  :locations '((55 . 4))
  :weight 150
  :cost 7000
  :sort-value 4020
  :the-kind '<hard-body-armour>
  :armour-rating 28
  :damage "1d4 (-1,0)"
  :ignores '(<acid>))

(define-object-kind "double-chain-mail" "double chain mail~"
  :numeric-id 121
  :gfx-sym (tile-paint-value +tilefile-armour+ 46)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 250
  :cost 850
  :sort-value 4007
  :the-kind '<hard-body-armour>
  :armour-rating 16
  :damage "1d4 (-2,0)")

;;; == handwear

(define-object-kind "cloak" "& cloak~"
  :numeric-id 123
  :gfx-sym (tile-paint-value +tilefile-armour+ 0)
  :text-sym (text-paint-value +term-green+ #\()
  :power-lvl 1
  :locations '((1 . 1) (20 . 1))
  :weight 10
  :cost 3
  :sort-value 3801
  :the-kind '<cloak>
  :armour-rating 1)

(define-object-kind "shadow-cloak" "& shadow cloak~"
  :numeric-id 124
  :gfx-sym (tile-paint-value +tilefile-armour+ 1)
  :text-sym (text-paint-value +term-l-dark+ #\()
  :power-lvl 60
  :locations '((60 . 4))
  :weight 5
  :cost 4000
  :sort-value 3806
  :the-kind '<cloak>
  :armour-rating 6
  :armour-modifier 4)

(define-object-kind "leather-gloves" "& set~ of leather gloves"
  :numeric-id 125
  :gfx-sym (tile-paint-value +tilefile-armour+ 23)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :power-lvl 1
  :locations '((1 . 1))
  :weight 5
  :cost 3
  :sort-value 3401
  :the-kind '<gloves>
  :armour-rating 1)

(define-object-kind "gauntlets" "& set~ of gauntlets"
  :numeric-id 126
  :gfx-sym (tile-paint-value +tilefile-armour+ 26)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :power-lvl 10
  :locations '((10 . 1))
  :weight 25
  :cost 35
  :sort-value 3402
  :the-kind '<gloves>
  :armour-rating 2
  :damage 1) 

(define-object-kind "cesti" "& set~ of cesti"
  :numeric-id 127
  :gfx-sym (tile-paint-value +tilefile-armour+ 27)
  :text-sym (text-paint-value +term-l-white+ #\])
  :power-lvl 50
  :locations '((50 . 1))
  :weight 40
  :cost 100
  :sort-value 3405
  :the-kind '<gloves>
  :armour-rating 5
  :damage 1) 

;;; == shieldwear

(define-object-kind "small-leather-shield" "& small leather shield~"
  :numeric-id 128
  :gfx-sym (tile-paint-value +tilefile-armour+ 90)
  :text-sym (text-paint-value +term-l-umber+ #\))
  :power-lvl 3
  :locations '((3 . 1))
  :weight 50
  :cost 30
  :sort-value 3702
  :the-kind '<shield>
  :armour-rating 2
  :damage 1) 

(define-object-kind "large-leather-shield" "& large leather shield~"
  :numeric-id 129
  :gfx-sym (tile-paint-value +tilefile-armour+ 29)
  :text-sym (text-paint-value +term-l-umber+ #\))
  :power-lvl 15
  :locations '((15 . 1))
  :weight 100
  :cost 120
  :sort-value 3704
  :the-kind '<shield>
  :armour-rating 4
  :damage "1d2") 

(define-object-kind "small-metal-shield" "& small metal shield~"
  :numeric-id 130
  :gfx-sym (tile-paint-value +tilefile-armour+ 93)
  :text-sym (text-paint-value +term-slate+ #\))
  :power-lvl 10
  :locations '((10 . 1))
  :weight 65
  :cost 50
  :sort-value 3703
  :the-kind '<shield>
  :armour-rating 3
  :damage "1d2") 

(define-object-kind "large-metal-shield" "& large metal shield~"
  :numeric-id 131
  :gfx-sym (tile-paint-value +tilefile-armour+ 89)
  :text-sym (text-paint-value +term-slate+ #\))
  :power-lvl 30
  :locations '((30 . 1))
  :weight 120
  :cost 200
  :sort-value 3705
  :the-kind '<shield>
  :armour-rating 5
  :damage "1d3") 

(define-object-kind "deflection-shield" "& shield~ of deflection"
  :numeric-id 122
  :gfx-sym (tile-paint-value +tilefile-armour+ 32)
  :text-sym (text-paint-value +term-l-blue+ #\[)
  :power-lvl 70
  :locations '((70 . 8))
  :weight 100
  :cost 10000
  :sort-value 3710
  :the-kind '<shield>
  :armour-rating 10
  :armour-modifier 10
  :damage 1
  :ignores '(<acid>))

;;; === dragonwear

(define-object-kind "dsm-black" "black dragon scale mail~"
  :numeric-id 400
  :gfx-sym (tile-paint-value +tilefile-armour+ 59)
  :text-sym (text-paint-value +term-slate+ #\[)
  :power-lvl 60
  :locations '((60 . 8))
  :weight 200
  :cost 30000
  :flags '(<activation>)
  :sort-value 4101
  :the-kind '<dsm-armour>
  :damage "2d4 (-2,0)"
  :armour-rating 30
  :armour-modifier 10
  :resists '(<acid>)
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "dsm-blue" "blue dragon scale mail~"
  :numeric-id 401
  :gfx-sym (tile-paint-value +tilefile-armour+ 57)
  :text-sym (text-paint-value +term-blue+ #\[)
  :power-lvl 40
  :locations '((40 . 8))
  :weight 200
  :cost 35000
  :flags '(<activation>)
  :sort-value 4102
  :the-kind '<dsm-armour>
  :damage "2d4 (-2,0)"
  :armour-rating 30
  :armour-modifier 10
  :resists '(<electricity>)
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "dsm-white" "white dragon scale mail~"
  :numeric-id 402
  :gfx-sym (tile-paint-value +tilefile-armour+ 58)
  :text-sym (text-paint-value +term-white+ #\[)
  :power-lvl 50
  :locations '((50 . 8))
  :weight 200
  :cost 40000
  :flags '(<activation>)
  :sort-value 4103
  :the-kind '<dsm-armour>
  :damage "2d4 (-2,0)"
  :armour-rating 30
  :armour-modifier 10
  :resists '(<cold>)
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "dsm-red" "red dragon scale mail~"
  :numeric-id 403
  :gfx-sym (tile-paint-value +tilefile-armour+ 60)
  :text-sym (text-paint-value +term-red+ #\[)
  :power-lvl 80
  :locations '((80 . 8))
  :weight 200
  :cost 100000
  :flags '(<activation>)
  :sort-value 4104
  :the-kind '<dsm-armour>
  :damage "2d4 (-2,0)"
  :armour-rating 30
  :armour-modifier 10
  :resists '(<fire>)
  :ignores '(<cold> <fire> <electricity> <acid>))

(define-object-kind "dsm-green" "green dragon scale mail~"
  :numeric-id 404
  :gfx-sym (tile-paint-value +tilefile-armour+ 61)
  :text-sym (text-paint-value +term-green+ #\[)
  :power-lvl 70
  :locations '((70 . 8))
  :weight 200
  :cost 80000
  :flags '(<activation>)
  :sort-value 4105
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<poison>))

(define-object-kind "dsm-mh" "multi-hued dragon scale mail~"
  :numeric-id 405
  :gfx-sym (tile-paint-value +tilefile-armour+ 68)
  :text-sym (text-paint-value +term-violet+ #\[)
  :power-lvl 100
  :locations '((100 . 16))
  :weight 200
  :cost 150000
  :flags '(<activation>)
  :sort-value 4106
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<poison> <cold> <fire> <electricity> <acid>))

(define-object-kind "dsm-shining" "shining dragon scale mail~"
  :numeric-id 406
  :gfx-sym (tile-paint-value +tilefile-armour+ 64)
  :text-sym (text-paint-value +term-orange+ #\[)
  :power-lvl 65
  :locations '((65 . 16))
  :weight 200
  :cost 60000
  :flags '(<activation>)
  :sort-value 4110
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<darkness> <light>))

(define-object-kind "dsm-law" "law dragon scale mail~"
  :numeric-id 407
  :gfx-sym (tile-paint-value +tilefile-armour+ 66)
  :text-sym (text-paint-value +term-l-blue+ #\[)
  :power-lvl 80
  :locations '((80 . 16))
  :weight 200
  :cost 80000
  :flags '(<activation>)
  :sort-value 4112
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<shards> <sound>))

(define-object-kind "dsm-bronze" "bronze dragon scale mail~"
  :numeric-id 408
  :gfx-sym (tile-paint-value +tilefile-armour+ 62)
  :text-sym (text-paint-value +term-l-umber+ #\[)
  :power-lvl 55
  :locations '((55 . 8))
  :weight 200
  :cost 30000
  :flags '(<activation>)
  :sort-value 4114
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<confusion>))

(define-object-kind "dsm-gold" "gold dragon scale mail~"
  :numeric-id 409
  :gfx-sym (tile-paint-value +tilefile-armour+ 63)
  :text-sym (text-paint-value +term-yellow+ #\[)
  :power-lvl 65
  :locations '((65 . 8))
  :weight 200
  :cost 40000
  :flags '(<activation>)
  :sort-value 4116
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<sound>))

(define-object-kind "dsm-chaos" "chaos dragon scale mail~"
  :numeric-id 410
  :gfx-sym (tile-paint-value +tilefile-armour+ 65)
  :text-sym (text-paint-value +term-violet+ #\[)
  :power-lvl 75
  :locations '((75 . 16))
  :weight 200
  :cost 70000
  :flags '(<activation>)
  :sort-value 4118
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<disenchant> <chaos> <confusion>))

(define-object-kind "dsm-balance" "balance dragon scale mail~"
  :numeric-id 411
  :gfx-sym (tile-paint-value +tilefile-armour+ 67)
  :text-sym (text-paint-value +term-violet+ #\[)
  :power-lvl 90
  :locations '((90 . 16))
  :weight 200
  :cost 100000
  :flags '(<activation>)
  :sort-value 4120
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 30
  :armour-modifier 10
  :damage "2d4 (-2,0)"
  :resists '(<disenchant> <chaos> <shards> <sound> <confusion>))

(define-object-kind "dsm-power" "power dragon scale mail~"
  :numeric-id 412
  :gfx-sym (tile-paint-value +tilefile-armour+ 69)
  :text-sym (text-paint-value +term-violet+ #\[)
  :power-lvl 110
  :locations '((110 . 64))
  :weight 200
  :cost 300000
  :flags '(<activation>)
  :sort-value 4130
  :the-kind '<dsm-armour>
  :ignores '(<cold> <fire> <electricity> <acid>)
  :armour-rating 40
  :armour-modifier 15
  :damage "2d4 (-3,0)"
  :resists '(<chaos> <nether> <nexus> <darkness> <light> <confusion>
	     <poison> <electricity> <cold> <fire> <acid>))
