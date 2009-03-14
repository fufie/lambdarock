;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/armour.lisp - armour for evomyth
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

#||
;;; === Overall armour info and rating

A piece of armour is made of an armour-type and an armour-type has
a rating.

cloth
heavy cloth

leather
fur

studded leather

ring
chain
elven chain

lamellar http://www.regia.org/lamellar.htm
bronze
iron
steel
silver

dragonscale
dragonbone

maybe add:
ebony
mithril
adamant

---

chest/cuirass/body armour is 50%
headgear is 15%
left glove is 5%
right glove is 5%
legs are 15%
boots are 10%

||#


(defconstant +ar-cloth+ 2)
(defconstant +ar-heavy-cloth+ 4)
(defconstant +ar-fur+ 8)
(defconstant +ar-leather+ 10)
(defconstant +ar-studded-leather+ 15)

(defconstant +ab-cloth+ 0)
(defconstant +ab-heavy-cloth+ 0)
(defconstant +ab-fur+ 1)
(defconstant +ab-leather+ 3)
(defconstant +ab-studded-leather+ 5)

(defconstant +ar-ring-mailed+ 18)
(defconstant +ar-chain-mail+ 22)
(defconstant +ar-elven-chain+ 25)

(defconstant +ab-ring-mailed+ 10)
(defconstant +ab-chain-mail+ 13)
(defconstant +ab-elven-chain+ 7)


(defconstant +ar-lamellar+ 25)
(defconstant +ar-bronze+ 24)
(defconstant +ar-iron+ 29)
(defconstant +ar-steel+ 32)
(defconstant +ar-silver+ 35)

(defconstant +ab-lamellar+ 16)
(defconstant +ab-bronze+ 22)
(defconstant +ab-iron+ 25)
(defconstant +ab-steel+ 23)
(defconstant +ab-silver+ 22)


(defconstant +ar-dragonscale+ 45)
(defconstant +ar-dragonbone+ 50)

(defconstant +ab-dragonscale+ 25)
(defconstant +ab-dragonbone+ 30)


;;; Weights and costs have not been checked well
;;; sorting values must be fixed later

;;; == footwear

(define-object-kind "sandals" "& pair~ of sandals"
  :numeric-id 2100
  :text-sym (text-paint-value +term-white+ #\])
  :weight 10
  :cost 4
  :power-lvl 1
  :sort-value 2100
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<boots>
  :armour-rating +ar-cloth+)

(define-object-kind "cloth-shoes" "& pair~ of cloth shoes"
  :numeric-id 2101
  :text-sym (text-paint-value +term-white+ #\])
  :weight 10
  :cost 4
  :power-lvl 1
  :sort-value 2101
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<boots>
  :armour-rating +ar-cloth+)

(define-object-kind "leather-boots" "& pair~ of leather boots"
  :numeric-id 2102
  :gfx-sym (tile-paint-value +tilefile-armour+ 20)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :weight 20
  :cost 7
  :power-lvl 2
  :sort-value 2102
  :skill '<light>
  :bulk +ab-leather+
  :the-kind '<boots>
  :armour-rating +ar-leather+)

(define-object-kind "fur-boots" "& pair~ of fur boots"
  :numeric-id 2103
  :text-sym (text-paint-value +term-slate+ #\])
  :weight 40
  :cost 12
  :power-lvl 3
  :sort-value 2103
  :skill '<light>
  :bulk +ab-fur+
  :the-kind '<boots>
  :armour-rating +ar-fur+)

(define-object-kind "chain-boots" "& pair~ of chain boots"
  :numeric-id 2104
  :gfx-sym (tile-paint-value +tilefile-armour+ 22)
  :text-sym (text-paint-value +term-slate+ #\])
  :weight 80
  :cost 50
  :power-lvl 8
  :sort-value 2104
  :skill '<heavy>
  :bulk +ab-chain-mail+
  :the-kind '<boots>
  :armour-rating +ar-chain-mail+)

(define-object-kind "bronze-boots" "& pair~ of bronze-plated boots"
  :numeric-id 2105
  :gfx-sym (tile-paint-value +tilefile-armour+ 22)
  :text-sym (text-paint-value +term-slate+ #\])
  :weight 80
  :cost 50
  :power-lvl 8
  :sort-value 2105
  :skill '<heavy>
  :bulk +ab-bronze+
  :the-kind '<boots>
  :armour-rating +ar-steel+)

(define-object-kind "steel-boots" "& pair~ of steel-plated boots"
  :numeric-id 2106
  :gfx-sym (tile-paint-value +tilefile-armour+ 22)
  :text-sym (text-paint-value +term-slate+ #\])
  :weight 80
  :cost 50
  :power-lvl 15
  :sort-value 2106
  :skill '<heavy>
  :bulk +ab-steel+
  :the-kind '<boots>
  :armour-rating +ar-steel+)

;;; == headwear

(define-object-kind "hat" "& hat~"
  :numeric-id 2200
  :gfx-sym (tile-paint-value +tilefile-armour+ 71)
  :text-sym (text-paint-value +term-umber+ #\])
  :weight 15
  :cost 12
  :power-lvl 1
  :sort-value 2200
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<headgear>
  :armour-rating +ar-cloth+)

(define-object-kind "leather-cap" "& leather cap~"
  :numeric-id 2201
  :gfx-sym (tile-paint-value +tilefile-armour+ 70)
  :text-sym (text-paint-value +term-umber+ #\])
  :weight 15
  :cost 12
  :power-lvl 2
  :sort-value 2201
  :skill '<light>
  :bulk +ab-leather+
  :the-kind '<headgear>
  :armour-rating +ar-leather+)

(define-object-kind "fur-cap" "& fur cap~"
  :numeric-id 2202
  :text-sym (text-paint-value +term-umber+ #\])
  :weight 15
  :cost 12
  :power-lvl 3
  :sort-value 2202
  :skill '<light>
  :bulk +ab-fur+
  :the-kind '<headgear>
  :armour-rating +ar-fur+)

(define-object-kind "chain-coif" "& chain-coif~"
  :numeric-id 2203
  :gfx-sym (tile-paint-value +tilefile-armour+ 11)
  :text-sym (text-paint-value +term-slate+ #\])
  :weight 20
  :cost 30
  :power-lvl 7
  :sort-value 2203
  :skill '<heavy>
  :bulk +ab-chain-mail+
  :the-kind '<headgear>
  :armour-rating +ar-chain-mail+)

(define-object-kind "iron-helm" "& iron helm~"
  :numeric-id 2204
  :gfx-sym (tile-paint-value +tilefile-armour+ 12)
  :text-sym (text-paint-value +term-slate+ #\])
  :weight 75
  :cost 75
  :power-lvl 6
  :sort-value 2204
  :skill '<heavy>
  :bulk +ab-iron+
  :the-kind '<headgear>
  :armour-rating 5)

(define-object-kind "steel-helm" "& steel helm~"
  :numeric-id 2205
  :gfx-sym (tile-paint-value +tilefile-armour+ 13)
  :text-sym (text-paint-value +term-l-white+ #\])
  :weight 60
  :cost 200
  :power-lvl 15
  :sort-value 2205
  :skill '<heavy>
  :bulk +ab-iron+
  :the-kind '<headgear>
  :armour-rating 6)


;;; == bodywear

(define-object-kind "shirt-expensive" "& expensive shirt~"
  :numeric-id 2300
  :gfx-sym (tile-paint-value 47 0)
  :text-sym (text-paint-value +term-blue+ #\()
  :weight 20
  :cost 40
  :power-lvl 8
  :sort-value 2300
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<body-armour>
  :armour-rating +ar-cloth+)


(define-object-kind "robe" "& robe~"
  :numeric-id 2301
  :gfx-sym (tile-paint-value +tilefile-armour+ 34)
  :text-sym (text-paint-value +term-blue+ #\()
  :weight 20
  :cost 4
  :power-lvl 1
  :sort-value 2301
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<body-armour>
  :armour-rating +ar-cloth+)


(define-object-kind "leather-armour" "leather armour~"
  :numeric-id 2302
  :gfx-sym (tile-paint-value +tilefile-armour+ 38)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :weight 80
  :cost 18
  :power-lvl 2
  :sort-value 2302
  :skill '<light>
  :bulk +ab-leather+
  :the-kind '<body-armour>
  :armour-rating +ar-leather+)

(define-object-kind "studded-leather" "studded leather~"
  :numeric-id 2303
  :gfx-sym (tile-paint-value +tilefile-armour+ 39)
  :text-sym (text-paint-value +term-l-umber+ #\()
  :weight 90
  :cost 35
  :power-lvl 5
  :sort-value 2304
  :skill '<light>
  :bulk +ab-studded-leather+
  :the-kind '<body-armour>
  :armour-rating 5)

(define-object-kind "chain-mail" "chain mail~"
  :numeric-id 2305
  :gfx-sym (tile-paint-value +tilefile-armour+ 45)
  :text-sym (text-paint-value +term-slate+ #\[)
  :weight 220
  :cost 750
  :power-lvl 7
  :sort-value 2305
  :skill '<heavy>
  :bulk +ab-chain-mail+
  :the-kind '<body-armour>
  :armour-rating +ar-chain-mail+)

(define-object-kind "metal-lamellar" "metal lamellar armour~"
  :numeric-id 2306
  :gfx-sym (tile-paint-value +tilefile-armour+ 51)
  :text-sym (text-paint-value +term-l-white+ #\[)
  :weight 340
  :cost 1250
  :power-lvl 10
  :sort-value 2306
  :skill '<heavy>
  :bulk +ab-lamellar+
  :the-kind '<body-armour>
  :armour-rating +ar-lamellar+)

(define-object-kind "steel-plate" "steel plate armour~"
  :numeric-id 2307
  :gfx-sym (tile-paint-value +tilefile-armour+ 52)
  :text-sym (text-paint-value +term-l-white+ #\[)
  :weight 380
  :cost 1350
  :power-lvl 15
  :sort-value 2307
  :skill '<heavy>
  :bulk +ab-steel+
  :the-kind '<body-armour>
  :armour-rating +ar-steel+)

(define-object-kind "shirt-common" "& common shirt~"
  :numeric-id 2308
  :gfx-sym (tile-paint-value 47 0)
  :text-sym (text-paint-value +term-blue+ #\()
  :weight 20
  :cost 40
  :power-lvl 8
  :sort-value 2308
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<body-armour>
  :armour-rating +ar-cloth+)


;;; == legwear

(define-object-kind "pants-expensive" "& expensive pants"
  :numeric-id 2400
  :gfx-sym (tile-paint-value 47 1)
  :text-sym (text-paint-value +term-blue+ #\()
  :weight 20
  :cost 40
  :power-lvl 7
  :sort-value 2400
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<legwear>
  :armour-rating +ar-cloth+)

(define-object-kind "pants-common" "& common pants"
  :numeric-id 2401
  :gfx-sym (tile-paint-value 47 1)
  :text-sym (text-paint-value +term-blue+ #\()
  :weight 20
  :cost 40
  :power-lvl 7
  :sort-value 2401
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<legwear>
  :armour-rating +ar-cloth+)



;;; == cloaks

(define-object-kind "coat-expensive" "& expensive coat~"
  :numeric-id 2500
  :gfx-sym (tile-paint-value +tilefile-armour+ 6)
  :text-sym (text-paint-value +term-green+ #\()
  :weight 10
  :cost 3
  :power-lvl 7
  :sort-value 2500
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<cloak>
  :armour-rating +ar-cloth+)


(define-object-kind "cloak" "& cloak~"
  :numeric-id 2501
  :gfx-sym (tile-paint-value +tilefile-armour+ 0)
  :text-sym (text-paint-value +term-green+ #\()
  :weight 10
  :cost 3
  :power-lvl 1
  :sort-value 2501
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<cloak>
  :armour-rating +ar-cloth+)

(define-object-kind "shadow-cloak" "& shadow cloak~"
  :numeric-id 2502
  :gfx-sym (tile-paint-value +tilefile-armour+ 1)
  :text-sym (text-paint-value +term-l-dark+ #\()
  :weight 5
  :cost 4000
  :power-lvl 15
  :sort-value 2502
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<cloak>
  :armour-rating 6
  :armour-modifier 4)

(define-object-kind "coat-common" "& common coat~"
  :numeric-id 2503
  :gfx-sym (tile-paint-value +tilefile-armour+ 6)
  :text-sym (text-paint-value +term-green+ #\()
  :weight 10
  :cost 3
  :power-lvl 7
  :sort-value 2503
  :skill '<light>
  :bulk +ab-cloth+
  :the-kind '<cloak>
  :armour-rating +ar-cloth+)


;;; == handwear

(define-object-kind "gloves-calfskin" "& set~ of calfskin gloves"
  :numeric-id 2600
  :gfx-sym (tile-paint-value +tilefile-armour+ 24)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :weight 5
  :cost 20
  :power-lvl 3
  :sort-value 2600
  :skill '<light>
  :bulk +ab-leather+
  :the-kind '<gloves>
  :armour-rating +ar-leather+)

(define-object-kind "gloves-leather" "& set~ of leather gloves"
  :numeric-id 2601
  :gfx-sym (tile-paint-value +tilefile-armour+ 23)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :weight 5
  :cost 3
  :power-lvl 2
  :sort-value 2601
  :skill '<light>
  :bulk +ab-leather+
  :the-kind '<gloves>
  :armour-rating 1)

(define-object-kind "gauntlets" "& set~ of gauntlets"
  :numeric-id 2602
  :gfx-sym (tile-paint-value +tilefile-armour+ 26)
  :text-sym (text-paint-value +term-l-umber+ #\])
  :weight 25
  :cost 35
  :power-lvl 7
  :sort-value 2602
  :skill '<heavy>
  :bulk +ab-chain-mail+
  :the-kind '<gloves>
  :armour-rating 2)

(define-object-kind "cesti" "& set~ of cesti"
  :numeric-id 2603
  :gfx-sym (tile-paint-value +tilefile-armour+ 27)
  :text-sym (text-paint-value +term-l-white+ #\])
  :weight 40
  :cost 100
  :power-lvl 12
  :sort-value 2604
  :skill '<heavy>
  :bulk +ab-steel+
  :the-kind '<gloves>
  :armour-rating 5)

;;; == shields

(define-object-kind "small-leather-shield" "& small leather shield~"
  :numeric-id 2700
  :gfx-sym (tile-paint-value +tilefile-armour+ 90)
  :text-sym (text-paint-value +term-l-umber+ #\))
  :weight 50
  :cost 30
  :power-lvl 2
  :sort-value 2700
  :skill '<shield>
  :bulk 3
  :the-kind '<shield>
  :armour-rating 2)

(define-object-kind "large-leather-shield" "& large leather shield~"
  :numeric-id 2701
  :gfx-sym (tile-paint-value +tilefile-armour+ 29)
  :text-sym (text-paint-value +term-l-umber+ #\))
  :weight 100
  :cost 120
  :power-lvl 5
  :sort-value 2701
  :skill '<shield>
  :bulk 8
  :the-kind '<shield>
  :armour-rating 4)

(define-object-kind "small-metal-shield" "& small metal shield~"
  :numeric-id 2702
  :gfx-sym (tile-paint-value +tilefile-armour+ 93)
  :text-sym (text-paint-value +term-slate+ #\))
  :weight 65
  :cost 50
  :power-lvl 7
  :sort-value 2702
  :skill '<shield>
  :bulk 6
  :the-kind '<shield>
  :armour-rating 3)

(define-object-kind "large-metal-shield" "& large metal shield~"
  :numeric-id 2703
  :gfx-sym (tile-paint-value +tilefile-armour+ 89)
  :text-sym (text-paint-value +term-slate+ #\))
  :weight 120
  :cost 200
  :power-lvl 12
  :sort-value 2703
  :skill '<shield>
  :bulk 10
  :the-kind '<shield>
  :armour-rating 5)

