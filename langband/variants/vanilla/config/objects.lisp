;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/objects.lisp - objects for vanilla variant
Copyright (c) 2000-2002 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

;;; === Note ===
;;; vanilla-specific treats
;;; :depth is translated to power-lvl slot

(define-object-kind "pile" "<pile>"
  :numeric-id 0
  :text-sym (text-paint-value +term-white+ #\&)
  :gfx-sym (tile-paint-value 10 54)
  :power-lvl 0
  ) 

(define-object-kind "shovel" "& shovel~"
  :numeric-id 84
  :gfx-sym (tile-paint-value 10 30)
  :text-sym (text-paint-value +term-slate+ #\\)
  :power-lvl 1
  :locations '((5 . 16))
  :weight 60
  :cost 10
  :flags '(<show-modififers>)
  :sort-value 2901
  :the-kind '<digger>
  :damage "1d2")

(define-object-kind "gnomish-shovel" "& gnomish shovel~"
  :numeric-id 85
  :gfx-sym (tile-paint-value 10 31)
  :text-sym (text-paint-value +term-l-green+ #\\)
  :power-lvl 20
  :locations '((20 . 4))
  :weight 60
  :cost 100
  :flags '(<show-modififers>)
  :sort-value 2902
  :the-kind '<digger>
  :damage "1d2")

(define-object-kind "dwarven-shovel" "& dwarven shovel~"
  :numeric-id 86
  :gfx-sym (tile-paint-value 10 32)
  :text-sym (text-paint-value +term-l-blue+ #\\)
  :power-lvl 40
  :locations '((40 . 1))
  :weight 120
  :cost 200
  :flags '(<show-modififers>)
  :sort-value 2903
  :the-kind '<digger>
  :damage "1d3")

(define-object-kind "pick" "& pick~"
  :numeric-id 87
  :gfx-sym (tile-paint-value 10 27)
  :text-sym (text-paint-value +term-slate+ #\\)
  :power-lvl 5
  :locations '((10 . 16))
  :weight 150
  :cost 50
  :flags '(<show-modififers>)
  :sort-value 2904
  :the-kind '<digger>
  :damage "1d3")

(define-object-kind "orcish-pick" "& orcish pick~"
  :numeric-id 88
  :gfx-sym (tile-paint-value 10 28)
  :text-sym (text-paint-value +term-green+ #\\)
  :power-lvl 30
  :locations '((30 . 4))
  :weight 150
  :cost 300
  :flags '(<show-modififers>)
  :sort-value 2905
  :the-kind '<digger>
  :damage "1d3")

(define-object-kind "dwarven-pick" "& dwarven pick~"
  :numeric-id 89
  :gfx-sym (tile-paint-value 10 29)
  :text-sym (text-paint-value +term-blue+ #\\)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 200
  :cost 600
  :flags '(<show-modififers>)
  :sort-value 2906
  :the-kind '<digger>
  :damage "1d4")


(define-object-kind "small-wooden-chest" "& small wooden chest~"
  :numeric-id 338
  :gfx-sym (tile-paint-value 10 0)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 250
  :cost 20
  :sort-value 2401
  :the-kind '<chest>
  :damage "2d3")

(define-object-kind "large-wooden-chest" "& large wooden chest~"
  :numeric-id 339
  :gfx-sym (tile-paint-value 10 1)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 15
  :locations '((15 . 1))
  :weight 500
  :cost 60
  :sort-value 2405
  :the-kind '<chest>
  :damage "2d5")

(define-object-kind "small-iron-chest" "& small iron chest~"
  :numeric-id 340
  :gfx-sym (tile-paint-value 10 2)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 25
  :locations '((25 . 1))
  :weight 300
  :cost 100
  :sort-value 2402
  :the-kind '<chest>
  :damage "2d4")

(define-object-kind "large-iron-chest" "& large iron chest~"
  :numeric-id 341
  :gfx-sym (tile-paint-value 10 3)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 35
  :locations '((35 . 1))
  :weight 1000
  :cost 150
  :sort-value 2406
  :the-kind '<chest>
  :damage "2d6")

(define-object-kind "small-steel-chest" "& small steel chest~"
  :numeric-id 342
  :gfx-sym (tile-paint-value 10 4)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 45
  :locations '((45 . 1))
  :weight 500
  :cost 200
  :sort-value 2403
  :the-kind '<chest>
  :damage "2d4")

(define-object-kind "large-steel-chest" "& large steel chest~"
  :numeric-id 343
  :gfx-sym (tile-paint-value 10 5)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 55
  :locations '((55 . 1))
  :weight 1000
  :cost 250
  :sort-value 2407
  :the-kind '<chest>
  :damage "2d6")

(define-object-kind "ruined-chest" "& ruined chest~"
  :numeric-id 344
  :gfx-sym (tile-paint-value 10 6)
  :text-sym (text-paint-value +term-slate+ #\~)
  :power-lvl 0
  :locations '((75 . 1))
  :weight 250
  :cost 0
  :sort-value 2400
  :the-kind '<chest>)

#||
(define-object-kind "iron-spike" "& iron spike~"
  :numeric-id 345
  :text-sym (text-paint-value +term-l-white+ #\~)
  :power-lvl 1
  :locations '((1 . 1))
  :weight 10
  :cost 1
  :sort-value 2300
  :damage 1)
||#

(define-object-kind "torch" "& wooden torch~"
  :numeric-id 346
  :gfx-sym (tile-paint-value 10 26)
  :text-sym (text-paint-value +term-umber+ #\~)
  :power-lvl 1
  :locations '((1 . 1))
  :weight 30
  :cost 2
  :flags '(<easy-know>)
  :sort-value 4200
  :the-kind '<light-source>
  :max-fuel 5000
  :status-descs '("fresh" "almost fresh" "half-burnt" "well-burnt" "almost out" "burnt out")
  :vulnerabilities '(<fire> <plasma>)
  :charges 4000
  :light-radius 1
  :damage 1)

(define-object-kind "lantern" "& brass lantern~"
  :numeric-id 347
  :gfx-sym (tile-paint-value 10 25)
  :text-sym (text-paint-value +term-l-umber+ #\~)
  :power-lvl 3
  :locations '((3 . 1))
  :weight 50
  :cost 35
  :flags '(<easy-know>)
  :sort-value 4201
  :the-kind '<light-source>
  :max-fuel 15000
  :status-descs '("full" "almost full" "half-full" "little left" "almost empty" "empty")
  :charges 7500
  :light-radius 2
  :damage 1
  :ignores '(<fire>))

(define-object-kind "oil-flask" "& flask~ of oil"
  :numeric-id 348
  :gfx-sym (tile-paint-value 10 23)
  :text-sym (text-paint-value +term-yellow+ #\!)
  :power-lvl 1
  :locations '((1 . 1))
  :weight 10
  :cost 3
  :sort-value 5700
  :text-colour +term-yellow+
  :vulnerabilities '(<sound> <cold> <shards> <force>)
  :charges 7500
  :the-kind '<light-source> ;; hack!
  :light-radius 0
  :damage "2d6")


(define-object-kind "empty-bottle" "& empty bottle~"
  :numeric-id 349
  :gfx-sym (tile-paint-value 10 22)
  :text-sym (text-paint-value +term-white+ #\!)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 2
  :cost 0
  :sort-value 2101
  :vulnerabilities '(<sound> <cold> <shards> <force> <acid>)
  :damage 1)
			

(define-object-kind "pottery-shards" "& shard~ of pottery"
  :numeric-id 389
  :gfx-sym (tile-paint-value 10 34)
  :text-sym (text-paint-value +term-red+ #\~)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 5
  :cost 0
  :the-kind '<junk>
  :sort-value 2203
  :damage 1)

(define-object-kind "broken-stick" "& broken stick~"
  :numeric-id 390
  :gfx-sym (tile-paint-value 10 42)
  :text-sym (text-paint-value +term-red+ #\~)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 3
  :cost 0
  :the-kind '<junk>
  :sort-value 2206
  :damage 1)

(define-object-kind "broken-skull" "& broken skull~"
  :numeric-id 391
  :gfx-sym (tile-paint-value 10 43)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 1
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2001
  :damage 1)

(define-object-kind "broken-bone" "& broken bone~"
  :numeric-id 392
  :gfx-sym (tile-paint-value 10 44)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 0
  :locations '((0 . 1))
  :weight 2
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2002
  :damage 1)

(define-object-kind "canine-skeleton" "& canine skeleton~"
  :numeric-id 393
  :gfx-sym (tile-paint-value 10 49)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 1
  :locations '((1 . 1))
  :weight 10
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2004
  :damage 1)


(define-object-kind "rodent-skeleton" "& rodent skeleton~"
  :numeric-id 394
  :gfx-sym (tile-paint-value 10 50)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 1
  :locations '((1 . 1))
  :weight 10
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2003
  :damage 1)

(define-object-kind "human-skeleton" "& human skeleton~"
  :numeric-id 395
  :gfx-sym (tile-paint-value 10 45)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 60
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2008
  :damage "1d2")

(define-object-kind "dwarf-skeleton" "& dwarf skeleton~"
  :numeric-id 396
  :gfx-sym (tile-paint-value 10 47)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 50
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2007
  :damage "1d2")

(define-object-kind "elf-skeleton" "& elf skeleton~"
  :numeric-id 397
  :gfx-sym (tile-paint-value 10 46)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 40
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2006
  :damage "1d2")

(define-object-kind "gnome-skeleton" "& gnome skeleton~"
  :numeric-id 398
  :gfx-sym (tile-paint-value 10 48)
  :text-sym (text-paint-value +term-white+ #\~)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 30
  :cost 0
  :the-kind '<skeleton>
  :sort-value 2005
  :damage "1d2")

;;; artifact items
#||
(define-object-kind "the-phial" "& phial~"
  :numeric-id 500
  :text-sym (text-paint-value +term-yellow+ #\~)
  :power-lvl 1
  :weight 10
  :cost 10000
  :flags '(<instant-artifact>)
  :sort-value 4204
  :the-kind '<light-source>
  :light-radius 3
  :damage 1)

(define-object-kind "the-star" "& star~"
  :numeric-id 501
  :text-sym (text-paint-value +term-yellow+ #\~)
  :power-lvl 30
  :weight 5
  :cost 25000
  :flags '(<instant-artifact>)
  :sort-value 4205
  :the-kind '<light-source>
  :light-radius 3
  :damage 1)

(define-object-kind "the-arkenstone" "& arkenstone~"
  :numeric-id 502
  :text-sym (text-paint-value +term-yellow+ #\~)
  :power-lvl 60
  :weight 5
  :cost 60000
  :flags '(<instant-artifact>)
  :sort-value 4206
  :the-kind '<light-source>
  :light-radius 3
  :damage 1)

(define-object-kind "amulet-carlammas" "& amulet~"
  :numeric-id 503
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 50
  :weight 3
  :cost 60000
  :flags '(<instant-artifact>)
  :sort-value 4310
  :the-kind '<amulet>)

(define-object-kind "amulet-ingwe" "& amulet~"
  :numeric-id 504
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 60
  :weight 3
  :cost 90000
  :flags '(<instant-artifact>)
  :sort-value 4311
  :the-kind '<amulet>)

(define-object-kind "dwarven-necklace" "& necklace~"
  :numeric-id 505
  :text-sym (text-paint-value +term-dark+ #\")
  :power-lvl 70
  :weight 3
  :cost 75000
  :flags '(<instant-artifact>)
  :sort-value 4312
  :the-kind '<neckwear>)

(define-object-kind "object-506" "& ring~"
  :numeric-id 506
  :text-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 50
  :weight 2
  :cost 65000
  :flags '(<instant-artifact>)
  :sort-value 4432
  :the-kind '<ring>)

(define-object-kind "object-507" "& ring~"
  :numeric-id 507
  :text-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 90
  :weight 2
  :cost 150000
  :flags '(<instant-artifact>)
  :sort-value 4433
  :the-kind '<ring>)

(define-object-kind "object-508" "& ring~"
  :numeric-id 508
  :text-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 80
  :weight 2
  :cost 100000
  :flags '(<instant-artifact>)
  :sort-value 4434
  :the-kind '<ring>)

(define-object-kind "object-509" "& ring~"
  :numeric-id 509
  :text-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 90
  :weight 2
  :cost 200000
  :flags '(<instant-artifact>)
  :sort-value 4435
  :the-kind '<ring>)

(define-object-kind "object-510" "& ring~"
  :numeric-id 510
  :text-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 100
  :weight 2
  :cost 300000
  :flags '(<instant-artifact>)
  :sort-value 4436
  :the-kind '<ring>)

(define-object-kind "one-ring" "& ring~"
  :numeric-id 511
  :text-sym (text-paint-value +term-yellow+ #\=)
  :power-lvl 110
  :weight 2
  :cost 5000000
  :flags '(<instant-artifact>)
  :sort-value 4437
  :the-kind '<ring>)
||#
