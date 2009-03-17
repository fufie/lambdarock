;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/gold.lisp - gold-objects for vanilla variant
Copyright (c) 2000-2003 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

(define-object-kind "object-480" "copper"
  :numeric-id 480
  :gfx-sym (tile-paint-value 10 7)
  :text-sym (text-paint-value +term-umber+ #\$)
  :power-lvl 1
  :weight 0
  :cost 3
  :sort-value 7101
  :the-kind '<money>) 

(define-object-kind "object-481" "copper"
  :numeric-id 481
  :gfx-sym (tile-paint-value 10 7)
  :text-sym (text-paint-value +term-umber+ #\$)
  :power-lvl 1
  :weight 0
  :cost 4
  :sort-value 7102
  :the-kind '<money>) 

(define-object-kind "object-482" "copper"
  :numeric-id 482
  :gfx-sym (tile-paint-value 10 7)
  :text-sym (text-paint-value +term-umber+ #\$)
  :power-lvl 1
  :weight 0
  :cost 5
  :sort-value 7103
  :the-kind '<money>) 

(define-object-kind "object-483" "silver"
  :numeric-id 483
  :gfx-sym (tile-paint-value 10 8)
  :text-sym (text-paint-value +term-slate+ #\$)
  :power-lvl 1
  :weight 0
  :cost 6
  :sort-value 7104
  :the-kind '<money>) 

(define-object-kind "object-484" "silver"
  :numeric-id 484
  :gfx-sym (tile-paint-value 10 8)
  :text-sym (text-paint-value +term-slate+ #\$)
  :power-lvl 1
  :weight 0
  :cost 7
  :sort-value 7105
  :the-kind '<money>) 

(define-object-kind "object-485" "silver"
  :numeric-id 485
  :gfx-sym (tile-paint-value 10 8)
  :text-sym (text-paint-value +term-slate+ #\$)
  :power-lvl 1
  :weight 0
  :cost 8
  :sort-value 7106
  :the-kind '<money>) 

(define-object-kind "object-486" "garnets"
  :numeric-id 486
  :gfx-sym (tile-paint-value 10 12)
  :text-sym (text-paint-value +term-red+ #\$)
  :power-lvl 1
  :weight 0
  :cost 9
  :sort-value 7107
  :the-kind '<money>) 

(define-object-kind "object-487" "garnets"
  :numeric-id 487
  :gfx-sym (tile-paint-value 10 12)
  :text-sym (text-paint-value +term-red+ #\$)
  :power-lvl 1
  :weight 0
  :cost 10
  :sort-value 7108
  :the-kind '<money>) 

(define-object-kind "object-488" "gold"
  :numeric-id 488
  :gfx-sym (tile-paint-value 10 9)
  :text-sym (text-paint-value +term-yellow+ #\$)
  :power-lvl 1
  :weight 0
  :cost 12
  :sort-value 7109
  :the-kind '<money>) 

(define-object-kind "object-489" "gold"
  :numeric-id 489
  :gfx-sym (tile-paint-value 10 9)
  :text-sym (text-paint-value +term-yellow+ #\$)
  :power-lvl 1
  :weight 0
  :cost 14
  :sort-value 7110
  :the-kind '<money>) 

(define-object-kind "object-490" "gold"
  :numeric-id 490
  :gfx-sym (tile-paint-value 10 9)
  :text-sym (text-paint-value +term-yellow+ #\$)
  :power-lvl 1
  :weight 0
  :cost 16
  :sort-value 7111
  :the-kind '<money>) 

(define-object-kind "object-491" "opals"
  :numeric-id 491
  :gfx-sym (tile-paint-value 10 13)
  :text-sym (text-paint-value +term-l-white+ #\$)
  :power-lvl 1
  :weight 0
  :cost 18
  :sort-value 7112
  :the-kind '<money>) 

(define-object-kind "object-492" "sapphires"
  :numeric-id 492
  :gfx-sym (tile-paint-value 10 14)
  :text-sym (text-paint-value +term-blue+ #\$)
  :power-lvl 1
  :weight 0
  :cost 20
  :sort-value 7113
  :the-kind '<money>) 

(define-object-kind "object-493" "rubies"
  :numeric-id 493
  :gfx-sym (tile-paint-value 10 15)
  :text-sym (text-paint-value +term-red+ #\$)
  :power-lvl 1
  :weight 0
  :cost 24
  :sort-value 7114
  :the-kind '<money>) 

(define-object-kind "object-494" "diamonds"
  :numeric-id 494
  :gfx-sym (tile-paint-value 10 16)
  :text-sym (text-paint-value +term-white+ #\$)
  :power-lvl 1
  :weight 0
  :cost 28
  :sort-value 7115
  :the-kind '<money>) 

(define-object-kind "object-495" "emeralds"
  :numeric-id 495
  :gfx-sym (tile-paint-value 10 17)
  :text-sym (text-paint-value +term-green+ #\$)
  :power-lvl 1
  :weight 0
  :cost 32
  :sort-value 7116
  :the-kind '<money>) 

(define-object-kind "object-496" "mithril"
  :numeric-id 496
  :gfx-sym (tile-paint-value 10 10)
  :text-sym (text-paint-value +term-l-blue+ #\$)
  :power-lvl 1
  :weight 0
  :cost 40
  :sort-value 7117
  :the-kind '<money>) 

(define-object-kind "object-497" "adamantite"
  :numeric-id 497
  :gfx-sym (tile-paint-value 10 11)
  :text-sym (text-paint-value +term-l-green+ #\$)
  :power-lvl 1
  :weight 0
  :cost 80
  :sort-value 7118
  :the-kind '<money>) 
