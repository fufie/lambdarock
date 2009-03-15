;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)


;;(define-room "simple-room" #'common-make-simple-room)
;;(define-room "overlapping-room" #'common-make-overlapping-room)

(register-information& "status-roll" 100 ;; what's the roll of status
		       "status-cap" 100) ;; what's max status

(register-information& "which-town" "bartertown")

(define-floor-type "nothing" "nothing"
  :numeric-id 73
  :text-sym (text-paint-value +term-white+ #\Space)
  :flags 0
  :gfx-sym (tile-paint-value 0 0))

;;; Reduce this list later
(define-element '<fire>        "fire"        :bit-flag #x00000001 :number 0)
(define-element '<acid>        "acid"        :bit-flag #x00000002 :number 1)
(define-element '<electricity> "electricity" :bit-flag #x00000004 :number 2)
(define-element '<cold>        "cold"        :bit-flag #x00000008 :number 3)
(define-element '<poison>      "poison"      :bit-flag #x00000010 :number 4)
(define-element '<darkness>    "darkness"    :bit-flag #x00000020 :number 5)
(define-element '<light>       "solar light" :bit-flag #x00000040 :number 6)
(define-element '<blindness>   "blindness"   :bit-flag #x00000080 :number 7)
(define-element '<disenchant>  "disenchant"  :bit-flag #x00000100 :number 8)
(define-element '<shards>      "shards"      :bit-flag #x00000200 :number 9)
(define-element '<confusion>   "confusion"   :bit-flag #x00000400 :number 10)
(define-element '<nexus>       "nexus"       :bit-flag #x00000800 :number 11)
(define-element '<sound>       "sound"       :bit-flag #x00001000 :number 12)
(define-element '<nether>      "nether"      :bit-flag #x00002000 :number 13)
(define-element '<gravity>     "gravity"     :bit-flag #x00004000 :number 14)
(define-element '<chaos>       "chaos"       :bit-flag #x00008000 :number 15)
(define-element '<fear>        "fear"        :bit-flag #x00010000 :number 16)
(define-element '<sleep>       "sleep"       :bit-flag #x00020000 :number 17)
(define-element '<plasma>      "plasma"      :bit-flag #x00040000 :number 18)
(define-element '<mana>        "mana"        :bit-flag #x00080000 :number 19)
(define-element '<water>       "water"       :bit-flag #x00100000 :number 20)
(define-element '<time>        "time"        :bit-flag #x00200000 :number 21)
(define-element '<inertia>     "inertia"     :bit-flag #x00400000 :number 22)
(define-element '<force>       "force"       :bit-flag #x00800000 :number 23)
(define-element '<holiness>    "holiness"    :bit-flag #x01000000 :number 24)
(define-element '<erosion>     "erosion"     :bit-flag #x02000000 :number 25)