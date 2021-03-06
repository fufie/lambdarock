;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/constants.lisp - constants internal to vanilla
Copyright (c) 2002-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

;;; === flags that control print/redraw


(define-redraw-key [stun] "...")
(define-redraw-key [confused] "...")
(define-redraw-key [afraid] "...")
(define-redraw-key [poisoned] "...")
(define-redraw-key [cut] "...")
(define-redraw-key [study] "...")
(define-redraw-key [paralysis] "...")
(define-redraw-key [mana] "...")

(define-update-key [mana] "...")
(define-update-key [spells] "...")

(defconstant +default-detect-radius+ 25 "what is the radius of a detection-spell")

(defconstant +van/turns-in-minute+ 60)
(defconstant +van/turns-in-hour+ 3600)
(defconstant +van/turns-in-24hours+ 86400)

(defconstant +tilefile-states+ 41)
