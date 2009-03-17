;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/defines.lisp - various defines that should be loaded as data
Copyright (c) 2000-2003 - Stig Erik Sandoe

|#

(in-package :org.langband.contraband)


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
