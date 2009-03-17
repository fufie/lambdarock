;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/settings.lisp - contraband-settings
Copyright (c) 2003 - Stig Erik Sandoe

|#

(in-package :org.langband.contraband)

(define-settings '("con-basic-frame" "basic-frame-locations")
    "max-mana" '(18 . 0)
    "cur-mana" '(19 . 0)
    
    "target" '(20 . 0)
    "cut"    '(21 . 0)
    "stun"   '(22 . 0))


(define-settings '("con-birth-settings" "birth-settings")
    )

(define-settings '("sdl-con-birth-settings" "con-birth-settings")
    "instr-x" 15
    "instr-y" 3
    "instr-attr" +term-blue+
    "instr-w" 45
    "query-x" 15
    "query-y" 24
    "query-reduced" t
    "query-attr" +term-blue+
    "info-x" 15
    "info-y" 16
    "info-attr" +term-umber+
    "choice-x" 52
    "choice-y" 3
    "choice-tattr" +term-blue+
    "choice-attr" +term-l-red+
    "text-x" 52
    "text-y" 7
    "text-w" 35
    "text-attr" +term-umber+
    "altern-cols" 2
    "altern-attr" +term-umber+
    "altern-sattr" +term-l-red+
    "note-colour" +term-white+
    )

(define-settings '("sdl-con-chardisplay" "chardisplay-settings")
    "title-x" 15
    "title-y" 10
    "title-attr" +term-blue+
    "picture-x" 25
    "picture-y" 2
    "extra-x" 15
    "extra-y" 18
    "elem-x" 15
    "elem-y" 24
    "value-attr" +term-green+
    "value-badattr" +term-red+
    "stats-attr" +term-blue+
    "statok-attr" +term-umber+
    "statbad-attr" +term-l-red+
    "stats-x" 53
    "skills-x" 53
    "combat-x" 53
    "combat-y" 20
    )

