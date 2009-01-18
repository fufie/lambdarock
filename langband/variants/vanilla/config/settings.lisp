;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/settings.lisp - vanilla-settings
Copyright (c) 2003-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-settings '("vanilla-basic-frame" "basic-frame-locations")
    "mana" 17
    
    "cut"    20
    "stun"   21)


(define-settings '("vanilla-birth-settings" "birth-settings")
    )

(define-settings '("sdl-vanilla-birth-settings" "vanilla-birth-settings")
    "instr-x" 15
    "instr-y" 3
    "instr-attr" +term-blue+
    "instr-w" 30
    
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
    "altern-attr" +term-umber+ ;;+term-l-red+
    "altern-sattr" +term-umber+ ;; +term-umber+
    "button-attr" :matte-brown ;;:green
    "button-sattr" :beige ;;:red
    "note-colour" +term-white+
    "background" '(engine-gfx "textures/plainbook.png")
    "background-width"  800
    "background-height" 600
    )

(define-settings '("sdl-vanilla-chardisplay" "chardisplay-settings")
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
    "background" '(engine-gfx "textures/plainbook.png")
    "background-width"  800
    "background-height" 600
    )

(define-settings '("sdl-vanilla-resist" "resistdisplay-settings")
    "title-x" 15
    "title-y" 3
    "title-attr" +term-blue+
    "list-x" 15
    "list-y" 6
    "unres-attr" +term-red+
    "res-attr" +term-green+
    )

(define-settings '("vanilla-dungeon-settings" "dungeon-settings")
    "max-width" 198 ;; must not be larger than +max-dungeon-width+
    "max-height" 66 ;; must not be larger than +max-dungeon-height+
    ;; ranges
    "stairs-down" '(10 . 20) ;; (3 4)
    "stairs-up" '(10 . 20) ;; (1 2)
    )



