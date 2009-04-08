;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/print - print stuff
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-printfield-handler '-basic/hitpoints-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)

	(let ((cur-hp (current-hp player))
	      (max-hp (maximum-hp player))
	      )
	  (warn "basic hp")
	  (output-string! win 0 row +term-white+ "HP")
	  
	  (print-number win (cond ((>= cur-hp max-hp) +term-l-green+)
				  ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
				  (t +term-red+))
			cur-hp 5 row 2)
	  (win/format win 7 row +term-l-green+ "/~v" 3 max-hp)
	  t))
    
    :hook-on-redraw '[hp])
