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
	  (output-string! win 0 row +term-l-white+ "Health")
	  (incf row)
	  (print-number win (cond ((>= cur-hp max-hp) +term-l-white+)
				  ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-violet+)
				  (t +term-red+))
			cur-hp 3 row 0)
	  (win/format win 4 row +term-l-white+ "/~v" 3 max-hp)
	  t))
    
    :hook-on-redraw '[hp])
