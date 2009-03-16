;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/print.lisp - various printing methods
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)



(defmethod display-player-skills ((variant evomyth) player term settings)
  (declare (ignore term))
  (let* ((row (setting-lookup settings "skills-y" 10))
	 (col (setting-lookup settings "skills-x" 42))
	 ;;(value-attr (setting-lookup settings "value-attr" +term-l-green+))
	 (sk-attr (setting-lookup settings "title-attr" +term-white+))
	 (counter 0))

    ;; maybe sort on score
    (loop for x across (variant.skills variant)
	  for disp-row = (+ row counter)
	  do
	  (when (and x (< counter 20)) ;; display max 20
	    (let ((score (gethash (evo/skill.slot x) (player.skills player))))
	      (when (plusp score)
		(incf counter)
		(put-coloured-str! +term-green+ (format nil "~2,'0d" score)
				   col disp-row)
		(put-coloured-str! sk-attr (evo/skill.id x) (+ 4 col) disp-row)
		
		))))
    
    player))

(defmethod display-player-combat-ratings ((variant evomyth) player term settings)
  ;; nothing
  (declare (ignore term))

  (let* ((col (setting-lookup settings "elem-x"))
	 (row (setting-lookup settings "elem-y"))
	 (title-attr (setting-lookup settings "title-attr" +term-white+))
	 (value-attr (setting-lookup settings "value-attr" +term-l-blue+))
	 (f-col (+ col 9)))

    (incf row 8)

    (put-coloured-str! title-attr "Attack" col row)
    (put-coloured-str! value-attr (format nil "~3d"  (evo/calculate-attack-strength player))
		       f-col (+ 0 row))

    (put-coloured-str! title-attr "Defence" col (+ row 1))
    (put-coloured-str! value-attr (format nil "~3d"  (evo/calculate-defence-strength player))
		       f-col (+ 1 row))
    
  t))

(defmethod print-armour-class ((variant evomyth) (player player) setting)
  "Prints AC to left frame."
  
  (let* ((ac-set (setting-lookup setting "ac")))

    (output-string! +charinfo-frame+ 0 ac-set +term-white+ "Defence")

    (print-number +charinfo-frame+ +term-l-green+
		  (evo/calculate-defence-strength player) ;; hack, optimise later
		  4
		  ac-set
		  7)))
