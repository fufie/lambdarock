;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/print.lisp - various printing methods
Copyright (c) 2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defun print-quests (var-obj player settings)
  (declare (ignore player settings))
  (clear-window *cur-win*)
  (let ((title-row 2)
	(title-col 15)
	(title-attr +term-blue+))
    
    (put-coloured-str! title-attr "Quests" title-col title-row)
    (put-coloured-str! title-attr "======" title-col (1+ title-row))

    (let ((row (+ title-row 3))
	  (dummy-col 9))
      (loop for x being the hash-values of (variant.quests var-obj)
	    do
	    (when (and (eq (quest.state x) :active)
		       (not (quest.parent x)))
	      (put-coloured-str! title-attr (quest.title x) title-col (incf row))
	      (multiple-value-setq (dummy-col row)
		(print-text! title-col (incf row) +term-green+ (quest.desc x) :end-col 45))

	      (when (stringp (quest.step x))
		(when-bind (q (find-quest var-obj (quest.step x)))
		  (when (eq (quest.state q) :active)
		    (put-coloured-str! title-attr (quest.title q) (+ 2 title-col) (incf row))
		    (multiple-value-setq (dummy-col row)
		      (print-text! (+ 2 title-col) (incf row) +term-green+ (quest.desc q) :end-col 45))

		    )))

	      (incf row)
	      )))
    
    (pause-last-line!)
    ))


(defmethod display-player-skills ((variant contraband) player term settings)
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
	    (let ((score (gethash (con/skill.slot x) (player.skills player))))
	      (when (plusp score)
		(incf counter)
		(put-coloured-str! +term-green+ (format nil "~2,'0d" score)
				   col disp-row)
		(put-coloured-str! sk-attr (con/skill.id x) (+ 4 col) disp-row)
		
		))))
    
    player))

(defmethod display-player-combat-ratings ((variant contraband) player term settings)
  ;; nothing
  (declare (ignore term))

  (let* ((col (setting-lookup settings "elem-x"))
	 (row (setting-lookup settings "elem-y"))
	 (title-attr (setting-lookup settings "title-attr" +term-white+))
	 (value-attr (setting-lookup settings "value-attr" +term-l-blue+))
	 (f-col (+ col 9)))

    (incf row 8)

    (put-coloured-str! title-attr "Attack" col row)
    (put-coloured-str! value-attr (format nil "~3d"  (con/calculate-attack-strength player))
		       f-col (+ 0 row))

    (put-coloured-str! title-attr "Defence" col (+ row 1))
    (put-coloured-str! value-attr (format nil "~3d"  (con/calculate-defence-strength player))
		       f-col (+ 1 row))
    
  t))

(defmethod print-armour-class ((variant contraband) (player player) setting)
  "Prints AC to left frame."
  
  (let* ((ac-set (setting-lookup setting "ac")))

    (output-string! +charinfo-frame+ 0 ac-set +term-white+ "Defence")

    (print-number +charinfo-frame+ +term-l-green+
		  (con/calculate-defence-strength player) ;; hack, optimise later
		  4
		  ac-set
		  7)))




(defmethod print-object ((inst quest) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
           (quest.id inst) (quest.state inst))
   inst))

