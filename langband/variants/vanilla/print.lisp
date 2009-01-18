;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/print.lisp - printout code for vanilla
Copyright (c) 2002-2004 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)

(defmethod print-depth ((variant vanilla-variant) (level level) setting)
  "prints current depth somewhere"
  (declare (ignore setting))
  (with-frame (+misc-frame+)
    (let ((column (- (get-frame-width +misc-frame+) 8))) 
      (put-coloured-line! +term-l-blue+ (format nil "~d ft" (* 50 (level.depth level))) column 0))))


(defmethod print-depth ((variant vanilla-variant) (level van/town-level) setting)
  "prints current depth somewhere"
  (declare (ignore setting))
  (let ((column (- (get-frame-width +misc-frame+) 8)))
    (with-frame (+misc-frame+)
      (put-coloured-line! +term-l-blue+ "Town"
			 column
			 0))))

(defun print-cut (variant player setting)

  (when (basic-frame-shown? variant)
    
    (let* ((win (get-window +charinfo-frame+))
	   (cuts (get-attribute-value '<cut> (player.temp-attrs player)))
	   (loc (setting-lookup setting "cut"))
	   (cut-info (%get-cutlvl cuts)))
      
      (warn "print cut ~s ~s ~s" cuts cut-info loc)
      
      (output-string! win 0 loc (second cut-info) (third cut-info))
      )))


(defun print-stun (variant player setting)

  (when (basic-frame-shown? variant)
    
    (let* ((win (get-window +charinfo-frame+))
	   (stun (get-attribute-value '<stun> (player.temp-attrs player)))
	   (loc (setting-lookup setting "stun"))
	   (stun-info (%get-stunlvl stun)))
      
      (warn "print stun ~s ~s ~s" stun stun-info loc)
      
      (output-string! win 0 loc (second stun-info) (third stun-info))
      )))



(defmethod print-extra-frame-content ((variant vanilla-variant) (dungeon dungeon) (player player))
  (let ((pr-set (get-setting variant :basic-frame-printing)))
	
    (print-cut variant player pr-set)
    (print-hunger variant player pr-set)
    (trigger-printfield-hooks& variant dungeon player '[speed])
    (print-stun variant player pr-set)
    
    (modify-visual-state! variant :poisoned
			  (get-attribute-value '<poisoned> (player.temp-attrs player)))
    (modify-visual-state! variant :afraid
			  (get-attribute-value '<fear> (player.temp-attrs player)))
    (modify-visual-state! variant :confused
			  (get-attribute-value '<confusion> (player.temp-attrs player)))
    (modify-visual-state! variant :paralysed
			  (get-attribute-value '<paralysed> (player.temp-attrs player)))
    (modify-visual-state! variant :blind
			  (get-attribute-value '<blindness> (player.temp-attrs player)))
    (modify-visual-state! variant :can-study
			  (can-learn-more-spells? variant player))

    ;; more
    ;; more
    
    (display-visual-states variant)
    
    t))



(defmethod redraw-stuff ((variant vanilla-variant) (dungeon dungeon) (player player))
  
  (unless (any-redraws? player) (return-from redraw-stuff nil))

  (let ((retval nil)
	(pr-set nil)
	)

    (when (want-redraw? player '[extra])
      (reset-redraw! player '[extra])
      (reset-redraw! player '[cut])
      (reset-redraw! player '[stun])
      (reset-redraw! player '[satiation])
      (reset-redraw! player '[blind])
      (reset-redraw! player '[confused])
      (reset-redraw! player '[afraid])
      (reset-redraw! player '[poisoned])
      (reset-redraw! player '[paralysis])
      (reset-redraw! player '[speed])
      (reset-redraw! player '[study])

      (print-extra-frame-content variant dungeon player)
      (setf retval t))

    
    (when (want-redraw? player '[cut])
      (reset-redraw! player '[cut])
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-cut variant player pr-set)
      (setf retval t))

    (when (want-redraw? player '[stun])
      (reset-redraw! player '[stun])
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-stun variant player pr-set)
      (setf retval t))

    (when (want-redraw? player '[satiation])
      (reset-redraw! player '[satiation])
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-hunger variant player pr-set)
      (setf retval t))

    (when (want-redraw? player '[blind])
      (reset-redraw! player '[blind])
      (modify-visual-state! variant :blind (get-attribute-value '<blindness> (player.temp-attrs player)))
      (setf retval t))

    (when (want-redraw? player '[confused])
      (reset-redraw! player '[confused])
      (modify-visual-state! variant :confused
			    (get-attribute-value '<confusion> (player.temp-attrs player)))
      (setf retval t))

    (when (want-redraw? player '[afraid])
      (reset-redraw! player '[afraid])
      (modify-visual-state! variant :afraid
			    (get-attribute-value '<fear> (player.temp-attrs player)))
      (setf retval t))
    

    (when (want-redraw? player '[poisoned])
      (reset-redraw! player '[poisoned])
      (modify-visual-state! variant :poisoned (get-attribute-value '<poisoned> (player.temp-attrs player)))
      (setf retval t))


    (when (want-redraw? player '[paralysis])
      (reset-redraw! player '[paralysis])
      (modify-visual-state! variant :paralysed (get-attribute-value '<paralysed> (player.temp-attrs player)))
      (setf retval t))
    
    ;; speed in engine

    (when (want-redraw? player '[study])
      (reset-redraw! player '[study])
      (modify-visual-state! variant :can-study (can-learn-more-spells? variant player))
      (setf retval t))

    (when (want-redraw? player '[mana])
      (reset-redraw! player '[mana])
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-mana-points variant player pr-set)
      (setf retval t))

    (when (call-next-method)
      (setf retval t))
    
    (when retval
      (display-visual-states variant))
    
    retval))


(defun print-mana-points (variant player setting)
  "Prints mana-points info to left frame."
  
  (when (is-spellcaster? player)

    (when (basic-frame-shown? variant)
	
      (let ((win (get-window +charinfo-frame+))
	    (cur-hp (current-mana player))
	    (max-hp (maximum-mana player))
	    (row (setting-lookup setting "mana"))
	    (colour +term-red+)
	    )
	
	(output-string! win 0 row +term-white+ "MP")
	
	(setf colour (cond ((>= cur-hp max-hp) +term-l-green+)
			   ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
			   (t +term-red+)))
	
	(win/format win 4 row colour "~v" 3 cur-hp)
	(win/format win 7 row +term-l-green+ "/~v" 3 max-hp)
	
	t))
    ))


(defmethod print-basic-frame ((variant vanilla-variant) (dungeon dungeon) (player player))

  (when (basic-frame-shown? variant)
    (call-next-method)
    
    (let ((pr-set (get-setting variant :basic-frame-printing)))
      (print-mana-points variant player pr-set)
      
      (reset-redraw! player '[mana])
      
      t)))


(defmethod display-player-skills ((variant vanilla-variant) player term settings)
  (declare (ignore term))
  (let* ((offset-x (setting-lookup settings "offset-x" 0))
	 (offset-y (setting-lookup settings "offset-y" 0))
	 (col (+ offset-x (setting-lookup settings "skills-x" 42)))
	 (row (+ offset-y (setting-lookup settings "skills-y" 10)))
	 (value-attr (setting-lookup settings "value-attr" +term-l-green+))
	 (sk-attr (setting-lookup settings "title-attr" +term-white+)))

    (flet ((print-skill (skill div row)
	     (declare (ignore div))
	     (let ((val (slot-value (player.skills player) skill)))
	       (put-coloured-str! value-attr
				  (format nil "~9d" val)
				  (+ col 14)
				  row))))
      
      (put-coloured-str! sk-attr "Saving Throw" col (+ row 0))
      (print-skill 'saving-throw 6 (+ row 0))

      (put-coloured-str! sk-attr "Stealth" col (+ row 1))
      (print-skill 'stealth 1 (+ row 1))
      
      (put-coloured-str! sk-attr "Fighting" col (+ row 2))
      (print-skill 'fighting 12 (+ row 2))

      (put-coloured-str! sk-attr "Shooting" col (+ row 3))
      (print-skill 'shooting 12 (+ row 3))

      (put-coloured-str! sk-attr "Disarming" col (+ row 4))
      (print-skill 'disarming 8 (+ row 4))

      (put-coloured-str! sk-attr "Magic Device" col (+ row 5))
      (print-skill 'device 6 (+ row 5))

      (put-coloured-str! sk-attr "Perception" col (+ row 6))
      (print-skill 'perception 6 (+ row 6))

      (put-coloured-str! sk-attr "Searching" col (+ row 7))
      (print-skill 'searching 6 (+ row 7))

      t)))

(defmethod display-player-combat-ratings ((variant vanilla-variant) player term settings)
  (declare (ignore term))

  (let* ((offset-x (setting-lookup settings "offset-x" 0))
	 (offset-y (setting-lookup settings "offset-y" 0))
	 (title-attr (setting-lookup settings "title-attr" +term-white+))
	 (value-attr (setting-lookup settings "value-attr" +term-l-blue+))
	 (col (+ offset-x (setting-lookup settings "combat-x" 26)))
	 (row (+ offset-y (setting-lookup settings "combat-y" 13)))
	 (f-col (+ col 7))

	 (perc (player.perceived-abilities player))
	 (p-ac (get-armour-rating perc))
	 (p-acmod (get-armour-modifier perc))
	 (tohit (get-tohit-modifier perc))
	 (dmg (get-damage-modifier perc))

	 (mewpn (get-melee-weapon player))
	 (miwpn (get-missile-weapon player))

	 (mel-tohit 0)
	 (mel-dmg 0)
	 (miss-tohit 0)
	 (miss-dmg 0)
	 )
    (when mewpn
      (setf mel-tohit (get-tohit-modifier mewpn)
	    mel-dmg (get-damage-modifier mewpn)))
    
    (when miwpn
      (setf miss-tohit (get-tohit-modifier miwpn)
	    miss-dmg (get-damage-modifier miwpn)))
    
    
    
    ;; hack
    (decf row 13)

    (put-coloured-str! title-attr "Armour" col (+ row 13))
          
    (put-coloured-str! value-attr
		       (format nil "~12@a" (format nil "[~d,~@d]" p-ac p-acmod))
		       (1+ f-col) (+ row 13))


      
    (put-coloured-str! title-attr  "Fight" col (+ row 14))
    (put-coloured-str! value-attr (format nil "~13@a" (format nil "(~@d,~@d)" tohit dmg))
		       f-col (+ row 14))
  
    ;; skip weapon+bow specifics now
    (put-coloured-str! title-attr  "Melee" col (+ row 15))
    (put-coloured-str! value-attr (format nil "~13@a" (format nil "(~@d,~@d)" (+ tohit mel-tohit)
							      (+ dmg mel-dmg)))
		       f-col (+ row 15))
  
    (put-coloured-str! title-attr "Shoot" col (+ row 16))
    (put-coloured-str! value-attr (format nil "~13@a" (format nil "(~@d,~@d)" (+ tohit miss-tohit) miss-dmg))
		       f-col (+ row 16))
  
  
  
    (put-coloured-str! title-attr  "Blows" col (+ row 17))
    (put-coloured-str! value-attr (format nil "~13@a" "1/turn")
		       f-col (+ row 17))
  
    (put-coloured-str! title-attr  "Shots" col (+ row 18))
    (put-coloured-str! value-attr (format nil "~13@a" "1/turn")
		       f-col (+ row 18))
		   
    (put-coloured-str! title-attr "Infra" col (+ row 19))

    (put-coloured-str! value-attr
		       (format nil "~10d ft" (* 10 (player.infravision player)))
		       f-col (+ row 19))
    
    t))
