;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/combat.lisp - combat-related code for vanilla
Copyright (c) 2002-2004 - Stig Erik Sandoe

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defmethod melee-inflict-damage! ((attacker player) target the-attack)
  (declare (ignore the-attack))
  (let* ((weapon (get-melee-weapon attacker)))

    (cond ((not weapon)
	   (deduct-hp! target 1)
	   1)
	  (t
	   ;; we have a weapon..
	   (let ((dmg (roll-dice (get-number-of-damage-dice weapon)
				 (get-damage-dice weapon))))
	       (incf dmg (get-damage-modifier weapon))
	       (when (< dmg 1) (setf dmg 1)) ;; minimum damage
	       (deduct-hp! target dmg)
	       dmg)))

    ))

;; this is a bit generic until it gets specialised
(defmethod deliver-damage! ((variant variant) source (target player) damage &key note dying-note)

  ;; to avoid warnings
  (declare (ignore note dying-note))
  
  ;;(warn "deliver ~s damage to player from ~s" damage (get-creature-name source))
  
  (let ((did-target-die? nil))
  
    ;; wake it up
    ;; deliver damage
    (decf (current-hp target) damage)

    (ask-for-redraw! target '[hp])
    
    (when (minusp (current-hp target))
      (let ((killer (etypecase source
		      (string source)
		      (active-monster (get-creature-desc source #x88)))))
	(setf did-target-die? t)

	;; this might fail badly, but at least it will be visible!
	(format-message! "You were killed by ~a." killer)

      (kill-target! variant *dungeon* source target (location-x target) (location-y target))
      ))

    did-target-die?))

(defun get-moving-gfx-sym (obj old-x new-x old-y new-y)
  (let* ((proj (get-visual-projectile obj)))

    (cond (proj
	   (aref (projectile.gfx-path proj) (get-direction-from-coord-diff (- new-x old-x)
									   (- new-y old-y))))
	  (obj
	   (gfx-sym obj))
	  (t
	   (tile-paint-value 4 48))))) ;; hack

(defun get-moving-text-sym (obj old-x new-x old-y new-y)
  (let ((proj (get-visual-projectile obj)))
    
    (cond (proj
	   (aref (projectile.text-path proj) (get-direction-from-coord-diff (- new-x old-x)
									    (- new-y old-y))))
	  (obj
	   (text-sym obj))
	  (t
	   (text-paint-value +term-red+ #\*))))) ;; hack


(defun van-thrown-hitting-target (source dungeon x y thrown)
  "RENAME, hurt.."
  (let ((target nil)
	(mon-name nil))
    
    (when (player-is-at? *player* x y)
      (setf target *player*))
		  
    (when-bind (monsters (cave-monsters dungeon x y))
      (setf target (if (consp monsters) (car monsters) monsters)))

    ;;(warn "throw hit ~s" target)

    (unless target
      (return-from van-thrown-hitting-target nil))

    (setf mon-name (get-creature-desc target #x00))
    
    ;; lots of work goes here
    ;;(warn "hit a dumb monster ~s" mon-name)
    
    (when (< (random 100) 50) ;; fix me
      
      (format-message! "The ~a was hit." mon-name)
      
      (deduct-hp! target (randint 6)) ;; fix me
      
      (when (< (current-hp target) 0)
	(format-message! "The ~a died." mon-name)
	(let ((target-xp (get-xp-value target)))
	  (modify-xp! source (if target-xp target-xp 0)))
	(kill-target! *variant* dungeon source target x y)
	;; repaint spot
	;;(light-spot! dungeon x y)
	))
    thrown))

  
(defun van-missile-hitting-target (source dungeon x y missile-weapon arrow)
  "RENAME.. hurt"
  (let ((target nil)
	(mon-name nil))
    
    (when (player-is-at? *player* x y)
      (setf target *player*))
    
    (when-bind (monsters (cave-monsters dungeon x y))
      (setf target (if (consp monsters) (car monsters) monsters)))
    
    ;;(warn "missile ~s hit ~s" arrow target)
    
    (unless target
      (return-from van-missile-hitting-target nil))
    
    (setf mon-name (get-creature-desc target #x00))
    
    ;;(warn "hit ~s" mon-name)
    (cond ((missile-hit-creature? source target missile-weapon arrow)
	   (format-message! "~@(~A~) was hit." mon-name)
	   (missile-inflict-damage! source target missile-weapon arrow)
	   (when (< (current-hp target) 0)
	     (format-message! "~@(~A~) died." mon-name)
	     (let ((target-xp (get-xp-value target)))
	       (modify-xp! source (if target-xp target-xp 0)))
	     (kill-target! *variant* dungeon source target x y)
		 ;; repaint spot
	     ;;(light-spot! dungeon x y)
	     ))
	  
	  (t
	   (format-message! "Missile barely misses ~a." mon-name)
	   ))
    
    arrow))


(defmethod throw-object ((variant vanilla-variant) (player player) (obj active-object) tx ty)
  
  (let* ((dungeon *dungeon*)
	 (source player)
	 (pvx (location-x player))
	 (pvy (location-y player))
	 (blind-player (is-blind? *variant* *player*))
	 (mul 10)
	 (div (if (> (object.weight obj) 10) (object.weight obj) 10)) ;; minimum 10
	 (max-range (int-/ (* mul (+ 20 (get-stat-info-value variant player
							     '<str> :blow-table)))
			   div))
	 (path-arr (make-array (1+ max-range) :fill-pointer 0))
	 (path-len (project-path dungeon max-range path-arr pvx pvy tx ty 0))
	 (cur-x pvx)
	 (cur-y pvy)
	 )
    
    (declare (ignore path-len))

    ;; unlike earlier, we do things in two passes
    ;; first we check how far we go
    ;; then we go that distance and act on what happens there.


    (loop named follow-path
	  for g across path-arr
	  do
	  (let ((x (grid-x g))
		(y (grid-y g))
		(old-x cur-x)
		(old-y cur-y)
		)

	    ;; we can't really go here, use last location
	    (unless (cave-floor-bold? dungeon x y)
	      (return-from follow-path nil))

	    ;; advance one step
	    (setq cur-x x
		  cur-y y)

	    (when (and (not blind-player)
		       (viewport-contains? *player* cur-x cur-y)
		       (player-has-los-bold? dungeon cur-x cur-y))

	      (display-moving-object dungeon cur-x cur-y
				     (get-moving-text-sym obj old-x cur-x old-y cur-y)
				     (get-moving-gfx-sym obj old-x cur-x old-y cur-y)))
	    

	    
	    (when (player-is-at? *player* x y)
	      (return-from follow-path nil))
	    
	    ;; we can stop if there was a monster
	    (when-bind (monsters (cave-monsters dungeon x y))
	      (return-from follow-path nil))
	    ))

    #+timer-code
    (let ((evt (make-thrown-visual *map-frame* obj pvx pvy cur-x cur-y)))

      (setf (display-x obj) pvx
	    (display-y obj) pvy)

      (push evt *visevents*))

    #-timer-code
    (van-thrown-hitting-target source dungeon cur-x cur-y obj)
    
    (when obj ;; might be crushed
      (drop-near-location! variant dungeon obj cur-x cur-y)
      (on-drop-object variant player obj))
    
    (ask-for-update! player '[bonuses])
    (ask-for-update! player '[torch])

    t))

#+timer-code
(defmethod finalise-visual-event ((evt thrown-object-movement) dungeon player cur-tick)
  (declare (ignorable dungeon player))
  
  (let* ((obj (visevent.object evt))
	 (variant *variant*)
	 (x (location-x obj))
	 (y (location-y obj)))
    
    ;;(warn "~a was thrown to ~s,~s" obj x y)


    
    (setf (visevent.mode evt) :dead))
  
  evt)



;; move somewhere else later
(defmethod shoot-a-missile ((dungeon dungeon) source destination
			    missile-weapon
			    (arrow active-object/ammo)
			    &key (range nil))
  
  (block missile-shooting
      
    ;;(warn "shoot ~s with ~s from ~s to ~s with range ~s" arrow missile-weapon source destination range)

    (multiple-value-bind (tx ty)
	(get-destination-coords source destination 99)
      
      (let* ((pvx (location-x source))
	     (pvy (location-y source))
	     (blind-player (is-blind? *variant* *player*))
	     (max-range (cond ((positive-integer? range) range)
			      ((typep missile-weapon 'active-object/bow)
			       (+ 10 (* 5 (object.multiplier (aobj.kind missile-weapon)))))
			      (t 10)))
	     (path-arr (make-array (1+ max-range) :fill-pointer 0))
	     (path-len (project-path dungeon max-range path-arr pvx pvy tx ty 0))
	     (cur-x pvx)
	     (cur-y pvy)
	     )

	(declare (ignore path-len))

	;; we follow the path to the end, and the end is our target
	(loop named follow-path
	      for g across path-arr
	      do
	      (let ((x (grid-x g))
		    (y (grid-y g))
		    (old-x cur-x)
		    (old-y cur-y)
		    )

		(unless (cave-floor-bold? dungeon x y)
		  (return-from follow-path nil))
		
		(setq cur-x x
		      cur-y y)


		(when (and (not blind-player)
			   (viewport-contains? *player* cur-x cur-y)
			   (player-has-los-bold? dungeon cur-x cur-y))

		  (display-moving-object dungeon cur-x cur-y
					 (get-moving-text-sym arrow old-x cur-x old-y cur-y)
					 (get-moving-gfx-sym arrow old-x cur-x old-y cur-y)))
					 
		  
		;; not sure on this one
		(when (player-is-at? *player* x y)
		  (return-from follow-path nil))
		  
		(when-bind (monsters (cave-monsters dungeon x y))
		  (return-from follow-path nil))
		
		))

	#+timer-code
	(let* ((obj arrow)
	       (evt (make-missile-visual *map-frame* obj pvx pvy cur-x cur-y)))
	  (setf (visevent.shooter evt) source
		(visevent.missile-weapon evt) missile-weapon)
	  (setf (display-x obj) pvx
		(display-y obj) pvy)
	  (push evt *visevents*))

	#-timer-code
	(van-missile-hitting-target source dungeon cur-x cur-y missile-weapon arrow)
	
	
	;; if it crashes in a wall, your arrow is gone.
	(when (cave-floor-bold? dungeon cur-x cur-y)
	  (when (> (aobj.number arrow) 1)
	    ;;(warn "Strip out one arrow")
	    (setf arrow (create-aobj-from-id (get-id arrow))))
	  (drop-near-location! *variant* dungeon arrow cur-x cur-y))

	))))

#+timer-code
(defun make-missile-visual (win arrow sx sy tx ty &key (speed nil))

  (let ((diff-x (- tx sx))
	(diff-y (- ty sy))
	(evt nil))

    (setf evt (make-instance 'missile-movement
		 :id "missile"
		 :object arrow
		 :window (cond ((is-window? win)
				win)
			       ((non-negative-integer? win)
				(aref *windows* win))
			       (t
				(error "Unknown window ~s" win)))
		 
		 :current-x sx
		 :current-y sy
		 :blocking? t
		 :move-speed (if (positive-integer? speed)
				 speed
				 +missile-speed+)
		 :source-x sx
		 :source-y sy
		 :target-x tx
		 :target-y ty
		 :angle (atan diff-y diff-x)
		 ))
    
    ;; we should find gfx-sym and text-sym depending on angle    
    (when-bind (vis (get-visual-projectile arrow))
      (setf (slot-value arrow 'text-sym) (aref (projectile.text-path vis)
					 (get-direction-from-coord-diff diff-x diff-y)))
      (setf (slot-value arrow 'gfx-sym) (aref (projectile.gfx-path vis)
					      (get-direction-from-coord-diff diff-x diff-y))))

    evt))


#+timer-code
(defmethod finalise-visual-event ((evt missile-movement) dungeon player cur-tick)
  (declare (ignorable dungeon player))

  (let ((arrow (visevent.object evt)))
		  
    ;; what did it hit?
    (van-missile-hitting-target (visevent.shooter evt) dungeon
				(location-x arrow) (location-y arrow)
				(visevent.missile-weapon evt)
				arrow)

    (setf (slot-value arrow 'gfx-sym) nil) 
    (setf (visevent.mode evt) :dead)
  
    evt))

(defmethod get-ranged-attack-skill ((variant vanilla-variant) (creature active-monster))
  (let ((power (get-power-lvl creature)))
    (when (not (plusp power))
      (setf power 1))
    ;; 30 is just a hack, half of a typical attack
    (+ 30 (* power 3))))

;; doesn't check for special arrows, or critical hits, etc. 
(defmethod missile-inflict-damage! ((attacker player) (target active-monster)
				    (miss-wpn active-object/bow) (missile active-object/ammo))

  (let ((dmg (roll-dice (get-number-of-damage-dice missile)
			(get-damage-dice missile))))
    
    (incf dmg (get-damage-modifier missile))
    (incf dmg (get-damage-modifier miss-wpn))
    
    (when-bind (bowkind (aobj.kind miss-wpn))
      (setf dmg (* dmg (object.multiplier bowkind))))
    
    (when (< dmg 1) (setf dmg 1)) ;; minimum damage
    
    (deduct-hp! target dmg)
    
    dmg))

(defmethod missile-inflict-damage! ((attacker active-monster) target wpn missile)
  ;;(warn "Inflict bloody damage on ~s ~s ~s" target wpn missile)
  (declare (ignore missile))
  (cond ((typep wpn 'ranged-spab)
	 (let ((dmg (roll-dice (spab.power wpn) 6)))
	   (deduct-hp! target dmg)
	   dmg))
	(t
	 (warn "Don't know how to handle missile damage for weapon ~s" wpn)
	 nil)))

;; very hackish code..
(defun interactive-fire-a-missile (dungeon player)
  "Hackish shoot-code."

  (block missile-shooting
    (let ((the-bow (get-missile-weapon player))
	  (the-missile nil))
      (flet ((sel-fun (table idx x)
	       (declare (ignore table idx))
	       (when (typep x 'active-object/ammo)
		 (let ((bowid (get-id (aobj.kind the-bow)))
		       (ammoid (get-id (aobj.kind x))))
		   ;; very very hackish
		   (cond ((or (equal bowid "long-bow")
			      (equal bowid "short-bow"))
			  (or (equal ammoid "arrow")
			      (equal ammoid "seeker-arrow")))
			 ((or (equal bowid "light-xbow")
			      (equal bowid "heavy-xbow"))
			  (or (equal ammoid "bolt")
			      (equal ammoid "seeker-bolt")))
			 ((or (equal bowid "sling")
			      (equal bowid "staff-sling"))
			  (or (equal ammoid "round-pebble")
			      (equal ammoid "iron-shot")))
			 (t nil))
		   ))))
	(unless (and the-bow (typep the-bow 'active-object/bow))
	  (print-message! "You have no missile weapon!")
	  (return-from missile-shooting nil))
	
	(setq the-missile (select-and-return-item
			   dungeon player '(:backpack :floor)
			   :prompt "Select missile:"
			   :selection-function #'sel-fun
			   :where :backpack))
	
	
	(cond ((and the-missile (typep the-missile 'active-object/ammo))
	       (when-bind (dir (get-aim-direction))
		 (assert (and (numberp dir) (< dir 10)))
		 (shoot-a-missile dungeon player dir the-bow the-missile)))
	      (t
	       (print-message! "No missile selected!")))
	))
    ))

;; move to variant
(defmethod get-power-of-attack ((variant vanilla-variant) kind)
  (ccase kind
    ;; these are not too common below 1000'
    (<eat-item> 5)
    (<eat-food> 5)
    (<eat-light> 5)
    (<un-power> 15)
    (<un-bonus> 20)
    (<exp-10> 5)
    (<exp-20> 5)
    (<exp-80> 5)
    (nil nil)
    ;; the rest should be defined in variant  (combat.lisp)
    ))

(defmethod kill-target! ((variant vanilla-variant) dungeon attacker (target active-monster) x y)

  (let* ((retval (call-next-method))
	 (coord (cave-coord dungeon x y))
	 (fl-type (floor.id (coord.floor coord))))

    ;; if this grows more complex, use a hash-table
    (cond ((equal fl-type "room-floor")
	   (setf (coord-floor coord) "1-blood-room-floor"))
	  
	  ((equal fl-type "normal-floor")
	   (setf (coord-floor coord) "1-blood-normal-floor"))
	  
	  ((equal fl-type "1-blood-normal-floor")
	   (setf (coord-floor coord) "2-blood-normal-floor"))
	  
	  ((equal fl-type "1-blood-room-floor")
	   (setf (coord-floor coord) "2-blood-room-floor"))

	  ((equal fl-type "2-blood-normal-floor")
	   (setf (coord-floor coord) "3-blood-normal-floor"))
	  
	  ((equal fl-type "2-blood-room-floor")
	   (setf (coord-floor coord) "3-blood-room-floor"))

	  ((equal fl-type "3-blood-normal-floor")
	   (setf (coord-floor coord) "4-blood-normal-floor"))
	  
	  ((equal fl-type "3-blood-room-floor")
	   (setf (coord-floor coord) "4-blood-room-floor"))
	  )
    
    retval))

(defun is-legal-slay? (variant slay)
  "Checks if slay is a known/legal slay for vanilla."
  (declare (ignore variant))
  (find slay '(<animal> <demon> <dragon> <evil> <giant> <orc> <troll> <undead>)))
