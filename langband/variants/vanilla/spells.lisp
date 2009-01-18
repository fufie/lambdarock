;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/spells.lisp - spell-effects
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)


;; hack
(defmacro spell-effect (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))


(defmethod calculate-creature-mana! ((variant vanilla-variant) (player player))

  (unless (is-spellcaster? player)
    (setf (current-mana player) 0
	  (maximum-mana player) 0)
    (return-from calculate-creature-mana! nil))

  (let* ((old-mana (maximum-mana player))
	 (pl-class (player.class player))
	 (pl-lvl (player.power-lvl player))
	 (pl-eqp (player.equipment player))
	 (magic-level (+ 1 (- pl-lvl (class.spells-at-level pl-class))))
	 (stats (variant.stats variant))
	 (stat-obj (find (class.spell-stat pl-class) stats :key #'stat.symbol))
	 (new-mana 0))
    
    ;; no negative value
    (when (minusp magic-level)
      (setf magic-level 0))

    (when stat-obj
      (let* ((stat-val (aref (player.active-stats player) (stat.number stat-obj)))
	     (half-mana (get-stat-info stat-obj stat-val :half-mana)))

	;; hack
	(setf new-mana (int-/ (* half-mana magic-level) 2))
	(when (plusp new-mana)
	  (incf new-mana))

	))

    ;; check if gloves screw up mana
    (when (find '<cumbered-by-gloves> (class.abilities pl-class))
      (when-bind (gloves (item-table-find pl-eqp 'eq.glove))
	;;(warn "Wearing gloves ~s from mana ~s" gloves new-mana)
	;; add free action check
	(setf new-mana (int-/ (* new-mana 3) 4))
	))

    (let ((allowed-weight (class.max-armour-weight pl-class))
	  (weight 0))
      (unless (minusp allowed-weight) ;; anything allowed
	(dolist (i '(eq.armour eq.cloak eq.shield eq.head eq.glove eq.feet))
	  (when-bind (obj (item-table-find pl-eqp i))
	    (incf weight (object.weight obj))))

	(let ((factor (int-/ (- weight allowed-weight) 10)))
	  ;;(warn "Weight ~s and factor ~s" weight factor)
	  (when (plusp factor)
	    (decf new-mana factor)))

	))

    (when (minusp new-mana) ;; never negative
      (setf new-mana 0))
    
    (setf (maximum-mana player) new-mana)

    (when (> (current-mana player) new-mana)
      (setf (current-mana player) new-mana))
    
    (when (/= old-mana new-mana)
      (ask-for-redraw! player '[mana]))
    
    t))


(defun define-spell (name id &key effect-type effect numeric-id)
  "Defines and registers a new spell."

  (assert (stringp name))
  (assert (stringp id))

  (let ((variant *variant*)
	(spell (make-instance 'magic-spell :name name :id id)))

    (when (integerp numeric-id)
      (setf (spell.numeric-id spell) numeric-id))
    
    (when (and effect (functionp effect))
      (setf (spell.effect spell) (compile nil effect)))

    ;; checking carefully
    (when effect-type
      (unless (is-legal-effect-type? effect-type)
	(warn "Unknown spell-type ~s for spell ~s" effect-type id))

      (when-bind (lookup (gethash effect-type (variant.visual-effects variant)))
	;;(warn "spell lookup is ~s" lookup)
	(setf (spell.effect-type spell) lookup)))
    
    
    ;; register spell in variant
    (multiple-value-bind (value present-p)
	(gethash (spell.id spell) (variant.spells variant))
      (when present-p
	(warn "Replacing spell ~s in vanilla variant" value))
      (setf (gethash (spell.id spell) (variant.spells variant)) spell))
 
    
    spell))

(defmethod get-visual-projectile ((obj magic-spell))
  (spell.effect-type obj))

(defun create-spellbook (name id spells)
  "Creates and returns a spellbook."

  (check-type name string)
  (assert (verify-id id))
  (assert (consp spells))
	  
  (let* ((variant *variant*)
	 (len (length spells))
	 (book (make-instance 'spellbook :name name :id id :size len)))

    (setf (spellbook.spells book) (make-array len :initial-element nil))

    (loop for i from 0
	  for spell in spells
	  do
	  (let ((spell-obj (gethash spell (variant.spells variant))))
	    (cond ((and spell-obj (typep spell-obj 'magic-spell))
		   (setf (aref (spellbook.spells book) i) spell-obj))
		  (t
		   (warn "Unable to find spell ~s in vanilla-variant" spell))
		  )))
    book))

(defun register-spellbook& (variant book)
  "Registers spellbook in variant and returns the book."
  (multiple-value-bind (value present-p)
      (gethash (spellbook.id book) (variant.spellbooks variant))
    (when present-p
      (warn "Replacing spellbook ~s in vanilla variant" value))
    (setf (gethash (spellbook.id book) (variant.spellbooks variant)) book))
  
  book)

(defun define-spellbook (name id &key (size 6) (spells nil))
  "Defines and registers a spellbook, should be done after the spells have been
made."

  (declare (ignore size))
  (register-spellbook& *variant* (create-spellbook name id spells)))

(defmethod get-effect-type (obj)
  (declare (ignore obj))
  nil)

(defmethod get-effect-type ((obj magic-spell))
  (spell.effect-type obj))

(defmethod get-effect-type ((obj active-object/wand))
    (object.effect-type (aobj.kind obj)))

(defmethod get-effect-type ((obj active-object/rod))
    (object.effect-type (aobj.kind obj)))

(defmethod get-effect-type ((obj active-object/staff))
    (object.effect-type (aobj.kind obj)))


(defun %simple-projection (source destination flag effect damage &key projected-object)
  "Destination can be a target or a direction.  A direction of 5 is interpreted
as target."
  (let ((sound (get-effect-type projected-object)))

    (when (typep sound 'visual-projectile)
      (setf sound (projectile.id sound)))
    ;;(warn "sound is ~s" sound)

    (bit-flag-add! flag +project-through+) ;; go ahead
    
    (multiple-value-bind (dest-x dest-y)
	(get-destination-coords source destination)
      (do-projection source dest-x dest-y flag :effect effect :damage damage
		     :projected-object projected-object :sound sound))
  ))

(defun project-on-los-monsters (source effect damage &key projected-object)
  "Projects an effect on all monsters that are in LOS of the source."

  (let (;;(flag #.(logior +project-kill+ +project-jump+ +project-hide+))
	(dungeon *dungeon*)
	(obvious nil)
	(loc-x 0)
	(loc-y 0)
	(sound nil))

    (dolist (i (dungeon.monsters dungeon))
      (setf loc-x (location-x i)
	    loc-y (location-y i))
      (when (and (creature-alive? i)
		 (player-has-los-bold? dungeon loc-x loc-y))
	;;(warn "project on ~s" i)
	(setf sound (get-effect-type projected-object))
	(when (typep sound 'visual-projectile)
	  (setf sound (projectile.id sound)))

	(when sound
	  (play-sound sound))

	;; shortcutting past do-projection
	(when (apply-projection-effect-to-target! *variant* source i
						  :x loc-x
						  :y loc-y
						  :effect effect
						  :damage damage :distance 0)
	  (setf obvious t))))
      
    obvious))

(defun van-fire-beam! (source destination effect damage
		       &key projected-object (flag 0))
  "Fires a beam in a direction."
  (let ((real-flag (logior +project-kill+ +project-beam+ flag)))
    (%simple-projection source destination real-flag effect damage
			:projected-object projected-object)))

(defun van-fire-bolt! (source destination effect damage
		       &key projected-object (flag 0))
  "Fires a bolt in a direction."
  (let ((real-flag (logior +project-kill+ +project-stop+ flag)))
    (%simple-projection source destination real-flag effect damage
			:projected-object projected-object)))


(defun van-fire-bolt-or-beam! (source beam-chance destination
			       effect damage &key projected-object)
  "Will fire a beam if beam-chance/100 happens, otherwise a bolt."
  (cond ((< (random 100) beam-chance)
	 (van-fire-beam! source destination effect damage
			 :projected-object projected-object))
	(t
	 (van-fire-bolt! source destination effect damage :projected-object
			 projected-object))))

(defun van-fire-ball! (source destination effect damage radius &key projected-object)
  "Fires a ball in a direction."
  (let ((flag (logior +project-kill+ +project-grid+ +project-stop+ +project-item+)))

    (multiple-value-bind (dest-x dest-y)
	(get-destination-coords source destination 99)
      (do-projection source dest-x dest-y flag
		     :effect effect :damage damage
		     :radius radius
		     :projected-object projected-object)
      )))

(defun van-breath! (source destination effect damage radius &key projected-object)
  "Fires a ball in a direction."
  (let ((flag (logior +project-kill+ +project-stop+ +project-item+)))

    (multiple-value-bind (dest-x dest-y)
	(get-destination-coords source destination 99)
      (do-projection source dest-x dest-y flag
		     :effect effect :damage damage
		     :radius radius
		     :projected-object projected-object)
      )))


  
(defun light-room! (dungeon x y &key (type '<light>))
  "Lights the room."
  (let ((coords (lb-ds:make-queue))
	(as-list nil))
    (flet ((add-coord (bx by)
	     (let ((flag (cave-flags dungeon bx by)))
	       ;; no recursion
	       ;;(warn "flag at ~s,~s is ~s" bx by flag)
	       (when (or (bit-flag-set? flag +cave-temp+)
			 ;; don't leave the room
			 (not (bit-flag-set? flag +cave-room+)))
		 (return-from add-coord))

	       (bit-flag-add! (cave-flags dungeon bx by) +cave-temp+)
	       ;;(warn "adding ~s ~s" bx by)
	       (lb-ds:enqueue (cons bx by) coords))))

      ;; add first grid
      (add-coord x y)

      (dolist (i (lb-ds:queue-as-list coords))
	(let ((cx (car i))
	      (cy (cdr i)))
	  (when (cave-floor-bold? dungeon cx cy)
	    ;; next to
	    (add-coord (1+ cx) cy)
	    (add-coord (1- cx) cy)
	    (add-coord cx (1+ cy))
	    (add-coord cx (1- cy))

	    ;; diagonal
	    (add-coord (1+ cx) (1+ cy))
	    (add-coord (1- cx) (1- cy))
	    (add-coord (1+ cx) (1- cy))
	    (add-coord (1- cx) (1+ cy))
	    ))))

    ;;(warn "coords ~s" coords)

    (setf as-list (lb-ds:queue-as-list coords))

    ;;(warn "Darken ~s" as-list)
    
    (dolist (i as-list)
      (let ((flag (cave-flags dungeon (car i) (cdr i))))
	;;(warn "~a changing ~s ~s from ~s" type (car i) (cdr i) flag)
	(bit-flag-remove! flag +cave-temp+)
	(ecase type
	  (<light>
	   (bit-flag-add! flag +cave-glow+))
	  (<darkness>
	   (bit-flag-remove! flag #.(logior +cave-mark+ +cave-glow+))))
	;;(warn "~a changing ~s to ~s" type (cave-flags dungeon (car i) (cdr i)) flag)
	(setf (cave-flags dungeon (car i) (cdr i)) flag)))

    ;; redraw things
    (ask-for-update! *player* '[forget-view])
    (ask-for-update! *player* '[update-view])
    (update-stuff *variant* dungeon *player*)

    (dolist (i as-list)
      (light-spot! dungeon (car i) (cdr i)))
    
    t))


(defun light-area! (dungeon source x y damage radius &key (type '<light>)
		    projected-object)
  "Lights the area."

  ;; unless blind
  (let ((blind-player (is-blind? *variant* *player*)))
    
    (unless blind-player
      (ecase type
	(<light>
	 (print-message! "You are surrounded by a white light."))
	(<darkness>
	 (print-message! "Darkness surrounds you!"))
	))
    
    (do-projection source x y (logior +project-grid+ +project-kill+)
		   :damage damage
		   :radius radius
		   :effect (get-spell-effect type)
		   :projected-object projected-object)
    (light-room! dungeon x y :type type))
  
  t)


(defun enchant-item! (dungeon player &key (type '<weapon>) (bonus 1) (restrict nil))

  (flet ((%local-enchant (item)
	   (warn "enchant ~s ~s ~s ~s" item type bonus restrict)
	   ;; improve later
	   (ecase type
	     (<weapon>
	      (when (< (get-tohit-modifier item) +10)
		(incf (get-tohit-modifier item) bonus)
		(incf (get-damage-modifier item) bonus)
		:used))
	       
	       (<armour>
		(when (< (get-armour-modifier item) +10)
		  (incf (get-armour-modifier item) bonus)
		  :used)))))
  
    (let ((retval :still-useful)
	  (selection (select-item dungeon player '(:backpack :equip)
				  :prompt "Enchant item: "
				  :where :backpack)))

      (cond (selection
	     (let* ((the-table (get-item-table dungeon player (car selection)))
		    (removed-obj (item-table-remove! the-table (cdr selection))))
	       (cond (removed-obj
		      (format-message! "~a ~a glow~a brightly." "The" "[some-object, FIX]" "s")
		      (setf retval (%local-enchant removed-obj))
		    
		      (item-table-add! the-table removed-obj))
		     (t
		      (warn "Did not find selected obj ~a" selection)))))
	    (t
	     (warn "Did not select anything.")))
    

      retval)))

(defun get-spell-id (spell)
  (etypecase spell
    (string spell)
    (spell-classdata (spell.id spell))
    (magic-spell (spell.id spell))))
  
(defun get-spell-data (player spell)
  "Tries to return SPELL-CLASSDATA for a given MAGIC-SPELL object.
Will return NIL if the spell is not usable (in any way) for the player."
  (when (is-spellcaster? player)
    (let ((spell-id (get-spell-id spell))
	  (spell-arr (class.spells (player.class player))))
      
      (when (vectorp spell-arr) ;; hack
	(loop for x across spell-arr
	      do
	      (when (equal spell-id (spell.id x))
		(return-from get-spell-data x))))
      
      nil)))


(defun learn-spell! (player spell)
  "Tries to ensure that the player learns the given spell."

  (unless (is-spellcaster? player)
    (print-message! "You are not a spellcaster and cannot learn spells.")
    (return-from learn-spell! nil))

;;  (warn "Trying to learn ~s" spell)
  
  (let ((spell-id (etypecase spell
		    (magic-spell (spell.id spell))
		    (spell-classdata (spell.id spell))
		    (string spell)))
	(learnt-spells (class.learnt-spells (player.class player))))

    (when (find spell-id learnt-spells :test #'equal)
      (print-message! "You already know the spell.")
      (return-from learn-spell! nil))

    (let ((spell-data (get-spell-data player spell-id)))
      (cond ((and (typep spell-data 'spell-classdata)
		  (<= (spell.level spell-data) (player.power-lvl player)))
	     (vector-push-extend spell-id learnt-spells)
	     (format-message! "~a learnt." (spell.name spell))
	     (ask-for-redraw! player '[study])
	     (return-from learn-spell! t))
	    
	    ((and (typep spell-data 'spell-classdata)
		  (> (spell.level spell-data) (player.power-lvl player)))
	     (print-message! "You're not powerful enough to learn that spell yet."))
	    
	    ((eq spell-data nil)
	     (print-message! "You are unable to learn that spell."))
	    
	    (t
	     (warn "Unknown value returned ~s, ~s." spell-data spell)))

      nil)))

   
	
(defun has-learnt-spell? (player spell)
  "Returns NIL if the player has not learnt the spell,
returns T if the player knows the spell."
  (let* ((spell-id (get-spell-id spell))
	 (learnt-spells (class.learnt-spells (player.class player)))
	 (existing-spell (find spell-id learnt-spells :test #'equal)))
;;    (warn "Checked for ~s in ~s" spell-id learnt-spells)
    (when existing-spell
      t)))

(defun can-learn-more-spells? (variant player)
  "Returns T if the player can learn spells, NIL otherwise."
  (when (is-spellcaster? player)
    (let* ((pl-class (player.class player))
	   (stats (variant.stats variant))
	   (stat-obj (find (class.spell-stat pl-class) stats :key #'stat.symbol))
	   )
      (when stat-obj
	(let* ((stat-val (aref (player.active-stats player) (stat.number stat-obj)))
	       (half-spells (get-stat-info stat-obj stat-val :half-spells))
	       (learnt-spells (class.learnt-spells pl-class))
	       (num-learnt (length learnt-spells))
	       (max-spells (int-/ (* (player.power-lvl player) half-spells) 2)))
	  
	  
	  ;;(warn "Max spells ~s vs learnt ~s" max-spells num-learnt)

	  (> max-spells num-learnt)))
      )))

(defun interactive-book-selection (dungeon player prompt)
  "Selects a book and returns it or NIL."
  (let ((the-table nil)
	(book nil)
	(selection (select-item dungeon player '(:backpack :equip :floor)
				:prompt prompt
				:where :backpack
				:selection-function #'(lambda (table key obj)
							(declare (ignore table key))
							;; maybe check priest vs mage
							(typep obj 'active-object/book))
				)))
    
    
    (cond (selection
	   (setf the-table (get-item-table dungeon player (car selection)))
	   (setf book (item-table-find the-table (cdr selection)))
	   book)
	  (t nil))))


(defun van-learn-spell! (dungeon player &key (variant *variant*))
  "Interactive selection of spell to learn."

  (unless (can-learn-more-spells? variant player)
    (print-message! "You cannot learn more spells at this level.")
    (return-from van-learn-spell! nil))
	   
  
  (block learn-spell

    (let ((book (interactive-book-selection dungeon player "Learn from which book")))

      (unless book
	(return-from learn-spell nil))
      
      (let* ((okind (aobj.kind book))
	     (book-id (get-id okind)))
	(when-bind (spell-info (gethash book-id (variant.spellbooks variant)))      
	  (when-bind (which-one (interactive-spell-selection player spell-info
							     :prompt "Learn which spell? "
							     :no-spell-msg "Cannot learn spells from this book."
							     :mode :learn
							     :selection-function
							     #'(lambda (book idx spell)
								 (declare (ignore book idx))
								 (let ((data (get-spell-data player spell)))
								   (and data
									(not (has-learnt-spell? player data))
									(<= (spell.level data)
									    (player.power-lvl player))
									)))
							     ))
	    (unless (and (integerp which-one) (>= which-one 0)
			 (< which-one (spellbook.size spell-info)))
	      (warn "Illegal choice ~s" which-one)
	      (return-from learn-spell nil))
	    
	    (let ((the-spell (aref (spellbook.spells spell-info) which-one)))
	      (learn-spell! player the-spell))
	    ))
	))
    
    (values)))





(defun van-invoke-spell! (dungeon player &key (variant *variant*))
  "Invokes a spell.. gee."

  (block cast-spell
    
    (when-bind (book (interactive-book-selection dungeon player "Cast spell from which book"))
      (let* ((okind (aobj.kind book))
	     (book-id (get-id okind))
	     (which-one 0))
	(when-bind (spell-info (gethash book-id (variant.spellbooks variant)))
	  (setf which-one (interactive-spell-selection player spell-info :mode :invoke
						       :selection-function
						       #'(lambda (book idx spell)
							   (declare (ignore book idx))
							   (let ((data (get-spell-data player spell)))
							     (and data
								  (has-learnt-spell? player data)
								  (>= (current-mana player) (spell.mana data)))))
						       ))
	  
	  (cond ((eq which-one nil)
		 (return-from cast-spell nil))
		((not (and (non-negative-integer? which-one) (< which-one (spellbook.size spell-info))))
		 (warn "Spell ~s not found." which-one)
		 (return-from cast-spell nil)))
	
	  ;; let us find the spell now.
	  (let* ((the-spell (aref (spellbook.spells spell-info) which-one))
		 (spell-effect (spell.effect the-spell))
		 (spell-data (get-spell-data player the-spell))
		 (learnt-spell (has-learnt-spell? player the-spell)))
	  
	    ;;(warn "Spell ~s: know (~s), learnt (~s)" the-spell spell-data learnt-spell)
	  
	    (unless (and spell-data learnt-spell)
	      (format-message! "You don't know the ~a spell." (spell.name the-spell))
	      (return-from cast-spell nil))
	  
	    (unless (>= (current-mana player) (spell.mana spell-data))
	      (print-message! "You don't have enough mana to cast that spell.")
	      (return-from cast-spell nil))
	  
	  
	    (cond ((and spell-effect (functionp spell-effect))
		   (funcall spell-effect dungeon player the-spell)
		   ;; deduct mana, better way?
		   (decf (current-mana player) (spell.mana spell-data))
		   (ask-for-redraw! player '[mana])
		   (unless (spell.tried spell-data)
		     ;;(warn "Tried spell ~s" (spell.id spell-data))
		     (setf (spell.tried spell-data) t)
		     (modify-xp! player (spell.xp spell-data)))
		 
		   )
		  (t
		   (format-message! "The ~a spell is not implemented yet." (spell.name the-spell))))
	    ))
      
	;; clean up some!
	;; (put-coloured-line! +term-white+ "" 0 0)
      
	(values)))))
      

(defun browse-spells (dungeon player &key (variant *variant*))
  "Interactive selection of spell to learn."

  (when-bind (book (interactive-book-selection dungeon player "Browse which spellbook"))
    (let* ((okind (aobj.kind book))
	   (book-id (get-id okind)))
      (when-bind (spell-info (gethash book-id (variant.spellbooks variant)))
	;;	  (warn "SI: ~s" spell-info)
	(with-dialogue ()
	  (let ((alts (display-spells player spell-info :mode :browse)))
	    (dolist (a alts)
	      (buttonify-selectable-ui-object *cur-win* a)))
	  (pause-last-line!))
	))))


(defmethod interactive-spell-selection (player spellbook
				    &key (prompt "Cast which spell? ")
				    prompt-frame
				    selection-function
				    (no-spell-msg "No castable spells.")
				    (mode :invoke))
  "Returns selection."
  (let ((printed-prompt nil)
	(place-len 0)
	(alts nil)
	(item-active (make-array (spellbook.size spellbook) :initial-element t))
	(legal-letters ""))

    
    (flet ((do-query (show-mode prompt-win display-win)
	     
	     
	     (setf place-len (spellbook.size spellbook))

	     (when (functionp selection-function)
	       (dotimes (i place-len)
		 (setf (aref item-active i) (funcall selection-function spellbook i
						     (aref (spellbook.spells spellbook) i)))))

	     (setf legal-letters (org.langband.engine::%get-legal-letters item-active))
	       
	     (cond ((= 0 (length legal-letters))
		    (setf printed-prompt no-spell-msg))
		   (t
		    (setf printed-prompt (format nil "(Spells ~a~a, ESC) ~a" legal-letters
						 (if (not show-mode)
						     ", * or right-click to see"
						     "") prompt))))
	       
	     (let ((*cur-win* prompt-win))
	       (put-coloured-line! +term-white+ printed-prompt 0 0))
	       
	     ;; sick
	     (when (equal printed-prompt no-spell-msg)
	       (return-from interactive-spell-selection nil))
	       
	     (when show-mode
	       (let ((*cur-win* display-win))
		 (setf alts (display-spells player spellbook :mode mode :selection-function selection-function))
		 (dolist (a alts)
		   (buttonify-selectable-ui-object display-win a))))

	     ;; ensure that the framebuffer is flipped (really needed?)
	     (org.langband.ffi:c-flip-framebuffer)

	     (let ((selection (select-displayed-alternative alts)) ;;(read-one-character))
		   (*cur-win* prompt-win))
	       
	       (cond ((eql selection +escape+)
		      ;; clear prompt and get out
		      (clear-row prompt-win 0 0)
		      (return-from interactive-spell-selection nil))
		     
		     ((eql selection #\*)
		      (return-from do-query :show))

		     ((alpha-char-p selection)
		      (put-coloured-line! +term-white+ "" 0 0)
		      (let ((num (a2i selection)))
			(when (< num (spellbook.size spellbook))
			  (return-from interactive-spell-selection num))))
		     (t
		      (warn "Fell through spell-selection with choice: ~s" selection)))
	       
	       (put-coloured-line! +term-white+ "" 0 0)
	       )))

      
      ;; hackish, first try a query in the message-frame, if show-mode is asked for
      ;; jump to the loop embedded in a with-dialogue so that we stay there
      (block read-loop
	(let ((prompt-win (cond ((non-negative-integer? prompt-frame)
				 (aref *windows* prompt-frame))
				((typep prompt-frame 'window)
				 prompt-frame)
				(t
				 (aref *windows* +query-frame+))))
	      (display-win (aref *windows* +dialogue-frame+))
	      (show-mode nil))
	  (loop until show-mode
		do (when (eq :show (do-query nil prompt-win display-win))
		     (setf show-mode t)))
	  
	  (with-dialogue ()
	    (loop (do-query t display-win display-win)))
	  ))
      )))

(defmethod interactive-spell-selection :after (player spellbook
						      &key (prompt "Cast which spell? ")
						      prompt-frame
						      selection-function
						      (no-spell-msg "No castable spells.")
						      (mode :invoke))
  
  (declare (ignore player spellbook prompt no-spell-msg mode selection-function))
  
  ;; just clears the prompt
  (let ((prompt-win (cond ((non-negative-integer? prompt-frame)
			   (aref *windows* prompt-frame))
			  ((typep prompt-frame 'window)
			   prompt-frame)
			  (t
			   (aref *windows* +query-frame+)))))

    (let ((*cur-win* prompt-win))
      (put-coloured-line! +term-white+ "" 0 0))))



(defun display-spells (player spellbook &key (x 20) (y 1) (mode :browse) selection-function)
  "Displays a spell-list.  Prints all the surrounding data and returns the clickable
spells as ui-objects that can be displayed by the event-loop itself."
  ;; l-blue for unknown
  ;; l-green for untried
  ;; white for normal/tried
  ;; red for difficult
  ;; l-dark for unreadable
  (let ((colour +term-white+)
	(comment "")
	(spells (spellbook.spells spellbook))
	(alts '()))

    (put-coloured-line! +term-white+ "" x y)
    (put-coloured-line! +term-white+ "" x (1+ y))
    
    (put-coloured-str! +term-l-green+ (format nil "Book: ~a" (spellbook.name spellbook)) (+ x 8) y)
    (incf y)
    (put-coloured-str! +term-white+ "Name" (+ x 5) y)
    (put-coloured-str! +term-white+ "Lv Mana Fail Info" (+ x 35) y)
    
    (loop for i from 0
	  for spell across spells
	  do
	  (let ((spell-data (get-spell-data player spell))
		(row (+ y i 1)))

	    (cond ((not spell-data)
		   (put-coloured-line! +term-l-dark+ (format nil "  ~a) ~30a" (i2a i) "<unreadable>")
				       x row))
		  (t
		   (let ((base-level (spell.level spell-data))
			 (base-mana (spell.mana spell-data))
			 (base-fail (spell.failure spell-data))
			 (spell-tried (spell.tried spell-data))
			 (learnt-it (has-learnt-spell? player spell-data))
			 )
		     
		     ;; we have the spell readable at least
		     (cond ((< (player.power-lvl player) base-level)
			    (setf colour +term-red+
				  comment (if (eq mode :learn)
					      "too difficult"
					      "difficult")))

			   ((not learnt-it)
			    (setf colour +term-l-blue+
				  comment (if (eq mode :learn)
					      "can learn"
					      "unknown")))

			   ((and (eq mode :invoke)
				 (< (current-mana player) base-mana))
			    (setf colour +term-violet+)
			    (setf comment "low mana"))

			   
			   ((not spell-tried)
			    (setf colour +term-l-green+
				  comment "untried"))
			   (t))
		   
		     (let ((str (format nil "  ~a) ~30a~2d ~4d ~3d% ~a"
					(i2a i) (spell.name spell) base-level base-mana base-fail comment
					))
			   (show-me (when selection-function (funcall selection-function spellbook i spell))))

		       (if show-me
			   (push (make-selectable-ui-object (i2a i) x row (+ x (length str)) row
							    :text str :text-colour colour
							    :button-colour :matte-brown) alts)
			   (put-coloured-line! colour str x row))
		       )))
		  )))
    
    (put-coloured-line! +term-white+ "" x (+ (spellbook.size spellbook) y  1))
    
    (nreverse alts)))

     

(defun teleport-creature! (dungeon player creature range)
  "Teleports the creature some distance."
  (unless (positive-integer? range)
    (warn "Invalid argument to teleport-creature: ~s" range)
    (return-from teleport-creature! nil))

  (let* ((minimum (floor range))
	 (cx (location-x creature))
	 (cy (location-y creature))
	 (tx cx)
	 (ty cy)
	 (cur-d range))
    (block find-grid
      (loop
       (when (> range 200)
	 (setf range 200))
       
       (block legal-dist
	 (dotimes (i 500)
	   (setf tx (rand-spread cx range)
		 ty (rand-spread cy range))
	   (setf cur-d (distance cx cy tx ty))
	   (when (and (>= cur-d minimum) (<= cur-d minimum))
	     (return-from legal-dist))))
       
       (when (and (in-bounds-fully? dungeon tx ty)
		  (can-place? dungeon tx ty :creature)
		  (not (cave-icky? dungeon tx ty)))
	 (return-from find-grid))
       
       (setf range (* 2 range)
	     minimum (floor minimum 2))))

    ;; we found an ok spot!
    (assert (and (in-bounds-fully? dungeon tx ty)
		 (can-place? dungeon tx ty :creature)
		 (not (cave-icky? dungeon tx ty))))

    ;; sound

    ;; swap monster
    (swap-monsters! dungeon player cx cy tx ty)

    ))

(defun summon-monster (dungeon x y depth &key (type :any))
  "Returns T if it summoned a monster successfully."
  (declare (ignore type depth))
;;  (warn "summoning at (~s,~s) type ~s" x y type)

  ;; we ignore type now, and fix that later.

  (let ((variant *variant*)
	(player *player*)
	(retval t))
    (loop for i from 1 to 10
	  do
	  (let ((fx (+ (randint i) x))
		(fy (+ (randint i) y))) ;; hack
	    (when (cave-empty-bold? dungeon fx fy) 
	      (place-monster! variant dungeon player fx fy nil nil)
	      (setf retval t))
	    ))
    retval))


(defun detect-invisible! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  (let ((detected-any nil))
    (apply-effect-to-area dungeon
			  (- (location-x player) radius)
			  (- (location-y player) radius)
			  (* radius 2) (* radius 2)
			  #'(lambda (coord x y)
			      (declare (ignore x y))
			      (when-bind (monsters (coord.monsters coord))
				(dolist (i monsters)
				  (when (has-ability? (amon.kind i) '<invisible>)
				    (add-monster-knowledge-flag! player i '<invisible>)
				    (bit-flag-add! (amon.vis-flag i)
						   #.(logior +monster-flag-mark+ +monster-flag-show+))
				    (setf detected-any t)
				    (update-monster! *variant* i nil))))))
    (when detected-any
       (print-message! "You detect invisible creatures!"))
    
    detected-any))

(defun is-evil? (monster)
  (etypecase monster
    (active-monster (is-evil? (amon.kind monster)))
    (monster-kind (eq '<evil> (monster.alignment monster)))))


(defun detect-evil-monsters! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  (let ((detected-any nil))
    (apply-effect-to-area dungeon
			  (- (location-x player) radius)
			  (- (location-y player) radius)
			  (* radius 2) (* radius 2)
			  #'(lambda (coord x y)
			      (declare (ignore x y))
			      (when-bind (monsters (coord.monsters coord))
				(dolist (i monsters)
				  (when (is-evil? i)
				    (add-monster-knowledge-flag! player i '<evil>)
				    (bit-flag-add! (amon.vis-flag i)
						   #.(logior +monster-flag-mark+ +monster-flag-show+))
				    (setf detected-any t)
				    (update-monster! *variant* i nil))))))
    (when detected-any
       (print-message! "You sense the presence of evil creatures!"))
    
    detected-any))


(defun detect-monsters! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (declare (ignore x y))
			       (when-bind (monsters (coord.monsters coord))
				 (dolist (i monsters)
				   (unless (has-ability? (amon.kind i) '<invisible>)
				     (bit-flag-add! (amon.vis-flag i)
						    #.(logior +monster-flag-mark+ +monster-flag-show+))
				     (setf detected-any t)
				     (update-monster! *variant* i nil))))))
     (when detected-any
       (print-message! "You sense the presence of monsters!"))

     detected-any))

(defun detect-traps! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (declare (ignorable x y))
			       (when-bind (decor (coord.decor coord))
				 (when (is-trap? decor)
				   (bit-flag-add! (coord.flags coord) +cave-mark+)
				   (decor-operation *variant* decor :visible :value t)
				   (setf detected-any t)))
			       ))
     
				 
     (when detected-any
       (print-message! "You sense the presence of traps!"))

     detected-any))

(defun detect-doors! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (declare (ignorable x y))
			       (when-bind (decor (coord.decor coord))
				 (when (is-door? decor)
				   (bit-flag-add! (coord.flags coord) +cave-mark+)
				   (decor-operation *variant* decor :visible :value t)
				   (setf detected-any t)))
			       ))
     
				 
     (when detected-any
       (print-message! "You sense the presence of doors!"))

     detected-any))

(defun detect-stairs! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (when-bind (floor (coord.floor coord))
				 (when (and (typep floor 'floor-type)
					    (or (= (floor.numeric-id floor) 77)
						(= (floor.numeric-id floor) 78)))
				   (bit-flag-add! (coord.flags coord) +cave-mark+)
				   (light-spot! dungeon x y)
				   (setf detected-any t)))
			       ))
     
				 
     (when detected-any
       (print-message! "You sense the presence of stairs!"))

     detected-any))


(defun detect-gold! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (when-bind (objs (coord.objects coord))
				 (dolist (obj (items.objs objs))
				   (when (typep obj 'active-object/money)
				     (setf (aobj.marked obj) t)
				     ;;(bit-flag-add! (coord.flags coord) +cave-mark+)
				     (light-spot! dungeon x y)
				     (setf detected-any t)))
				 )))
     
				 
     (when detected-any
       (print-message! "You sense the presence of treasure!"))

     detected-any))

(defun detect-normal-objects! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (when-bind (objs (coord.objects coord))
				 (dolist (obj (items.objs objs))
				   (unless (typep obj 'active-object/money)
				     (setf (aobj.marked obj) t)
				     ;;(bit-flag-add! (coord.flags coord) +cave-mark+)
				     (light-spot! dungeon x y)
				     (setf detected-any t)))
				 )))
     
				 
     (when detected-any
       (print-message! "You sense the presence of objects!"))

     detected-any))

(defun detect-all! (dungeon player source &optional (radius +default-detect-radius+))
  (detect-traps! dungeon player source radius)
  (detect-doors! dungeon player source radius)
  (detect-stairs! dungeon player source radius)
  (detect-gold! dungeon player source radius)
  (detect-normal-objects! dungeon player source radius)
  (detect-invisible! dungeon player source radius)
  (detect-monsters! dungeon player source radius)
  t)

;; FIX for type '<powerful>
(defun interactive-identify-object! (dungeon player &key (type '<normal>))

  (block id-obj
    (let* ((limit-from '(:backpack :floor :worn))
	   (prompt "Identify which item? ")
	   (variant *variant*)
	   (selection (select-item dungeon player limit-from
				   :prompt prompt
				   :where (first limit-from))))
    
      (unless (and selection (consp selection))
	(return-from id-obj nil))

      (let* ((the-table (get-item-table dungeon player (car selection)))
	     (removed-obj (item-table-remove! the-table (cdr selection))))
	  
	(unless (and removed-obj (typep removed-obj 'active-object))
	  (return-from id-obj nil))

	(ecase type
	  (<normal>
	   (possible-identify! player removed-obj))
	  (<powerful>
	   (possible-identify! player removed-obj)
	   (learn-about-object! player removed-obj :fully-known)))

	;; put object back where it was found
	(put-object-in-container! dungeon player the-table removed-obj)

	(format-message! "Object is ~a."
			 (with-output-to-string (s)
			   (write-obj-description variant removed-obj s)))

	t))
    ))
      

(defmethod print-object ((inst magic-spell) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
           (spell.id inst)
           (spell.name inst)))
  inst)

(defmethod print-object ((inst spellbook) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
           (spellbook.id inst)
           (spellbook.name inst)))
  inst)

(defmethod print-object ((inst spell-classdata) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
           (spell.id inst)
           (spell.level inst)))
  inst)



;;(trace light-area! light-room!)
#||
;; clash with above?
(defun has-spell? (player spell)
  "Returns NIL if the player cannot have the spell. Returns the spell-info if it does."
  (when (is-spellcaster? player)
    (let ((spells (class.spells (player.class player)))
	  (spell-id (spell.id spell)))
      (loop for x across spells
	    do
	    (when (equal (spell.id x) spell-id)
	      (return-from has-spell? x)))
      nil)))
||#



(defun modify-creature-speed! (effect creature amount duration)
  "Tries to haste the creature."
  (assert (or (eq effect '<hasted>) (eq effect '<slowed>)))
  (etypecase creature
    ;; temporary for player
    (player (modify-creature-state! creature effect :add duration))
    (active-monster
     (let* ((target creature)
	    (speed (get-creature-speed target))
	    (max-speed 150)
	    (min-speed 60)
	    (calc (+ amount speed)))
	    
       (cond ((and (< min-speed calc)
		   (> max-speed calc))
	      (incf (get-creature-speed target) amount))
	     ((plusp amount)
	      (incf (get-creature-speed target) 2))
	     (t
	      (decf (get-creature-speed target) 2)))
       t))
    ))


(defun haste-creature! (creature amount duration)
  "Tries to haste the creature."
  (when (minusp amount)
    (setf amount (- amount)))
  (modify-creature-speed! '<hasted> creature amount duration))

(defun slow-creature! (creature amount duration)
  "Tries to haste the creature."
  (when (plusp amount)
    (setf amount (- amount)))
  (modify-creature-speed! '<slowed> creature amount duration))

;;(trace haste-creature!)

(defun remove-curse! (creature power)
  "Removes any curses on the creature."
  
  (unless (is-player? creature)
    (warn "Uncursing creature ~s" creature)
    (return-from remove-curse! nil))

  (let ((count 0))
  
  (flet ((do-curse (tbl key item)
	   (declare (ignore tbl key))
	   (when item
	     (when (uncurse-object! item power)
	       (incf count)))))
    
    (declare (dynamic-extent #'do-curse))
    
    (when-bind (inv (get-creature-inventory creature))
      (when-bind (container (aobj.contains inv))
	(item-table-iterate! container #'do-curse)))
    
    (when-bind (equ (player.equipment creature))
      (item-table-iterate! equ #'do-curse))
  
    count)))

(defun toggle-word-of-recall! (player)
  "Toggles word of recall, not activate it."

  (let* ((temp-attrs (player.temp-attrs player))
	 (already-recalling (get-attribute-value '<recalling> temp-attrs)))

    (cond (already-recalling
	   ;; then we cancel it quietly
	   (let ((attr (gethash '<recalling> temp-attrs)))
	     (setf (attr.duration attr) 0
		   (attr.value attr) nil)
	     (print-message! "A tension leaves the air around you...")))
	  (t
	   (print-message! "The air about you becomes charged...")
	   (modify-creature-state! player '<recalling> :add (+ 15 (random 20)))))

    t))

(defun floorify-coord! (dungeon x y)
  "Turns a dungeon spot into normal floor and removes any mark."

  (bit-flag-remove! (cave-flags dungeon x y) +cave-mark+)
  (setf (cave-floor dungeon x y) "normal-floor"
	(cave-decor dungeon x y) nil) ;; destroy decor as well
  t)

(defmethod polymorph-creature ((variant vanilla-variant)
			       (level random-level)
			       (original active-monster)
			       &key (boost 5))

  (unless (integerp boost)
    (setf boost 5))
  
  (let* ((target-power (get-power-lvl original))
	 (orig-kind (amon.kind original))
	 (min-lvl (min-cap (- target-power
			      (1+ (int-/ (randint 20) (randint 9)))) 1)) ;; always +1 in vanilla
	 (max-lvl (min-cap (+ target-power
			      (1+ (int-/ (randint 20) (randint 9)))) 2)) ;; at least have some risk
	 (wanted-depth (+ boost (int-/ (+ (level.depth level)
					  target-power) 2))))

    ;;(warn "Must find replacement for ~s between ~s and ~s from start-depth ~s"
    ;;original min-lvl max-lvl wanted-depth)
    
    (loop named find-new-kind
	  for i from 0 below 1000
	  do
	  (when-bind (kind (get-monster-kind-by-level variant level
						      :depth wanted-depth))
	    (when (and (not (eq orig-kind kind))
		       (>= (get-power-lvl kind) min-lvl)
		       (<= (get-power-lvl kind) max-lvl)
		       (not (is-unique-monster? kind)))
	      (return-from polymorph-creature kind))
	    ))
    nil))

(defmethod polymorph-creature ((variant vanilla-variant) (level van/town-level) (original active-monster)
			       &key (boost 5))
  (declare (ignore boost))
  ;;(setf boost 0) ;; no boost
  
  (let ((orig-kind (amon.kind original)))
    
    (loop for i from 0 below 1000
	  do
	  (when-bind (kind (get-monster-kind-by-level variant level
						      :depth (level.depth level)))
	    (when (and (not (eq kind orig-kind))
		       (not (is-unique-monster? kind)))
	      (return-from polymorph-creature kind))))
    nil))

;; unlike angband where the effect is bigger the bigger window you have, the
;; effect has fixed size here
(defun map-area! (dungeon player source &optional (radius +default-detect-radius+))
  (declare (ignore source))
  
   (let ((detected-any nil))

     (apply-effect-to-area dungeon
			   (- (location-x player) radius)
			   (- (location-y player) radius)
			   (* radius 2) (* radius 2)
			   #'(lambda (coord x y)
			       (declare (ignore x y))
			       ;; original only marks non-walls and then it checks
			       ;; area around the non-wall and marks any walls, this one
			       ;; marks everything nethack-style.
			       (when-bind (floor (coord.floor coord))
				 (when (typep floor 'floor-type)
				   (bit-flag-add! (coord.flags coord) +cave-mark+)
				   ;;(light-spot! dungeon x y)
				   (setf detected-any t)))
			       ))
     
				 
     (when detected-any
       (ask-for-redraw! player '[map]))       

     detected-any))
