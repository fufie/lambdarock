;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/effects.lisp - apply effects on stuff
Copyright (c) 2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defconstant +effect-file+ 4)

(defun gfx-bolt-array (offset)
  (let ((arr (make-array 10 :initial-element 0)))
    (setf (aref arr 1) (tile-paint-value +effect-file+ (+ 2 offset))
	  (aref arr 2) (tile-paint-value +effect-file+ offset)
	  (aref arr 3) (tile-paint-value +effect-file+ (+ 3 offset))
	  (aref arr 4) (tile-paint-value +effect-file+ (+ 1 offset))
	  (aref arr 6) (tile-paint-value +effect-file+ (+ 1 offset))
	  (aref arr 7) (tile-paint-value +effect-file+ (+ 3 offset))
	  (aref arr 8) (tile-paint-value +effect-file+ offset)
	  (aref arr 9) (tile-paint-value +effect-file+ (+ 2 offset)))
    arr))

(defun text-bolt-array (colour)
  (let ((arr (make-array 10 :initial-element 0)))
    (setf (aref arr 1) (text-paint-value colour #\/)
	  (aref arr 2) (text-paint-value colour #\|)
	  (aref arr 3) (text-paint-value colour #\\)
	  (aref arr 4) (text-paint-value colour #\-)
	  (aref arr 6) (text-paint-value colour #\-)
	  (aref arr 7) (text-paint-value colour #\\)
	  (aref arr 8) (text-paint-value colour #\|)
	  (aref arr 9) (text-paint-value colour #\/))
    arr))

(defun gfx-missile-array (offset)
  (let ((arr (make-array 10 :initial-element 0)))
    (setf (aref arr 1) (tile-paint-value +effect-file+ (+ 0 offset))
	  (aref arr 2) (tile-paint-value +effect-file+ (+ 1 offset))
	  (aref arr 3) (tile-paint-value +effect-file+ (+ 2 offset))
	  (aref arr 4) (tile-paint-value +effect-file+ (+ 3 offset))
	  (aref arr 6) (tile-paint-value +effect-file+ (+ 4 offset))
	  (aref arr 7) (tile-paint-value +effect-file+ (+ 5 offset))
	  (aref arr 8) (tile-paint-value +effect-file+ (+ 6 offset))
	  (aref arr 9) (tile-paint-value +effect-file+ (+ 7 offset)))
    arr))


(defun is-legal-effect-type? (effect-type)
  (find effect-type '("teleport" "cold" "light" "acid" "fire" "healing" "mana"
		      "divination" "enhance" "meteor" "darkness" "arrow"
		      "magic-missile" "poison" "electricity" "enchant" "erosion"
		      "polymorph" "summon")
	:test #'equal))


(defun define-spell-effect (id &key gfx-beam text-beam gfx-ball text-ball
			    gfx-orb text-orb gfx-bolts text-bolts)

  (unless (verify-id id)
    (error-condition 'illegal-speff-data :id id
		     :desc "Illegal id for spell-effect."))
  
  (let ((spell-effect (make-instance 'visual-projectile :id id)))

    (cond ((arrayp gfx-bolts)
	   (setf (projectile.gfx-path spell-effect) gfx-bolts))
	  ((eq nil gfx-bolts))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper gfx-bolt argument to spell-effect.")))

	   
    (cond ((arrayp text-bolts)
	   (setf (projectile.text-path spell-effect) text-bolts))
	  ((eq nil text-bolts))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper text-bolt argument to spell-effect.")))


    (cond ((non-negative-integer? gfx-ball)
	   (setf (projectile.gfx-explosion spell-effect) gfx-ball))
	  ((eq nil gfx-ball))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper gfx-ball argument to spell-effect.")))

    (cond ((non-negative-integer? text-ball)
	   (setf (projectile.text-explosion spell-effect) text-ball))
	  ((eq nil text-ball))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper text-ball argument to spell-effect.")))


    (cond ((non-negative-integer? gfx-orb)
	   (setf (projectile.gfx-impact spell-effect) gfx-orb))
	  ((eq nil gfx-orb))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper gfx-orb argument to spell-effect.")))

    (cond ((non-negative-integer? text-orb)
	   (setf (projectile.text-impact spell-effect) text-orb))
	  ((eq nil text-orb))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper text-orb argument to spell-effect.")))

    (cond ((non-negative-integer? gfx-beam)
	   (setf (projectile.gfx-beam spell-effect) gfx-beam))
	  ((eq nil gfx-beam))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper gfx-beam argument to spell-effect.")))

    (cond ((non-negative-integer? text-beam)
	   (setf (projectile.text-beam spell-effect) text-beam))
	  ((eq nil text-beam))
	  (t
	   (signal-condition 'illegal-speff-data :id id
			     :desc "Not proper text-beam argument to spell-effect.")))

    
    (setf (gethash id (variant.visual-effects *variant*)) spell-effect)
    
    spell-effect))

(defstruct speff
  type)

(defun is-spell-effect? (obj)
  (speff-p obj))
;;  (functionp obj))

(defun get-spell-effect (type)
  (make-speff :type type))
#||
  (warn "getting spell effect ~s" type)
  #'(lambda (var source target &key x y damage state-object)
      (ignore-errors
	(warn "GSE: Apply ~A on ~s by ~s" type (get-creature-name target) (get-creature-name source)))
      (apply-spell-effect! var type source target :x x :y y :damage damage :state-object state-object)))
||#

(defmethod obj-damaged-by-element? ((variant vanilla-variant) (object active-object) element)
  (when (is-legal-element? variant element)
    (vulnerable-to-element? object element)))


(defmethod obj-damaged-by-element? ((variant vanilla-variant) (object active-object) (element (eql '<holiness>)))
  (is-cursed? object))

(defmethod apply-projection-effect-to-target! ((variant vanilla-variant)
					       source (target floor-type)
					       &key
					       (x 0) (y 0) (damage 0)
					       (effect nil) (distance 0))
  ;;(warn "damage floor ~s,~s" x y)
  
  (let ((balanced-damage (int-/ (+ damage distance) (1+ distance))))
    (cond ((is-spell-effect? effect)
	   (apply-spell-effect! variant (speff-type effect) source target
				:x x :y y :damage balanced-damage)
	   ;;(funcall effect variant source target :x x :y y :damage balanced-damage)
	   t)
	  (t
	   (warn "No spell-effect ~s" effect)
	   nil))
    ))

(defmethod apply-projection-effect-to-target! ((variant vanilla-variant)
					       source (target decor)
					       &key
					       (x 0) (y 0) (damage 0)
					       (effect nil) (distance 0))
  (let ((balanced-damage (int-/ (+ damage distance) (1+ distance))))
    (cond ((is-spell-effect? effect)
	   ;;(funcall effect variant source target :x x :y y :damage balanced-damage)
	   (apply-spell-effect! variant (speff-type effect) source target
				:x x :y y :damage balanced-damage)
	   t)
	  (t
	   (warn "No speff: ~s" effect)
	   nil))))

;;; this one is a bloody nightmare
(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source target
					       &key (x 0) (y 0) (damage 0)
					       (effect nil) (distance 0))
  ;;  (declare (ignore x y damage effect distance source))
  #||
  (when (or (is-monster? target) (is-player? target))
    (warn "APET(VV): Apply damaging (~s) ~s proj ~s on ~s by ~s" damage
	  effect distance (get-creature-name target) (get-creature-name source)))
  ||#
  
  (let* ((balanced-damage (int-/ (+ damage distance) (1+ distance)))
	 
	 (seen? (or (is-player? target) (amon.seen-by-player? target)))
	 (desc (when (is-monster? target) (get-creature-desc target #x00)))
	 (meff (make-instance 'vanilla-monster-effect :seen seen?
			      :damage balanced-damage
			      :note nil :dying-note "dies")))

    (when (is-monster? target)
      (let ((type (monster.type (amon.kind target))))
	(when (or (eq type '<demon>)
		  (eq type '<undead>)
		  (eq type '<stupid>)) ;; fix
	  (setf (meff.dying-note meff) "is destroyed"))))
    
    
    (cond ((is-spell-effect? effect)
	   ;; (warn "Function-effect not implemented for project-monster")
	   (let ((retval
		  (apply-spell-effect! variant (speff-type effect)
				       source target
				       :x x :y y :damage balanced-damage :state-object meff)
		   ;;(funcall effect variant source target :x x :y y :damage balanced-damage :state-object meff)
		   ))
	     (when (typep retval 'vanilla-monster-effect)
	       (setf meff retval))))
	  (t
	   (warn "No effect-function for ~s" effect)
	   ;;	       (warn "Hit monster ~s at (~s,~s) from ~s at (~s,~s) [~s]" (monster.name the-monster) loc-x loc-y
	   ;;		     (if (typep source 'player) "player" "someone")
	   ;;		     (location-x source) (location-y source) distance)
	   ))

    ;; add skip!
    
    ;; we simplify greatly here!
    
    (setf balanced-damage (meff.damage meff))
    
    ;; uniques only killed by player
    (when (and (is-unique-monster? target)
	       (is-player? source) 
	       (< (current-hp target) balanced-damage))
      (setf balanced-damage (current-hp target)))

   
 
    (cond ((and (is-monster? target)
		(> balanced-damage (current-hp target)))
	   (setf (meff.note meff) (meff.dying-note meff)))
	  ;; skip polymorph
	  ;; skip teleport
	  ;; skip stun
	  ;; skip confusion
	  (t))
    ;; skip fear

    ;;(warn "Actual damage is ~s" balanced-damage)

    (cond ((is-monster? source)
	   (warn "APET(VV): Monster ~s attacked ~s"
		 (get-creature-name source) (get-creature-name target))
	   (when (plusp balanced-damage)
	     (deliver-damage! variant source target balanced-damage)))

	  ((is-player? source)
	   (let ((is-dead? nil))

	     (when (plusp balanced-damage)
	       (setf is-dead? (deliver-damage! variant source
					       target balanced-damage
					       :dying-note (meff.dying-note meff))))
	     (unless is-dead? ;; he died
	       ;; improve message later
	       (when (meff.note meff)
		 (format-message! "~@(~A~) ~a" desc
				  (if (meff.note meff) (meff.note meff) "was hurt.")))
	       ;; skip fear
	       ;; skip sleep
	       )))
	  (t
	   (error "Who was source?? ~s" source)))
    
    
    (cond ((is-monster? target)
	   (update-monster! variant target nil))
	  ((is-player? target)
	   (ask-for-update! target '[bonuses])
	   t)
	  (t
	   (warn "APET(VV): ODD TARGET ~s" target)))
    
    (light-spot! *dungeon* x y)
    
    
    ;; skip window
    
    ;; return if the object was obviously seen
    (meff.obvious meff)))


(defmethod apply-projection-effect-to-target! ((variant vanilla-variant)
					       source (target active-object)
					       &key (x 0) (y 0)
					       (damage 0) (effect nil)
					       (distance 0))
  (declare (ignore distance))
  ;;  (warn "VAN-OBJ: Applying effect ~s to ~s" effect target)
  (when (and effect (is-spell-effect? effect))
    (apply-spell-effect! variant (speff-type effect) source target :x x :y y
			 :damage damage)))


  
(defun %destroy-floor-obj (variant dungeon x y obj msg)
  (let ((item-table (cave-objects dungeon x y)) 
	(desc (with-output-to-string (s)
		(write-obj-description variant obj s)))
	(verb (if (plusp (aobj.number obj))
		  "are"
		  "is")))
    (format-message! "~@(~a~) ~a ~a." desc verb msg)
    (item-table-remove! item-table obj)
    (when (= 0 (items.cur-size item-table))
      (setf (cave-objects dungeon x y) nil))
    (light-spot! dungeon x y)))


(defmethod apply-spell-effect! ((variant vanilla-variant) type source
				target &key x y (damage 0)
				(state-object nil))
  (declare (ignore x y type damage source target))
  ;; do nothing default
  ;;(ignore-errors
  ;;  (warn "ASE(VV): No apply [~a ~s] from ~a" type (get-creature-name target) (get-creature-name source)))
  state-object)


(defmethod apply-spell-effect! ((variant vanilla-variant) type source
				(target player)
				&key x y (damage 0) (state-object nil))

  (declare (ignore source x y))
  ;; iterate over equipment

  (when-bind (inv (get-creature-inventory target))
    (when-bind (container (aobj.contains inv))
      (let ((cur-size (items.cur-size container))
	    (objs (items.objs container))
	    (any-removed nil))
	;;(warn "We have ~s objs" cur-size)
	(loop for i from 0
	      for obj across objs
	      do
	      (when obj
		;;(format t "~&~s obj ~s is ~s damaged by ~s~%"
		;;(aobj.number obj) (object.name obj) (damaged-by-element? variant obj type) type)
		(when (obj-damaged-by-element? variant obj type) ;; it can die :-)
		  (let ((chance (cond ((< damage 30) 1)
				      ((< damage 60) 2)
				      (t 3)))
			(count 0)
			(num-objs (aobj.number obj))
			(oname (with-output-to-string (s)
				 (write-obj-description variant obj s :numeric-prefix nil))))
		    (dotimes (i num-objs)
		      (when (< (random 100) chance)
			(incf count)))

		    (cond ((= count num-objs)
			   ;;(warn "Remove ~a" (object.name obj))
			   (setf (aref objs i) nil)
			   (decf cur-size)
			   (setf any-removed t)
			   (if (= count 1)
			       (format-message! "Your ~a was destroyed!" oname)
			       (format-message! "All of your ~a were destroyed!" oname))
			   )
			   
			  ((> count 0)
			   (decf (aobj.number obj) count)
			   (if (= count 1)
			       (format-message! "One of your ~a was destroyed!" oname)
			       (format-message! "Some of your ~a were destroyed!" oname))
			   )
			  (t ;; they resisted!
			   ))
		    
		    ))
		))

	
	(when any-removed
	  (setf (items.cur-size container) (shrink-array! objs))
	  ;;(warn "Shrink claims there are now ~s objs"  (items.cur-size container))
	  (assert (= cur-size (items.cur-size container)))
	  (ask-for-redraw! target '[equipment])
	  )

	)))
  
  state-object)


(defmethod apply-spell-effect! ((variant vanilla-variant) type source
				(target active-object)
				&key x y (damage 0) (state-object nil))
  (declare (ignore damage source))
  
  (when (obj-damaged-by-element? variant target type)
    (%destroy-floor-obj variant *dungeon* x y target "destroyed"))
  state-object)

;; example!
(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<fire>)) source
				(target active-object)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (when (obj-damaged-by-element? variant target '<fire>)
    (%destroy-floor-obj variant *dungeon* x y target "burns"))
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<weak-light>)) source
				(target floor-type)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (when (and (non-negative-integer? x)
	     (non-negative-integer? y))
    (let ((dungeon *dungeon*))
      (bit-flag-add! (cave-flags dungeon x y) +cave-glow+)
      (when (player-has-los-bold? dungeon x y)
	(ask-for-update! *player* '[forget-view])
	(ask-for-update! *player* '[update-view])
	)))
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<light>)) source
				(target floor-type)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (when (and (non-negative-integer? x)
	     (non-negative-integer? y))
    (let ((dungeon *dungeon*))
      (bit-flag-add! (cave-flags dungeon x y) +cave-glow+)
      (when (player-has-los-bold? dungeon x y)
	(ask-for-update! *player* '[forget-view])
	(ask-for-update! *player* '[update-view])
	)))
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<weak-darkness>)) source
				(target floor-type)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (when (and (non-negative-integer? x)
	     (non-negative-integer? y))
    (let ((dungeon *dungeon*))
      (bit-flag-remove! (cave-flags dungeon x y) +cave-glow+)
      ;; remove marked?
      (when (player-has-los-bold? dungeon x y)
	(ask-for-update! *player* '[forget-view])
	(ask-for-update! *player* '[update-view])
	)))
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<darkness>)) source
				(target floor-type)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (when (and (non-negative-integer? x)
	     (non-negative-integer? y))
    (let ((dungeon *dungeon*))
      (bit-flag-remove! (cave-flags dungeon x y) +cave-glow+)
      ;; remove marked?
      (when (player-has-los-bold? dungeon x y)
	(ask-for-update! *player* '[forget-view])
	(ask-for-update! *player* '[update-view])
	)))
  
  state-object)

#||
(defmethod apply-spell-effect! ((variant vanilla-variant) (type (eql '<magic-missile>)) source target
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source damage))
  
  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  state-object)
||#
    

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<fire>)) source
				(target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (let ((elm-flag (get-element-flag variant type))
	(mon-know (get-monster-knowledge *player* target)))

    (when (meff.seen state-object)
      (setf (meff.obvious state-object) t)
      (bit-flag-add! (monster.tried-elm mon-know) elm-flag))

    (cond ((immune-to-element? target type)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-immunities mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))
	  
	  ((resists-element? target type)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-resists mon-know) elm-flag))
	   ;; we're resisting
	   (setf (meff.note state-object) "resists a lot.")
	   (setf (meff.damage state-object) (int-/ damage 9))
	   ;; skip lore
	   )
	  
	  ((vulnerable-to-element? target type)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag))
			  
	   (setf (meff.note state-object) "is hurt badly."
		 (meff.dying-note state-object) "shrivels away to ashes!")
	   (setf (meff.damage state-object) (* 2  damage)))
	  (t
	   0))
	
    
    state-object))

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<weak-light>)) source
				(target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (let* ((test-elm '<light>)
	 (elm-flag (get-element-flag variant test-elm))
	 (mon-know (get-monster-knowledge *player* target)))
  
    (when (meff.seen state-object)
      (setf (meff.obvious state-object) t))


    (cond ((vulnerable-to-element? target test-elm)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag))
			  
	   (setf (meff.note state-object) "cringes from the light."
		 (meff.dying-note state-object) "shrivels away in the light!")
	   (setf (meff.damage state-object) damage))

	  ((immune-to-element? target test-elm)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-immunities mon-know) elm-flag))
	     
	   (setf (meff.damage state-object) 0))
	
	  ((resists-element? target test-elm)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-resists mon-know) elm-flag))

	   (setf (meff.damage state-object) 0))
	  
	  (t
	   (setf (meff.damage state-object) 0)))
    
    state-object))

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<weak-darkness>)) source
				(target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (let* ((test-elm '<darkness>)
	 (elm-flag (get-element-flag variant '<darkness>))
	 (mon-know (get-monster-knowledge *player* target)))
  
    (when (meff.seen state-object)
      (setf (meff.obvious state-object) t)
      (bit-flag-add! (monster.tried-elm mon-know) elm-flag))
	  

    (cond ((vulnerable-to-element? target test-elm)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag))
			  
	   (setf (meff.note state-object) "cringes from the darkness."
		 (meff.dying-note state-object) "shrivels away in the light!")
	   (setf (meff.damage state-object) damage))
	
	  ((immune-to-element? target test-elm)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-immunities mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))
	
	  ((resists-element? target test-elm)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-resists mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))
	
	  (t
	   (setf (meff.damage state-object) 0)))
    
    state-object))

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<erosion>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (let ((elm-flag (get-element-flag variant '<erosion>))
	(mon-know (get-monster-knowledge *player* target)))
  
    (when (meff.seen state-object)
      (setf (meff.obvious state-object) t)
      (bit-flag-add! (monster.tried-elm mon-know) elm-flag))
	  

    (cond ((vulnerable-to-element? target '<erosion>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag))
			  
	   (setf (meff.note state-object) "is partly eroded!"
		 (meff.dying-note state-object) "dissolves!")
	   (setf (meff.damage state-object) damage))
	
	  ((immune-to-element? target '<erosion>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-immunities mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))
	
	  ((resists-element? target '<erosion>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-resists mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))
	
	  (t
	   (setf (meff.damage state-object) 0)))
    
    state-object))


(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<light>)) source
				(target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (let ((elm-flag (get-element-flag variant '<light>))
	(mon-know (get-monster-knowledge *player* target)))
  
    (when (meff.seen state-object)
      (bit-flag-add! (monster.tried-elm mon-know) elm-flag)

      (setf (meff.obvious state-object) t))

    (cond ((immune-to-element? target '<light>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-immunities mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))

	  ((resists-element? target '<light>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-resists mon-know) elm-flag))
	   (setf (meff.note state-object) "resists.")
	   (setf (meff.damage state-object) (int-/ (* 2 damage) (+ (randint 6) 6))))
	
	  ((vulnerable-to-element? target '<light>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag))
	   (setf (meff.note state-object) "cringes from the light."
		 (meff.dying-note state-object) "shrivels away in the light!")
	   (setf (meff.damage state-object) (* 2 damage)))

	  ;; no extra stuff here
	  (t
	   ;;(setf (meff.damage state-object) 0)
	   ))
    
    state-object))

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<darkness>)) source
				(target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (let ((elm-flag (get-element-flag variant '<darkness>))
	(mon-know (get-monster-knowledge *player* target)))
      
    (when (meff.seen state-object)
      (bit-flag-add! (monster.tried-elm mon-know) elm-flag)

      (setf (meff.obvious state-object) t))

    (cond ((immune-to-element? target '<darkness>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-immunities mon-know) elm-flag))
	   (setf (meff.damage state-object) 0))

	  ((resists-element? target '<darkness>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-resists mon-know) elm-flag))
	   (setf (meff.note state-object) "resists.")
	   (setf (meff.damage state-object) (int-/ (* 2 damage) (+ (randint 6) 6))))

	  ((vulnerable-to-element? target '<darkness>)
	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag))
	   (setf (meff.note state-object) "cringes from the darkness."
		 (meff.dying-note state-object) "shrivels away in the darkness!")
	   (setf (meff.damage state-object) (* 2 damage)))

	  ;; no extra stuff here
	  (t
	   ;;(setf (meff.damage state-object) 0)
	   ))
    
    state-object))

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<erosion>)) source
				(target floor-type)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (let ((flags (floor.flags target))
	(dungeon *dungeon*)
	(update nil)
	(word "wall"))
    ;;(warn "Erode ~s" target)

    (unless (bit-flag-set? flags +floor-flag-permanent+)
      
    ;; first consider normal walls
      (cond ((is-door? (cave-decor dungeon x y)) ;; destroy doors too
	     (setf update t
		   word "door"))
	    
	    ((bit-flag-set? flags +floor-flag-wall+)
	     (setf update t
		   word "wall"))
	    
	    (t nil))
	    
      (when update
	
	(when (player-can-see-bold? dungeon x y)
	  (format-message! "The ~a turns into mud!" word)
	  (ask-for-update! *player* '[forget-view])
	  (ask-for-update! *player* '[update-view])
	  (ask-for-update! *player* '[forget-flow])
	  (ask-for-update! *player* '[update-flow])
	  (ask-for-update! *player* '[monsters])
	  ;;(setf (meff.obvious state-object) t)
	  )

	(floorify-coord! dungeon x y)))
    
    state-object))
  

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<polymorph>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore source))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  (setf (meff.note state-object) "is unaffected!")
  
  (unless (is-unique-monster? target)
    (warn "possible polymorph fuckall ~s ~s vs ~s" target damage (get-power-lvl target))
    ;; only do those we're powerful enough to do
    (let ((target-power (get-power-lvl target))
	  (dungeon *dungeon*)
	  (new-one nil))
      (when (< target-power (+ damage (randint 10))) ;; differs from vanilla but is simpler
	;; ok, we're powerful enough to polymorph the monster, but let him get a saving throw
	(when (> (randint 90) target-power)
	  ;; pick a new random monster, close in power to the one we had
	  (setf new-one (polymorph-creature variant *level* target :boost 5))
	  	  
	  ;;(warn "We will turn a ~s into a ~s" target new-one)

	  (when (and new-one
		     (not (eq new-one (amon.kind target))))
	    (setf new-one (produce-active-monster variant new-one)))

	  (when (typep new-one 'active-monster)
	    
	    ;; delete old monster, place new monster in old spot
	    (setf (meff.note state-object) "changes!")
	    (setf (creature-alive? target) nil)
	    (remove-monster-from-dungeon! dungeon target)
	    (place-single-monster! dungeon *player* new-one x y nil)
	    (update-monster! variant new-one t) ;; hack..
	    ;;(when (player-can-see-bold? dungeon x y)
	    ;;  (setf (amon.seen-by-player? new-one) t)) ;; let's fake it and see him right away
	    (light-spot! dungeon x y))
	  
	  ))
      
      ))

  (setf (meff.damage state-object) 0) ;; no real damage
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<heal-monster>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore source x y))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  (setf (meff.damage state-object) 0) ;; no damage
    ;; wake up
  (when (plusp damage)
    (heal-creature! target damage))
  
  (setf (meff.note state-object) "looks healthier.")
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<haste>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore source x y damage))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  (setf (meff.damage state-object) 0) ;; no damage
  ;; wake up
  (haste-creature! target 10 (+ 20 (randint 20))) 

  (setf (meff.note state-object) "starts moving faster.")
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<probe>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore source x y damage))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  (setf (meff.damage state-object) 0) ;; no damage

  ;; learn shit about the monster
  
  state-object)


(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<slow>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  
  (declare (ignore source x y))

  (setf (meff.damage state-object) 0) ;; no damage
  ;; wake up
  ;; only when non-unique and failing saving-throw
  (cond ((and (not (is-unique-monster? target))
	      (< (get-power-lvl target) (+ damage (randint 10))))

	 (when (meff.seen state-object)
	   (setf (meff.obvious state-object) t))
	 
	 (slow-creature! target 10 (+ 20 (randint 20))) 
	 (setf (meff.note state-object) "starts moving slower."))
	
	(t
	 (setf (meff.note state-object) "is unaffected.")))
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<sleep>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  
  (declare (ignore source x y))
  
  (setf (meff.damage state-object) 0) ;; no damage

  (let ((elm-flag (get-element-flag variant type))
	(mon-know (get-monster-knowledge *player* target)))
      
    ;; wake up
    ;; only when non-unique and failing saving-throw
    (cond ((or (is-unique-monster? target)
	       (immune-to-element? target type)
	       (resists-element? target type)
	       (> (get-power-lvl target) (+ damage (randint 10))))

	   (when (or (immune-to-element? target type)
		     (resists-element? target type))
	     (when (meff.seen state-object)
	       (bit-flag-add! (get-immunities mon-know) elm-flag)))
	 

	   (setf (meff.note state-object) "is unaffected!"
		 (meff.obvious state-object) nil))

	  (t

	   (when (meff.seen state-object)
	     (bit-flag-add! (get-vulnerabilities mon-know) elm-flag)
	     (setf (meff.obvious state-object) t))
	 
	   (setf (meff.note state-object) "falls asleep.")
	   (modify-creature-state! target '<sleeping>
				   :new-value  500)))
    
    state-object))


(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<destroy-traps/doors>))
				source (target decor)
				&key
				x y (damage 0)  (state-object nil))
  
  (declare (ignore source damage))

  (let* ((dungeon *dungeon*)
	 (floor (cave-floor dungeon x y))
	 (flags (floor.flags floor))
	 (update nil))
    ;;(warn "Erode ~s" target)

    (unless (bit-flag-set? flags +floor-flag-permanent+)

      (when (or (is-door? target)
		(is-trap? target))
	(setf update t)))
    
    (when update
      
      (when (player-can-see-bold? dungeon x y)
	(print-message! "There is a brief flash of light!")
	(ask-for-update! *player* '[forget-view])
	(ask-for-update! *player* '[update-view])
	(ask-for-update! *player* '[monsters])

	;; fix later
	;;(setf (meff.obvious state-object) t)
	)

      (floorify-coord! dungeon x y))
    
  
  state-object))

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<destroy-traps>))
				source (target decor)
				&key
				x y (damage 0)  (state-object nil))
  
  (declare (ignore source damage))

  (let* ((dungeon *dungeon*))
    (unless (not (bit-flag-set? (cave-flags dungeon x y)
				+floor-flag-permanent+))

      (cond ((and (is-door? target)
		  (not (decor.visible? target)))
	     ;; unhide and unlock
	     (decor-operation variant target :visible :value t)
	     (setf (door.lock target) 0)
	     (ask-for-update! *player* '[forget-view])
	     (ask-for-update! *player* '[update-view])
	     )

	    ;; unlock locked doors
	    ((and (is-door? target)
		  (plusp (door.lock target)))
	     (when (player-can-see-bold? dungeon x y)
	       ;; add obvious
	       (print-message! "There is a click."))
	     (setf (door.lock target) 0))


	    ((is-trap? target)
	     (when (player-can-see-bold? dungeon x y)
	       (print-message! "There is a brief flash of light!")
	       ;;(setf (meff.obvious state-object) t)
	       )
	     ;; does a bit more, but ok for vanilla
	     (floorify-coord! dungeon x y)
	     (ask-for-update! *player* '[forget-view])
	     (ask-for-update! *player* '[update-view])
	     )

	    (t nil)))

  
  state-object))


(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<teleport>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore source x y))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  (setf (meff.damage state-object) 0) ;; no damage
  ;; wake up
  (teleport-creature! *dungeon* *player* target damage)

  (setf (meff.note state-object) "disappears!")
  
  state-object)

(defmethod apply-spell-effect! ((variant vanilla-variant)
				(type (eql '<dispel-evil>))
				source (target active-monster)
				&key
				x y (damage 0)  (state-object nil))
  (declare (ignore source x y))


  (cond ((is-evil? target)
	 (when (meff.seen state-object)
	   (add-monster-knowledge-flag! *player* target '<evil>)
	   (setf (meff.obvious state-object) t))
	 (setf (meff.note state-object) "shudders."
	       (meff.dying-note state-object) "dissolves."
	       (meff.damage state-object) damage))
	(t
	 (setf (meff.damage state-object) 0) ;; no damage
	 ))
  
  state-object)
