;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#||

DESC: variants/contraband/spells.lisp - spell-effects
Copyright (c) 2003 - Eugene Zaikonnikov, Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#
(in-package :org.langband.contraband)

(defclass spellbook ()
  ((name   :accessor spellbook.name
	   :initform ""
	   :initarg :name
	   :documentation "The name of the spellbook, used for listings.")
   (id     :accessor spellbook.id
	   :initform nil
	   :initarg :id
	   :documentation "The id for the spellbook, used for lookups, e.g from object-kinds.")
   (size   :accessor spellbook.size
	   :initform 6
	   :initarg :size
	   :documentation "The size of the spellbook, ie how many spells it _can_ take.") 
   (spells :accessor spellbook.spells
	   :initform nil
	   :initarg :spells
	   :documentation "An array of spell-objects."))
  (:documentation "Represents the spell-data of a spellbook, not the actual book."))


(defclass spell ()
  ((name   :accessor spell.name
	   :initform ""
	   :initarg :name
	   :documentation "The name of the spell, as it will be shown in spellbooks.")
   (id     :accessor spell.id 
	   :initform nil
	   :initarg :id
	   :documentation "The id for the spell, used for lookups.")
   (text   :accessor spell.clauses
	   :initform nil
	   :initarg :clauses
	   :type list
	   :documentation "The clauses of the spell, in order of activation.")
   )
  (:documentation "Represents a spell composed from one or several clauses."))


(defclass clause ()
  ((G      :accessor clause.geometry
	   :initarg :geometry
	   :documentation "The propagation dynamics of the clause.")
   (E      :accessor clause.element
	   :initarg :element
	   :documentation "The element clause yields.")
   (ME     :accessor clause.mass-energy
	   :initform 1
	   :initarg :mass-energy
	   :documentation "Mass-energy involved in the clause.")
   (V      :accessor clause.velocity
	   :initform 0
	   :initarg :velocity
	   :documentation "Velocity of clause propagation.")
;   (D      :accessor clause.duration
;           :initform 1
;           :initarg :duration
;           :documentation "How many turns the clause will be effective when cast.")
   )
  (:documentation "Represents a single clause in a spell."))

(defclass spell-effect (visual-projectile)
  ((gfx-beam :initform 0 :accessor spell-effect.gfx-beam :documentation "gfx code for beam.")
   (text-beam :initform 0 :accessor spell-effect.text-beam :documentation "text code for beam.")
   ))

(defgeneric apply-spell-effect! (variant type source target &key x y damage state-object)
  (:documentation "Applies a spell-effect of type TYPE from SOURCE on TARGET at coords (X,Y) for
DAMAGE damage.  The state-object may be used to pass info back to calling function.  The methods
should return NIL or the state-object (with possibly updated state."))

(defun write-down-spell! (book)
  (with-dialogue ()
    (let ((spell (get-string-input "Input your spell: " :max-length 60))
	  (name (get-string-input "Spell name: ")))
      (push (make-instance 'spell
			   :name name
			   :clauses (parse-spell spell))
	    (spellbook.spells book)))))
  

(defun parse-spell (string)
  "Produces spell object with clauses based on valid incantation in input language,
signals error otherwise."
  ;;our current input language is sexprs
  (let ((list (read-from-string string)))
    (loop for clausen in list
	  collecting (make-instance 'clause
				    :geometry (first clausen)
				    :element (second clausen)
				    :mass-energy (third clausen)
				    :velocity (fourth clausen)))))


(defun element-cost (element)
  ;;adjust as necessary
  (case element
    (<frost> 1)
    (<fire> 1)
    (<lightning> 1)
    (<poison> 1)
    (<acid> 1)
    (<electricity> 1)
    (otherwise (error "Unknown element type ~A" element))))

(defun geometry-cost (geometry)
  ;;adjust as necessary
  (case geometry
    (linear 1)
    (linear-continuous 1)
    (radial 1)
    (dot 1)
    (otherwise (error "Unknown geometry type ~A" geometry))))

(defmethod activate-spell ((spell spell) (player player))
  (let ((cost (loop for clause in (spell.clauses spell) ;;calculate spell cost
		    summing (* (+ (clause.velocity clause)
				  (* (abs (clause.mass-energy clause)) ;can be negative
				     (element-cost (clause.element clause))))
			       (geometry-cost (clause.geometry clause))
			       ))))
    (print (current-mana player))
    ;;;TODO: enable mana check
    (when t ;(> (current-mana player) cost)
      (loop for clause in (spell.clauses spell) do
	    (activate-clause clause player))
      (decf (current-mana player) cost))))

(defmethod activate-clause ((clause clause) (player player))
  (format t "Clause element: ~A, geometry: ~A, mass-energy: ~A, velocity: ~A~%"
	  (clause.element clause) (clause.geometry clause) (clause.mass-energy clause)
	  (clause.velocity clause))
  (when (and (is-legal-effect-type? (clause.element clause))
	     (is-legal-geometry-type? (clause.geometry clause)))
    (if (plusp (clause.mass-energy clause)) ;the spell is offensive
	(when-bind (dir (get-aim-direction))
		   (funcall (case (clause.geometry clause)
			      (linear 'contra-fire-bolt!)
			      (linear-continuous 'contra-fire-beam!)
			      (otherwise (error "Wrong geometry: ~A" (clause.geometry clause))))
			    player
			    dir
			    (get-spell-effect (clause.element clause))
			    (clause.velocity clause) ;;TODO: figure out something better
			    );;:projected-object spell)
		   )
      ;;here we apply protective effect (should always have 'dot' geometry)
      t) 
      
    ))

(defun is-legal-geometry-type? (geometry-type)
  (find geometry-type '(linear linear-continous radial dot)
	:test #'equal))

(defun is-legal-effect-type? (effect-type)
  (find effect-type '(<frost> <fire> <lightning> <poison> <acid> <electricity>)
	:test #'equalp))

;; hack
(defmacro spell-effect (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defun define-spell-effect (id &key gfx-beam text-beam gfx-ball text-ball
			    gfx-orb text-orb gfx-bolts text-bolts)
  (declare (ignore gfx-beam text-beam))
  (assert (verify-id id))
  
  (let ((spell-effect (make-instance 'spell-effect :id id)))

    (when (arrayp gfx-bolts)
      (setf (projectile.gfx-path spell-effect) gfx-bolts))
    (when (arrayp text-bolts)
      (setf (projectile.text-path spell-effect) text-bolts))

    (when (numberp gfx-ball)
      (setf (projectile.gfx-explosion spell-effect) gfx-ball))
    (when (numberp text-ball)
      (setf (projectile.text-explosion spell-effect) text-ball))

    (when (numberp gfx-orb)
      (setf (projectile.gfx-impact spell-effect) gfx-orb))
    (when (numberp text-orb)
      (setf (projectile.text-impact spell-effect) text-orb))

    
    (setf (gethash id (variant.visual-effects *variant*)) spell-effect)
    
    spell-effect))

    
(defun get-spell-effect (type)
  #'(lambda (var source target &key x y damage state-object)
      (apply-spell-effect! var type source target :x x :y y :damage damage :state-object state-object)))

(defmethod print-mana-points ((variant contraband) player setting)
  (when (is-spellcaster? player)
    (call-next-method)))

(defmethod calculate-creature-mana! ((variant contraband) (player player))

  (setf (maximum-mana player) 100) ;FIXME: for testing
  #+(or)(let* ((pl-class (player.class player))
	       (pl-lvl (player.power-lvl player))
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

    (setf (maximum-mana player) new-mana)
    
    ;; skip gloves
    ;; skip weight
    
      
    t))


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


(defun %simple-projection (source destination flag effect damage &key projected-object)
  "Destination can be a target or a direction.  A direction of 5 is interpreted
as target."
  
  (multiple-value-bind (dest-x dest-y)
      (get-destination-coords source destination)
    (do-projection source dest-x dest-y flag :effect effect :damage damage :projected-object projected-object)))


(defun contra-fire-beam! (player destination effect damage &key projected-object)
  "Fires a beam in a direction."
  (let ((flag (logior +project-kill+ +project-beam+ +project-through+)))
    (%simple-projection player destination flag effect damage :projected-object projected-object)))

(defun contra-fire-bolt! (player destination effect damage &key projected-object)
  "Fires a bolt in a direction."
  (let ((flag (logior +project-kill+ +project-stop+ +project-through+)))
    (%simple-projection player destination flag effect damage :projected-object projected-object)))


(defun contra-fire-bolt-or-beam! (player beam-chance destination effect damage &key projected-object)
  "Will fire a beam if beam-chance/100 happens, otherwise a bolt."
  (cond ((< (random 100) beam-chance)
	 (van-fire-beam! player destination effect damage :projected-object projected-object))
	(t
	 (van-fire-bolt! player destination effect damage :projected-object projected-object))))

(defun contra-fire-ball! (source destination effect damage radius &key projected-object)
  "Fires a ball in a direction."
  (let ((flag (logior +project-kill+ +project-grid+ +project-stop+ +project-item+)))

    (multiple-value-bind (dest-x dest-y)
	(get-destination-coords source destination 99)
      (do-projection source dest-x dest-y flag
		     :effect effect :damage damage
		     :radius radius
		     :projected-object projected-object)
      )))
  

(defun contra-learn-spell! (dungeon player &key (variant *variant*))
  "Interactive selection of spell to learn."
  (when-bind (book (interactive-book-selection dungeon player))
	     (write-down-spell! book))

  )


(defun contra-invoke-spell! (dungeon player &key (variant *variant*))
  "Invokes a spell.. gee."
  (let ((book (make-instance 'spellbook
			     :name "test")))
    (write-down-spell! book)
    (activate-spell (car (spellbook.spells book)) player))
  ;;for now we just create spell and cast it right away

    
  (values))

(defun browse-spells (dungeon player &key (variant *variant*))
  "Interactive selection of spell to learn."

  (with-dialogue ()
    (when-bind (book (interactive-book-selection dungeon player))
      (let* ((okind (aobj.kind book))
	     (book-id (get-id okind)))
	(when-bind (spell-info (gethash book-id (variant.spellbooks variant)))
;;	  (warn "SI: ~s" spell-info)
	  (display-spells player spell-info)
	  (pause-last-line!)
	  )))))


(defun interactive-spell-selection (player spellbook &key (prompt "Cast which spell? "))
  "Returns selection."
  (block select-spell
    (loop
     (let ((select-string (format nil "(Spells ~a-~a, *=List, ESC) ~a"
				  #\a (i2a (1- (spellbook.size spellbook))) prompt)))

       (put-coloured-line! +term-white+ select-string 0 0)
       (let ((selection (read-one-character)))
	 (cond ((eql selection +escape+)
		(return-from select-spell nil))
	       ((eql selection #\*)
		(display-spells player spellbook)
		;; (warn "Display spellbook-spells not implemented.")
		nil)
	       ((alpha-char-p selection)
		(let ((num (a2i selection)))
		  (when (< num (spellbook.size spellbook))
		    (return-from select-spell num))))
	       (t
		(warn "Fell through spell-selection with choice: ~s" selection)))
	 )))
    ))


(defun display-spells (player spellbook &key (x 20) (y 1))
  "Displays the spell somehow."
  ;; l-blue for unknown
  ;; l-green for untried
  ;; white for normal/tried
  ;; red for difficult
  ;; l-dark for unreadable
  (let ((colour +term-white+)
	(comment ""))

    (put-coloured-line! +term-white+ "" x y)
    (put-coloured-line! +term-white+ "" x (1+ y))
    
    (put-coloured-str! +term-l-green+ (format nil "Book: ~a" (spellbook.name spellbook)) (+ x 8) y)
    (incf y)
    (put-coloured-str! +term-white+ "Name" (+ x 5) y)
    (put-coloured-str! +term-white+ "Lv Mana Fail Info" (+ x 35) y)
    
    (loop for i from 0
	  for spell across (spellbook.spells spellbook)
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
				  comment "difficult"))
			   
			   ((< (current-mana player) base-mana)
			    (setf colour +term-violet+)
			    (setf comment "low mana"))

			   ((not learnt-it)
			    (setf colour +term-l-blue+
				  comment "unknown"))
			   
			   ((not spell-tried)
			    (setf colour +term-l-green+
				  comment "untried"))
			   (t))
		   
		     (let ((str (format nil "  ~a) ~30a~2d ~4d ~3d% ~a"
					(i2a i) (spell.name spell) base-level base-mana base-fail comment
					)))
		       (put-coloured-line! colour str  x row))))
		  )))
    
    (put-coloured-line! +term-white+ "" x (+ (spellbook.size spellbook) y  1))
    
    nil
    ))
     

  
(defun %destroy-floor-obj (variant dungeon x y obj msg)
  (let ((item-table (cave-objects dungeon x y)) 
	(desc (with-output-to-string (s)
		(write-obj-description variant obj s))))
	   (format-message! "~a ~a." desc msg)
	   (item-table-remove! item-table obj)
	   (when (= 0 (items.cur-size item-table))
	     (setf (cave-objects dungeon x y) nil))
	   (light-spot! dungeon x y)))


(defmethod apply-spell-effect! ((variant contraband) type source target &key x y (damage 0) (state-object nil))
  (declare (ignore x y type damage source target))
  ;; do nothing default
;;  (warn "Fell through for ~s ~s" type target)
  state-object)


;;(defmethod apply-fire-effect! ((variant contraband) source target &key x y (damage 0) (state-object nil))
;;  (declare (ignore x y damage source target state-object))
;;  )


(defmethod apply-spell-effect! ((variant contraband) (type (eql '<fire>)) source (target active-object)
				&key x y (damage 0) (state-object nil))
  (declare (ignore source damage))
  (cond ((damaged-by-element? variant target '<fire>)
	 (%destroy-floor-obj variant *dungeon* x y target "burns"))
	(t
	 nil))
  state-object)

(defmethod apply-spell-effect! ((variant contraband) (type (eql '<magic-missile>)) source (target active-monster)
			       &key
			       x y (damage 0)  (state-object nil))
  (declare (ignore x y source damage))
  
  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))

  state-object)

    
  
(defmethod apply-spell-effect! ((variant contraband) (type (eql '<fire>)) source (target active-monster)
			       &key
			       x y (damage 0)  (state-object nil))
  (declare (ignore x y source))

  (when (meff.seen state-object)
    (setf (meff.obvious state-object) t))
  
  (unless (damaged-by-element? variant target '<fire>)
    ;; we're resisting
    (setf (meff.note state-object) " resists a lot.")
    (setf (meff.damage state-object) (int-/ damage 9))
    ;; skip lore
    )
    
  state-object)

