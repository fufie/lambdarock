;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/objects.lisp - code related to vanilla object-types
Copyright (c) 2002-2004 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)

(defmethod is-eatable? ((player player) (obj active-object))
  nil)

(defmethod is-eatable? ((creature active-monster) (obj active-object))
  nil)

(defmethod is-eatable? ((player player) (obj active-object/food))
  (is-eatable? player (aobj.kind obj)))

(defmethod is-eatable? ((player player) (obj object-kind/food))
  t)

(defmethod is-eatable? ((player player) (obj active-object/potion))
  (is-eatable? player (aobj.kind obj)))

(defmethod is-eatable? ((player player) (obj object-kind/potion))
  (plusp (object.food-value obj)))


(defun %dummy-eat-fun (dun pl item)
  "A hack to ensure all food-objects can be eaten."
  (declare (ignore dun pl item))
  ;;(warn "eating ~s" item)
  :used)

(defmethod apply-usual-effects-on-used-object! ((variant vanilla-variant) (player player) (obj active-object/potion))

  (warn "APPLY-USUAL ~s ~s" obj (is-eatable? player obj))
  (modify-satiation! player (object.food-value (aobj.kind obj)))
  t)

(defmethod apply-usual-effects-on-used-object! ((variant vanilla-variant) (player player) (obj active-object/food))

  (warn "APPLY-USUAL (food) ~s ~s" obj (is-eatable? player obj))
  (modify-satiation! player (object.food-value (aobj.kind obj)))
  t)


(defmethod get-price ((object active-object) situation)
  (declare (ignore situation))
  (let* ((kind (aobj.kind object))
	 (known-p (is-object-known? object)))

    ;; skip broken/cursed

    ;; also ignore discounts
    
    (if known-p
	(object.cost kind)
	(typecase object
	  (active-object/food 5)
	  (active-object/potion 20)
	  (active-object/scroll 20)
	  (active-object/staff 70)
	  (active-object/wand 50)
	  (active-object/rod 90)
	  (active-object/ring 45)
	  (active-object/amulet 45)
	  (otherwise 0)))))


;; store-related method
(defmethod store-mass-produce! ((variant variant) (store store) (object active-object))
  ;; hack

  (let ((number 1)
	(cost (get-price object store)))

    (block increase-number
      (cond ((or (typep object 'active-object/light-source)
		 (typep object 'active-object/food))
	     (when (<= cost 5) (incf number (roll-dice 3 5)))
	     (when (<= cost 20) (incf number (roll-dice 3 5))))
	     
	    ((or (typep object 'active-object/potion)
		 (typep object 'active-object/scroll))
	     (when (<= cost 60) (incf number (roll-dice 3 5)))
	     (when (<= cost 240) (incf number (roll-dice 3 5))))
	    ;; skip food, flask, light
	    ;; skip spellbooks
	    ((or (typep object 'active-object/armour)
		 (typep object 'active-object/weapon))
	     ;; test for artifact
	     (when (<= cost 10) (incf number (roll-dice 3 5)))
	     (when (<= cost 100) (incf number (roll-dice 3 5))))
	    ;; add spike
	    ((typep object 'active-object/ammo)
	     (when (<= cost 5) (incf number (roll-dice 5 5)))
	     (when (<= cost 50) (incf number (roll-dice 5 5))))
	    (t
	     nil)))

    ;; add discount..
     
      
    (setf (aobj.number object) number)))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/ring) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))

  (let* ((o-type (aobj.kind obj))
	 (known-type (or store (object.aware o-type)))
	 (obj-known (or store (is-object-known? obj)))
	 (flav-obj (if store nil (object.flavour o-type)))
	 )
    
    (write-pluralised-string stream "& #ring~@" (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :flavour flav-obj :numeric-prefix numeric-prefix)
    
    (when obj-known
      
      ;; if it has combat-bonuses, add those
      (let ((tohit-mod (get-tohit-modifier obj))
	    (dmg-mod (get-damage-modifier obj))
	    )
	(cond ((and (/= 0 tohit-mod) (/= 0 dmg-mod))
	       (format stream " (~@d,~@d)" tohit-mod dmg-mod))
	      ((/= tohit-mod 0)
	       (format stream " (~@d)" tohit-mod))
	      ((/= dmg-mod 0)
	       (format stream " (~@d)" dmg-mod))
	      (t
	       nil)))
      
      ;; display armour bonuses
      (let ((ac-bonus (get-armour-modifier obj)))
	(when (/= 0 ac-bonus)
	  (format stream " [~@d]" ac-bonus)))
      )

    (when-bind (inscr (aobj.inscr obj))
      (when (plusp (length inscr))
	(format stream " {~A}" inscr)))
    
    ))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/light-source) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))

  (let* ((o-type (aobj.kind obj))
	 (known-type (or store (object.aware o-type)))
	 ;;(obj-known (or store (is-object-known? obj)))
	 )
    
    (write-pluralised-string stream (object.name obj) (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :numeric-prefix numeric-prefix)

    (format stream " (~a charges)" (aobj.charges obj))
    #||
    (when-bind (desc (get-charge-status obj))
      (when (stringp desc)
	(format stream " (~a)" desc)))
    ||#
    
    ))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/wand) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))

  (let* ((o-type (aobj.kind obj))
	 (known-type (or store (object.aware o-type)))
	 )
    (write-pluralised-string stream "& #wand~@" (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :numeric-prefix numeric-prefix)

    (format stream " (~a charges)" (aobj.charges obj))
    
    ))


(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/armour) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  
  (declare (ignore verbosity))
  
  (let* ((o-type (aobj.kind obj))
	 (obj-known (or store (is-object-known? obj)))
	 (known-type (or store (object.aware o-type))))
    
    (write-pluralised-string stream (object.name obj) (aobj.number obj)
			     :ident known-type :actual-name (object.name obj)
			     :numeric-prefix numeric-prefix)
    
    (let ((ac-val (get-armour-rating obj))
	  (ac-bonus (get-armour-modifier obj))
	  (tohit-mod (get-tohit-modifier obj))
	  )

      (when-bind (ego (aobj.ego obj))
	(format stream " ~a" (ego.name ego)))
      
      ;; display armour bonuses
      (when (and obj-known (/= tohit-mod 0))
	(format stream " (~@d)" tohit-mod))
      
      (cond (obj-known
	     (format stream " [~d,~@d]" ac-val ac-bonus))
	    ((plusp ac-val)
	     (format stream " [~d]" ac-val)))
      )))


(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/book) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  (declare (ignore verbosity store))
  (write-pluralised-string stream "& ritual-book~" (aobj.number obj)
			   :numeric-prefix numeric-prefix)
  (format stream " called '~A'" (object.name obj)))


(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/scroll) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  (declare (ignore verbosity))
  (let* ((o-type (aobj.kind obj))
	 (flavour (if store nil (object.flavour o-type)))
	 (known-type (or store (object.aware (aobj.kind obj)))))
    
    (write-pluralised-string stream "& scroll~" (aobj.number obj)
			     :numeric-prefix numeric-prefix)
    
    (cond ((and (not known-type) flavour)
	   (format stream " \"~A\"" (flavour.name flavour)))
	  (t
	   (format stream " of ~A" (object.name o-type))))))
      
  
(defmethod write-obj-description ((variant vanilla-variant) (obj active-object/weapon) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))
  "this one should be moved out into the variant directories.  it conses"

  (declare (ignore verbosity))
  (let* ((o-type (aobj.kind obj))
	 (number (aobj.number obj))
	 (known-obj (is-object-known? obj))
	 (base (plural-name number (object.name o-type) nil (or store known-obj) nil
			    :numeric-prefix numeric-prefix))
	 (suffix (if (aobj.ego obj) (format nil " ~a" (ego.name (aobj.ego obj))) ""))
	 (tohit-mod (get-tohit-modifier obj))
	 (dmg-mod (get-damage-modifier obj))
	 )
    (cond (known-obj
	   (format stream "~a~a (~@d,~@d)" base suffix tohit-mod dmg-mod))
	  (t
	   (write-string base stream)))))

(defmethod write-obj-description ((variant vanilla-variant) (obj active-object) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))
  (let* ((o-type (aobj.kind obj))
	 (name (object.name obj))
	 (flavour (if store nil (object.flavour o-type)))
	 (known-type (or store (is-object-known? obj)))
	 (number (aobj.number obj))
	 ;;(plural-string nil)
	 )


    (let ((str (typecase obj
		 (active-object/mushroom
		  (plural-name number "& #mushroom~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/potion
		  (plural-name number "& #potion~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/ring
		  (error "Ring should be handled elsewhere"))
		 (active-object/staff
		  (plural-name number "& #staff~@" flavour known-type name :numeric-prefix numeric-prefix))  
		 (active-object/wand
		  (plural-name number "& #wand~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/rod
		  (plural-name number "& #rod~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (active-object/amulet
		  (plural-name number "& #amulet~@" flavour known-type name :numeric-prefix numeric-prefix))
		 (otherwise
		  (plural-name number name nil known-type nil :numeric-prefix numeric-prefix))  
		 )))
      (write-string str stream))

    (when (typep obj 'active-object/rod)
      (when (plusp (aobj.recharge-time obj))
	(write-string " {recharging}" stream)))
    
    ))


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))

    (when (getf keyword-args :depth)
      (signal-condition 'illegal-object-data :id id :desc "Found deprecated :depth keyword, use :power-lvl instead."))
    (when (getf keyword-args :locale)
      (signal-condition 'illegal-object-data :id id :desc "Found deprecated :locale keyword, use :locations instead."))
    (when (getf keyword-args :chance)
      (signal-condition 'illegal-object-data :id id :desc "Found deprecated :chance keyword, use :locations instead."))
    
    ;; add stuff here
    
    new-obj))

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/ammo) keyword-args)

  (call-next-method)
  
  (when-bind (e-t (getf keyword-args :visual-effect))
    ;; get effect-type
    (when-bind (lookup (gethash e-t (variant.visual-effects var-obj)))
      (setf (object.effect-type new-obj) lookup)))

  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/spellbook) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))
    (when-bind (spells (getf keyword-args :spells))
      (cond ((consp spells)
	     (let ((book (create-spellbook (object.name new-obj) (get-id new-obj) spells)))
	       (register-spellbook& var-obj book)))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format of :spells data for spellbook."))))

    new-obj))

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/prayerbook) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))
    (when-bind (spells (getf keyword-args :spells))
      (cond ((consp spells)
	     (let ((book (create-spellbook (object.name new-obj) (get-id new-obj) spells)))
	       (register-spellbook& var-obj book)))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format for :spells data for prayerbook"))))
    new-obj))

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/weapon) keyword-args)
  
  (call-next-method)

  (when-bind (slays (getf keyword-args :slays))
    ;; improve checking?
    (cond ((consp slays)
	   (if (every #'(lambda (x) (is-legal-slay? var-obj x)) slays)
	       (setf (get-slays new-obj) slays)
	       (signal-condition 'illegal-object-data :id (get-id new-obj)
				 :desc (format nil "Unknown slays ~s for weapon"
					       (remove-if-not #'(lambda (x)
								  (is-legal-slay? var-obj x)) slays)))))
	  (t
	   (signal-condition 'illegal-object-data :id (get-id new-obj)
			     :desc "Unknown format for slays info for weapon"))))
  
  new-obj)

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/bow) keyword-args)

  (call-next-method)

  ;; get bow multiplier
  (when-bind (multiplier (getf keyword-args :multiplier))
    (cond ((non-negative-integer? multiplier)
	   (setf (object.multiplier new-obj) multiplier))
	  (t
	   (signal-condition 'illegal-object-data :id (get-id new-obj)
			     :desc "Unknown format for multiplier data for bow"))))
  
  new-obj)


(defmethod get-visual-projectile ((obj active-object/wand))
  (object.effect-type (aobj.kind obj)))

(defmethod get-visual-projectile ((obj active-object/rod))
  (object.effect-type (aobj.kind obj)))

(defmethod get-visual-projectile ((obj active-object/ammo))
  (object.effect-type (aobj.kind obj)))


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/wand) keyword-args)

  (call-next-method)

  (when-bind (e-t (getf keyword-args :effect-type))
    (unless (is-legal-effect-type? e-t)
      (signal-condition 'illegal-object-data :id (get-id new-obj) :desc "Uknown effect-type for wand."))
    ;; get effect-type
    (when-bind (lookup (gethash e-t (variant.visual-effects var-obj)))
      
      (setf (object.effect-type new-obj) lookup)))

  new-obj)
  
(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/rod) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))
    (when-bind (e-t (getf keyword-args :effect-type))
      (unless (is-legal-effect-type? e-t)
	(signal-condition 'illegal-object-data :id id :desc "Uknown effect-type for rod."))
      ;; get effect-type
      (when-bind (lookup (gethash e-t (variant.visual-effects var-obj)))
	
	(setf (object.effect-type new-obj) lookup)))

    (when-bind (recharge (getf keyword-args :recharge-time))
      (unless (positive-integer? recharge)
	(signal-condition 'illegal-object-data :id id :desc "Uknown recharge-time for rod."))
      (setf (object.recharge-time new-obj) recharge))
    
    new-obj))


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/food) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))
    (when-bind (food-val (getf keyword-args :food-value))
      (unless (non-negative-integer? food-val)
	(signal-condition 'illegal-object-data :id id :desc "Food-value not >= 0."))
      (setf (object.food-value new-obj) food-val))

    new-obj))

(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/potion) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))
    (when-bind (food-val (getf keyword-args :food-value))
      (unless (non-negative-integer? food-val)
	(signal-condition 'illegal-object-data :id id :desc "Food-value not >= 0."))
      (setf (object.food-value new-obj) food-val))

    new-obj))


(defmethod get-charge-status ((obj active-object))
  nil)

;; may need updated with artifacts
(defmethod get-charge-status ((obj active-object/light-source))
  (let ((kind (aobj.kind obj)))
    (when-bind (descs (object.status-descs kind))
      (let ((charges (aobj.charges obj))
	    (max-charge (object.max-fuel kind)))
	
	(when (numberp charges)
	  ;; hackish
	  (let ((ratio (int-/ (* 100 charges) max-charge)))
	    ;; (warn "Ratio is ~d/~d -> ~d" charges max-charge ratio)
	    (cond ((> ratio 90) (elt descs 0))
		  ((> ratio 70) (elt descs 1))
		  ((> ratio 30) (elt descs 2))
		  ((> ratio 10) (elt descs 3))
		  ((> ratio 0) (elt descs 4))
		  ((<= ratio 0) (elt descs 5))
		  #-(or cmu sbcl)
		  (t
		   (error "Never fall this far!"))))
	  ))
      )))

(defmethod get-stat-sustains ((object active-object/vanilla-object))
  "Includes EGO bonuses."
  (logior (get-stat-sustains (aobj.kind object))
	  (if (aobj.ego object)
	      (get-stat-sustains (aobj.ego object))
	      0)))

(defmethod get-immunities ((object active-object/vanilla-object))
  (logior (get-immunities (aobj.kind object))
	  (if (aobj.ego object)
	      (get-immunities (aobj.ego object))
	      0)))

(defmethod get-resists ((object active-object/vanilla-object))
  (logior (get-resists (aobj.kind object))
	  (if (aobj.ego object)
	      (get-resists (aobj.ego object))
	      0)))

(defmethod get-ignores ((object active-object/vanilla-object))
  (logior (get-ignores (aobj.kind object))
	  (if (aobj.ego object)
	      (get-ignores (aobj.ego object))
	      0)))

(defmethod get-vulnerabilities ((object active-object/vanilla-object))
  (logior (get-vulnerabilities (aobj.kind object))
	  (if (aobj.ego object)
	      (get-vulnerabilities (aobj.ego object))
	      0)))

(defmethod produce-active-object ((variant variant) (okind object-kind/light-source))
  (let ((obj (call-next-method)))
    ;;(warn "light source ~s" (object.charges okind))
    (when (plusp (object.charges okind))
      (setf (aobj.charges obj) (object.charges okind)))
    obj))


(defmethod produce-active-object ((variant variant) (aobj active-object/light-source))
  (let ((obj (produce-active-object variant (aobj.kind aobj))))
    ;;(warn "coopy")
    (setf (aobj.charges obj) (aobj.charges aobj))
    obj))

(defmethod stackable? ((obj-a active-object/light-source) (obj-b active-object/light-source))
  (let ((retval (call-next-method)))
    ;;(warn "Comparing ~s and ~s -> ~s" obj-a obj-b retval)
    (and retval
	 (= (aobj.charges obj-a) (aobj.charges obj-b)))
    ))

(defmethod stackable? ((obj-a active-object/wand) (obj-b active-object/wand))
  (let ((retval (call-next-method)))
    (and retval
	 (= (aobj.charges obj-a) (aobj.charges obj-b)))
    ))

(defmethod stackable? ((obj-a active-object/staff) (obj-b active-object/staff))
  (let ((retval (call-next-method)))
    (and retval
	 (= (aobj.charges obj-a) (aobj.charges obj-b)))
    ))

(defmethod stackable? ((obj-a active-object/rod) (obj-b active-object/rod))
  (let ((retval (call-next-method)))
    (and retval
	 (= (aobj.recharge-time obj-a) (aobj.recharge-time obj-b)))
    ))

(defmethod copy-active-object ((variant vanilla-variant) (obj active-object/wand) &optional (target nil))
  (let ((retobj (call-next-method variant obj target)))
    ;;(warn "copying to ~s ~s" target (eq target retobj))
    (setf (aobj.charges retobj) (aobj.charges obj))
    
    retobj))

(defmethod copy-active-object ((variant vanilla-variant) (obj active-object/staff) &optional (target nil))
  (let ((retobj (call-next-method variant obj target)))

    (setf (aobj.charges retobj) (aobj.charges obj))
    
    retobj))


(defmethod initialise-object-kind! ((var-obj vanilla-variant) (new-obj object-kind/light-source) keyword-args)

  (call-next-method)

  (let ((id (get-id new-obj)))
    
    (when-bind (descs (getf keyword-args :status-descs))
      (cond ((consp descs)
	     (setf (object.status-descs new-obj) descs))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Status-desc format for light-source unknown."))))
    
    (when-bind (fuel (getf keyword-args :max-fuel))
      (cond ((non-negative-integer? fuel)
	     (setf (object.max-fuel new-obj) fuel))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of max-fuel for light-source unknown."))))

    (when-bind (charges (getf keyword-args :charges))
      (cond ((non-negative-integer? charges)
	     (setf (object.charges new-obj) charges))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of charges for light-source unknown."))))
    
    new-obj))

(defun magic-bonus-for-level (max level)
  "Wrapper for get-level-appropriate-enchantment."
  (get-level-appropriate-enchantment *variant* level max))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object) depth quality)

  (let ((add-magic-effect (get-object-effect variant item :add-magic)))
    (when (and add-magic-effect
	       (effect-entry-p add-magic-effect)
	       (functionp (effect-entry-fun add-magic-effect)))
      (funcall (effect-entry-fun add-magic-effect) item depth quality))
    
    t))


(defun van/add-ego-ability! (variant item depth quality)
  (declare (ignore quality)) ;; use this later
  (block make-ego-item
    ;;(warn "ego ~s ~s" quality (get-id item))
    ;; skip boost

    (let ((total 0)
	  (table (gobj-table.alloc-table (get-named-gameobj-table variant "level"
								  'ego-items-by-level))))

      (loop named counting-area
	    for a-obj across table
	    do
	    (progn
	      (setf (alloc.prob3 a-obj) 0)
	   
	      (when (>= depth (alloc.depth a-obj))
		;;(warn "Checked ~s at ~s vs ~s" (alloc.obj a-obj) (alloc.depth a-obj) depth)
		;; test if the ego-obj can fit
		(let* ((ego (alloc.obj a-obj))
		       (types (ego.obj-types ego)))
		  (dolist (i types)
		    (cond ((symbolp i)
			   (when (satisfies-obj-type? i item)
			     ;;(warn "~s satisfied ~s at ~s" ego item depth)
			     (setf (alloc.prob3 a-obj) (alloc.prob2 a-obj))))
			  (t nil)))
		  ))
	      (incf total (alloc.prob3 a-obj))))

      (when (= 0 total)
	(warn "No suitable ego-items at depth ~a for ~a" depth item)
	(return-from make-ego-item nil))

    
      (let ((val (random total)))
	(loop for a-obj across table
	      do
	      (when (< val (alloc.prob3 a-obj))
		(assert (typep item 'active-object/vanilla-object))
		(setf (aobj.ego item) (alloc.obj a-obj))
		(return-from make-ego-item t))
	      (decf val (alloc.prob3 a-obj))))

      nil)))

(defun van/add-magic-to-weapon! (variant item depth quality)
  (block add-magic-to-item!
    (when (eq quality :normal)
      (return-from add-magic-to-item! nil))

    ;; skip ego-check for :great and :horrible
    (when (or (eq quality :great) (eq quality :horrible))
      (van/add-ego-ability! variant item depth quality)) ;; returns t for success and nil otherwise

    (let ((to-hit (+ (randint 5) (get-level-appropriate-enchantment variant depth 5)))
	  (to-dmg (+ (randint 5) (get-level-appropriate-enchantment variant depth 5)))
	  (to-hit-extra (+ (get-level-appropriate-enchantment variant depth 10)))
	  (to-dmg-extra (+ (get-level-appropriate-enchantment variant depth 10)))
	  )

      (case quality
	(:good
	 (incf (get-damage-modifier item) to-dmg)
	 (incf (get-tohit-modifier item) to-hit))
	(:great
	 (incf (get-damage-modifier item) (+ to-dmg to-dmg-extra))
	 (incf (get-tohit-modifier item) (+ to-hit to-hit-extra)))
	(:bad
	 (decf (get-damage-modifier item) to-dmg)
	 (decf (get-tohit-modifier item) to-hit))
	(:horrible
	 (decf (get-damage-modifier item) (+ to-dmg to-dmg-extra))
	 (decf (get-tohit-modifier item) (+ to-hit to-hit-extra)))
	(otherwise
	 (warn "Unknown quality ~s wanted for item ~s" quality item)))

      ;; skip cursed flag
      ;; skip super-charges

      ;;    (warn "Added magic (~s,~s) to ~s" to-hit to-dmg item)
    
      t)))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/weapon) depth quality)
  (van/add-magic-to-weapon! variant item depth quality))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/ammo) depth quality)
  (van/add-magic-to-weapon! variant item depth quality))

(defmethod add-magic-to-item! ((variant vanilla-variant) (item active-object/armour) depth quality)

  (when (eq quality :normal)
    (return-from add-magic-to-item! nil))

  ;; skip ego-check for :great and :horrible
  (when (or (eq quality :great) (eq quality :horrible))
    (van/add-ego-ability! variant item depth quality)) ;; returns t for success and nil otherwise

  (let ((to-ac1 (+ (randint 5) (get-level-appropriate-enchantment variant depth 5)))
	(to-ac2 (+ (get-level-appropriate-enchantment variant depth 10)))
	)

    (case quality
      (:good
       (incf (get-armour-modifier item) to-ac1))
      (:great
       (incf (get-armour-modifier item) (+ to-ac1 to-ac2)))
      (:bad
       (decf (get-armour-modifier item) to-ac1)
       (curse-object! item :light))
      (:horrible
       (decf (get-armour-modifier item) (+ to-ac1 to-ac2))
       (curse-object! item :light))
      (otherwise
       (warn "Unknown quality ~s wanted for item ~s" quality item)))

    ;; skip cursed flag
    ;; skip super-charges

    ))
  

(defmethod add-magic-to-item! :after ((variant vanilla-variant) (item active-object/vanilla-object) depth quality)
  (declare (ignore depth quality))

  (when-bind (ego (aobj.ego item))
    ;;(warn "Made ego ~s for ~s" ego item)
    ;; add any cursed flag
    (bit-flag-add! (get-sanctity item) (get-sanctity ego))
    ;; skip xtra
    ;; skip broken
    ;; skip cursed
    ;; skip penalties/bonuses
    ;; skip level-feeling
    ))
  
(defun add-charges! (item charges)
  (check-type item active-object)
  (incf (aobj.charges item) charges))

(defmethod print-object ((inst active-object/light-source) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~a ~S, ~a charges]" (lbsys/class-name inst)
	   (aobj.number inst) (get-id (aobj.kind inst)) (aobj.charges inst))
  inst))

(defmethod create-gold ((variant vanilla-variant) (dungeon dungeon) &key originator)

  (declare (ignore originator)) ;; fix for creeping coins
  
  (let* ((gold-table (variant.gold-table variant))
	 (gold-len (length gold-table))
	 (obj-level (dungeon.depth dungeon))
	 (which-gold (- (int-/ (+ (randint (+ 2 obj-level)) 2)
			   2)
			1)))
    (when (>= which-gold gold-len)
      (setf which-gold (1- gold-len)))
    
    (let* ((gold-kind (aref gold-table which-gold))
	   (base-amount (object.cost gold-kind))
	   (amount (+ base-amount (* 8 (randint base-amount)) (randint 8))))
      
;;      (warn "Making ~s gold (~a) of kind ~s" amount (object.name gold-kind) which-gold)
      
      (create-aobj-from-kind gold-kind :amount amount :variant variant))))


(defmethod use-object! ((variant vanilla-variant) dungeon player the-object &key (which-use :use))
;;  (declare (ignore var))
  (check-type the-object active-object)
  
  (let* ((okind (aobj.kind the-object))
	 (effects (object.effects okind))
	 (the-effect (find which-use effects :key #'effect-entry-type))
	 (retval :not-used))

    ;;(warn "Found use-effect ~s for ~s ~s" the-effect (aobj.number the-object) the-object)

    (unless the-effect 
      (warn "Didn't find ~s effect for ~s" which-use (get-id okind))
      (return-from use-object! retval))

    (when (typep the-object 'active-object/rod)
      (when (plusp (aobj.recharge-time the-object))
	(print-message! "The rod is still recharging.")
	(return-from use-object! :not-used)))

    (when (typep the-object 'active-object/wand)
      (when (< (aobj.charges the-object) 1)
	(print-message! "The wand has no charges left.")
	(return-from use-object! :not-used)))

    (when (typep the-object 'active-object/staff)
      (when (< (aobj.charges the-object) 1)
	(print-message! "The staff has no charges left.")
	(return-from use-object! :not-used)))

    (when the-effect
      (assert (and (effect-entry-p the-effect)
		   (functionp (effect-entry-fun the-effect))))
;;      (unless (compiled-function-p (effect-entry-fun the-effect))
;;	(warn "not compiled"))
      
      (setf retval (funcall (effect-entry-fun the-effect) dungeon player the-object))

      (cond ((eq retval :used)
	     (ask-for-redraw! player '[equipment]) ;;possibly used up, time to updated equip display
	     (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	    ((eq retval :still-useful)
	     (cond ((typep the-object 'active-object/rod)
		    (setf (aobj.recharge-time the-object) (object.recharge-time (aobj.kind the-object)))
		    (ask-for-redraw! player '[equipment]) ;; probable reason for redrawing equipment
		    )
		   
		   ((or (typep the-object 'active-object/wand)
			(typep the-object 'active-object/staff))
		    (ask-for-redraw! player '[equipment]) ;; probable reason for redrawing equipment
		    (decf (aobj.charges the-object))))
	     
	     (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	    
	    ;; do nothing
	    ((eq retval :not-used) nil)
	    ((eq retval nil)
	     (warn "Object-effect ~s for object ~s returned nil, fix?"
		   (effect-entry-type the-effect) the-object))
	    (t
	     (error "Unknown return-value from effect: ~s" retval))))
      
    
    retval))

(defmethod create-aobj-from-spec ((variant vanilla-variant) spec)
  "Creates an object from a spec by guessing wildly."
  (let ((retobj (call-next-method)))
    (cond ((and (consp spec) (eq (car spec) 'obj))
	   (destructuring-bind (dummy-id &key (ego nil) &allow-other-keys)
	       spec
	     (declare (ignore dummy-id))

	     (cond (ego
		    (let ((ego-obj (get-ego-item variant ego)))
		      (cond (ego-obj
			     ;;(warn "assigning ego ~s to ~s" ego-obj retobj)
			     (setf (aobj.ego retobj) ego-obj))
			    (t
			     (warn "Unable to find ego-item ~s for object with id ~s"
				   ego (get-id retobj))))))
		   (t
		    nil))
	     
	     ))
	  ((integerp spec)
	   (error "mofo spec to create-aobj-from-spec ~s" spec))
		   
	  (t
	   (warn "Don't know how to handle obj-creation in vanilla from ~s" spec)))
    
    retobj))
