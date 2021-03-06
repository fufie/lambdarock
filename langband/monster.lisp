;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: monster.lisp - monster-code
Copyright (c) 2000-2004, 2009 - Stig Erik Sandoe

||#

(in-package :org.langband.engine)

(defmethod is-monster? (obj)
  (declare (ignore obj))
  nil)

(defmethod is-monster? ((obj active-monster))
  t)

(defmethod is-unique-monster? (obj)
  (declare (ignore obj))
  nil)

(defmethod is-unique-monster? ((obj active-monster))
  (is-unique-monster? (amon.kind obj)))

(defmethod is-unique-monster? ((obj unique-monster))
  t)

(defmethod is-creatable? ((variant variant) (mon monster-kind))
  t)

(defmethod is-creatable? ((variant variant) (mon unique-monster))
  ;; also check if it is on the level, already generated
  (if (monster.already-dead mon)
      nil
      t))

(defmethod is-blind? ((variant variant) (creature active-monster))
  nil)

(defmethod is-sleeping? ((variant variant) (creature active-monster))
  (when-bind (attr (gethash '<sleeping> (amon.temp-attrs creature)))
    (plusp (attr.value attr))))



(defmethod has-ability? ((mon monster-kind) ability)
  (dolist (i (monster.abilities mon))
    (cond ((and (symbolp i) (eq ability i))
	   (return-from has-ability? i))
	  ((and (consp i) (eq (car i) ability))
	   (return-from has-ability? i))))
  nil)
	  
(defmethod has-ability? ((mon active-monster) ability)
  (has-ability? (amon.kind mon) ability))

(defmethod produce-monster-kind ((variant variant) id name &key the-kind)
  (assert (stringp id))
  (assert (stringp name))

  (flet ((make-of-class (cls)
	   (make-instance cls :id id :name name)))
    (declare (dynamic-extent #'make-of-class))
    (let ((ret-obj nil))
      (when (consp the-kind)
	(when (find '<unique> the-kind)
	  (setf ret-obj (make-of-class 'unique-monster))))

      (unless ret-obj
	(setf ret-obj (make-of-class 'monster-kind)))
      
      ret-obj)))

(defmethod produce-active-monster ((variant variant) mon-type)

  (assert (not (eq mon-type nil)))

  
  (let ((the-kind (cond ((symbolp mon-type)
			 (get-monster-kind variant (symbol-name mon-type)))
			((stringp mon-type)
			 (get-monster-kind variant mon-type))
			((typep mon-type 'monster-kind)
			 mon-type)
			(t
			 (error "Mon-type argument to produce-active-monster is not {symbol,string,mkind}, but is ~s"
				mon-type)))))
    
    (unless (typep the-kind 'monster-kind)
      (warn "Unable to find the monster-kind ~s" mon-type)
      (return-from produce-active-monster nil))

    (unless (is-creatable? variant the-kind)
      (warn "Tried to produce dead unique ~a, failed" (get-id the-kind)) 
      (return-from produce-active-monster nil))

    (make-instance 'active-monster :kind the-kind)))


;; a bit hackish
(defmethod produce-active-monster :around ((variant variant) mon-type)
  (let ((amon (call-next-method variant mon-type))
	(the-kind nil))

    (unless amon
      (return-from produce-active-monster nil))
    
    ;;(warn "Amon in around is ~s" amon)
    
    (setf the-kind (amon.kind amon))

    ;; ensure hitpoints are assigned!
    (let ((num-hitdice (car (monster.hitpoints the-kind)))
	  (hitdice (cdr (monster.hitpoints the-kind))))
      
      (if (has-ability? the-kind '<max-hitpoints>)
	  (setf (current-hp amon) (* num-hitdice hitdice))
	  (setf (current-hp amon) (roll-dice num-hitdice hitdice)))
      
      (setf (maximum-hp amon) (current-hp amon)))
    
    (setf (get-creature-speed amon) (monster.speed the-kind))
      ;;    (warn "Monster ~a got ~a hp from ~a dice" (get-creature-name amon)
      ;;	  (current-hp amon) (monster.hitpoints kind))
      ;; blah
    
    (when (has-ability? the-kind '<initial-sleeper>)
      ;; sleepy!
      )

    ;;(warn "Producing ~s" amon)
    
    (when (eq (slot-value amon 'gfx-sym) nil)
      (setf (slot-value amon 'gfx-sym) (gfx-sym the-kind)))
    
    (when (eq (slot-value amon 'text-sym) nil)
      (setf (slot-value amon 'text-sym) (text-sym the-kind)))
    
    amon))
   


(defmethod get-xp-value ((creature active-monster))
  (monster.xp (amon.kind creature)))

(defmethod get-creature-name ((creature active-monster))
  (monster.name (amon.kind creature)))

(defmethod get-creature-name ((creature monster-kind))
  (monster.name creature))

(defmethod get-creature-ac ((creature active-monster))
  (monster.armour (amon.kind creature)))


(defmethod monster.name ((creature active-monster)) ;; remove eventually
  (monster.name (amon.kind creature)))

(defmethod get-id ((creature active-monster)) ;; remove eventually
  (get-id (amon.kind creature)))

(defmethod get-creature-name ((creature player))
  (player.name creature))

(defmethod get-creature-name ((trap active-trap)) ;; :-)
  (trap.name (decor.type trap)))

(defmethod get-power-lvl ((obj active-monster))
  (get-power-lvl (amon.kind obj)))

(defmethod get-power-lvl ((obj monster-kind))
  (monster.power-lvl obj))


(defmethod modify-xp! ((mon active-monster) amount)
  (declare (ignore amount))
  nil)

(defmethod add-creature-attribute ((amon active-monster) attr)
  (ecase (attr.type attr)
    (:temporary (setf (gethash (attr.key attr) (amon.temp-attrs amon))
		      attr))))

(defmethod monster.attacks ((mon active-monster))
  (monster.attacks (amon.kind mon)))

(defmethod get-mkind-table ((var-obj variant) (level level))
  (gobj-table.obj-table (get-mtype-table var-obj level)))

(defmethod get-mkind-alloc-table ((var-obj variant) (level level))
  (gobj-table.alloc-table (get-mtype-table var-obj level)))

(defmethod get-monster-kind ((variant variant) mon)
  "Returns monster-kind or nil for a variant (not level dependent.  I think this one is quite limited."
  (etypecase mon
    (symbol (gethash (symbol-name mon) (variant.monsters variant)))
    (string (gethash mon (variant.monsters variant)))
    (integer (block foo
	       (loop for x being the hash-values of (variant.monsters variant)
		     do
		     (when (eql mon (monster.numeric-id x))
		       (return-from foo x))
		     ))
	     )))


(defmethod convert-obj (attacks (to (eql :attk-list)) &key)
  (mapcar #'(lambda (x)
	      (list (attack.kind x)
		    :type (if (typep (attack.dmg-type x) 'attack-type)
			      (attack-type.key (attack.dmg-type x))
			      (attack.dmg-type x))
		    :damage (attack.damage x)))
	  attacks))

(defmethod convert-obj (attk-list (to (eql :attacks)) &key)
  "Converts attacks in list-form to a list of attack instances."
  (loop for i in attk-list
	if (and (consp i) (symbolp (car i)))
	collect (make-instance 'attack :kind (car i)
			       :dmg-type (when-bind (wanted-type (getf (cdr i) :type))
					   (if (get-attack-type wanted-type)
					       (get-attack-type wanted-type)
					       wanted-type))
			       :damage (getf (cdr i) :damage))
	else if (not (eq i nil)) do
	(warn "Unknown attack-info ~s" i)))


(defmethod get-monster-list ((var-obj variant) &key (sort-key #'get-id) (predicate #'string<))
  "returns a fresh list of all monsters for the variant, sorted appropriately."
  (assert (functionp sort-key))
  (assert (functionp predicate))
  
  (let ((table (variant.monsters var-obj)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 predicate
		 :key sort-key)))


(defun %parse-treasure-spec (spec)
  "Parses the spec and returns data in a good format."

  (assert (consp spec))

  (let ((drops '())
	(quality :normal)
	(type :any))
    
    (dolist (i spec)
      (assert (or (consp i) (symbolp i)))
      
      (cond ((eq i '<drop-good>)
	     (setf quality :good))
	    ((eq i '<drop-great>)
	     (setf quality :great))
	    ((eq i '<drop-planned>)
	     ;; fix this later
	     )
	    ((eq i '<only-drop-items>)
	     (setf type :item))
	    ((eq i '<only-drop-gold>)
	     (setf type :gold))
	    ((consp i)
	     (ecase (car i)
	       (<drop>
		(check-type (cadr i) string)
		(push (make-instance 'treasure-drop :amount (parse-dice (cadr i)))
		      drops))
	       (<drop-chance>
		(check-type (cadr i) number)
		(push (make-instance 'treasure-drop :chance (cadr i))
		      drops))))
	     
	    (t
	     (error "Unknown treasure-argument ~s" i)))
	    

      )

      (dolist (i drops)
	(setf (drop.quality i) quality)
	(setf (drop.type i) type))
      
      drops))



(defmethod initialise-monster-kind! ((var-obj variant) (m-obj monster-kind) keyword-args)
  (let ((id (get-id m-obj)))

    (when-bind (numeric-id (getf keyword-args :numeric-id))
      (cond ((integerp numeric-id)
	     (unless (non-negative-integer? numeric-id)
	       (signal-condition 'illegal-monster-data :id id :desc "numeric-id negative for monster-kind"))
	     (setf (monster.numeric-id m-obj) numeric-id))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for numeric-id data for mon-kind"))))
    
    (when-bind (desc (getf keyword-args :desc))
      (cond ((stringp desc)
	     (unless (plusp (length desc))
	       (signal-condition 'illegal-monster-data :id id :desc "empty desc for monster-kind"))
	     (setf (monster.desc m-obj) desc))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for desc data for monster-kind"))))
      
    (when-bind (xp (getf keyword-args :xp))
      (cond ((integerp xp)
	     (unless (non-negative-integer? xp)
	       (signal-condition 'illegal-monster-data :id id :desc "xp negative for monster-kind"))
	     (setf (monster.xp m-obj) xp))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for xp data for monster-kind"))))

    
    (when-bind (speed (getf keyword-args :speed))
      (cond ((integerp speed)
	     (unless (non-negative-integer? speed)
	       (signal-condition 'illegal-monster-data :id id :desc "speed negative for monster-kind"))
	     (setf (monster.speed m-obj) speed))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for speed data for monster-kind"))))

    (when-bind (power-lvl (getf keyword-args :power-lvl))
      (cond ((integerp power-lvl)
	     (unless (non-negative-integer? power-lvl)
	       (signal-condition 'illegal-monster-data :id id :desc "power-lvl negative for monster-kind"))
	     
	     (setf (monster.power-lvl m-obj) power-lvl))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format on power-lvl data for mon-kind"))))

    
    (when-bind (armour (getf keyword-args :armour))
      (cond ((integerp armour)
	     (unless (non-negative-integer? armour)
	       (signal-condition 'illegal-monster-data :id id :desc "armour negative for monster-kind"))
	     (setf (monster.armour m-obj) armour))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for armour data for mon-kind"))))

    (when-bind (alertness (getf keyword-args :alertness))
      (cond ((integerp alertness)
	     (unless (non-negative-integer? alertness)
	       (signal-condition 'illegal-monster-data :id id :desc "alertness negative for monster-kind"))
	     (setf (monster.alertness m-obj) alertness))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for alertness data for mon-kind"))))

    (when-bind (vision (getf keyword-args :vision))
      (cond ((integerp vision)
	     (unless (non-negative-integer? vision)
	       (signal-condition 'illegal-monster-data :id id :desc "vision negative for monster-kind"))
	     (setf (monster.vision m-obj) vision))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for vision data for mon-kind"))))


    (when-bind (hitpoints (getf keyword-args :hitpoints))
      (cond ((consp hitpoints)
	     (unless (and (non-negative-integer? (car hitpoints)) (non-negative-integer? (cdr hitpoints)))
	       (signal-condition 'illegal-monster-data :id id :desc "hitpoints for mon-kind not non-negative integers."))
	     (setf (monster.hitpoints m-obj) hitpoints))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown format for hitpoints data for mon-kind"))))

    (when-bind (gender (getf keyword-args :gender))
      (cond ((nonboolsym? gender)
	     ;; check value?
	     (setf (monster.gender m-obj) gender))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown gender for monster-kind"))))
    
    ;; maybe move to variant?
    (when-bind (alignment (getf keyword-args :alignment))
      (cond ((nonboolsym? alignment)
	     ;; check value?
	     (setf (monster.alignment m-obj) alignment))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown alignment for monster-kind"))))


    (when-bind (type (getf keyword-args :type))
      (cond ((consp type)
	     (setf (monster.type m-obj) type))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown type-info for monster-kind"))))


    (when-bind (attacks (getf keyword-args :attacks))
      (cond ((consp attacks)
	     (let ((attks (convert-obj attacks :attacks)))
	       (setf (monster.attacks m-obj) attks)))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown attacks argument for monster-kind"))))

    (when-bind (treasures (getf keyword-args :treasures))
      (cond ((consp treasures)
	     (setf (monster.treasures m-obj) (%parse-treasure-spec treasures)))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Illegal format for monster-kind treasures"))))

    (when-bind (appear-in-group? (getf keyword-args :appear-in-group?))
      (cond ((functionp appear-in-group?)
	     (setf (monster.in-group m-obj) appear-in-group?))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "in-group for monster is not a function"))))

    (when-bind (abilities (getf keyword-args :abilities))
      (cond ((consp abilities)
	     (setf (monster.abilities m-obj) abilities))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Illegal format for monster-kind abilities."))))

    (when-bind (special-abilities (getf keyword-args :special-abilities))
      (cond ((consp special-abilities)
	     (setf (monster.sp-abilities m-obj) special-abilities))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Illegal format for monster-kind sp-abilities."))))
    
    (when-bind (vulnerabilities (getf keyword-args :vulnerabilities))
      (cond ((consp vulnerabilities)
	     (dolist (i vulnerabilities)
	       (cond ((and (symbolp i) (not (eq nil i)))
		      (if (is-legal-element? var-obj i)
			  (bit-flag-add! (get-vulnerabilities m-obj) (get-element-flag var-obj i))
			  (signal-condition 'illegal-monster-data :id id
					    :desc "Illegal vulnerability-arg for mon-kind")))
		     (t
		      (signal-condition 'illegal-monster-data :id id
					:desc "Unknown vulnerability argument for mon-kind")))))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Illegal format for mon-kind vulnerabilities."))))

    (when-bind (immunities (getf keyword-args :immunities))
      (cond ((consp immunities)
	     (dolist (i immunities)
	       (cond ((and (symbolp i) (not (eq nil i)))
		      (if (is-legal-element? var-obj i)
			  (bit-flag-add! (get-immunities m-obj) (get-element-flag var-obj i))
			  (signal-condition 'illegal-monster-data :id id
					    :desc "Illegal immunity-arg for mon-kind")))
		     (t
		      (signal-condition 'illegal-monster-data :id id
					:desc "Unknown immunity argument for mon-kind")))))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "non-cons immunity argument for monster-kind"))))

    (when-bind (resists (getf keyword-args :resists))
      (cond ((consp resists)
	     (dolist (i resists)
	       (cond ((and (symbolp i) (not (eq nil i)))
		      (if (is-legal-element? var-obj i)
			  (bit-flag-add! (get-resists m-obj) (get-element-flag var-obj i))
			  (signal-condition 'illegal-monster-data :id id
					    :desc "Illegal resist-arg for mon-kind")))
		     (t
		      (signal-condition 'illegal-monster-data :id id
					:desc "Unknown resist argument for mon-kind")))))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "non-cons resist argument for monster-kind"))))

    
    (when-bind (gfx-sym (getf keyword-args :gfx-sym))
      (cond ((non-negative-integer? gfx-sym)
	     (setf (gfx-sym m-obj) gfx-sym))
	    (t
	     (signal-condition 'illegal-monster-data :id id
			       :desc (format nil "Unknown value for gfx-sym: ~s" gfx-sym))
	     )))
    
    (when-bind (text-sym (getf keyword-args :text-sym))
      (cond ((non-negative-integer? text-sym)
	     (setf (text-sym m-obj) text-sym))
	    (t
	     (signal-condition 'illegal-monster-data :id id
			       :desc (format nil "Unknown value for text-sym: ~s" text-sym))
	     )))
        
    m-obj))

(defvar *ab-list* '())

#-langband-release
(defmethod initialise-monster-kind! :after ((var-obj variant) (m-obj monster-kind) keyword-args)
  "Used to verify data afterwards."

  (declare (ignore keyword-args))
  (let ((id (get-id m-obj)))
    ;; tough demand
    (when-bind (treasures (monster.treasures m-obj))
      (unless (every #'(lambda (x) (typep x 'treasure-drop)) treasures)
	(signal-condition 'illegal-monster-data :id id :desc "Not all treasure-infos are monster-drops")))
    (unless (integerp (monster.power-lvl m-obj))
      (signal-condition 'illegal-monster-data :id id :desc "Non-integer power-lvl for monster-kind."))
    (unless (>= (monster.power-lvl m-obj) 0)
      (signal-condition 'illegal-monster-data :id id :desc "Negative power-lvl for monster-kind."))

    ;; might be changed, check now
    (unless (consp (monster.hitpoints m-obj))
      (signal-condition 'illegal-monster-data :id id :desc "Hitpoints-data not a cons."))

    (let ((descs (variant.attack-descriptions var-obj)))
      (dolist (i (monster.attacks m-obj))
	(unless (typep i 'attack)
	  (signal-condition 'illegal-monster-data :id id :desc "monster.attack info not obj of type attack."))
	(multiple-value-bind (val found-p)
	    (gethash (attack.kind i) descs)
	  (declare (ignore val))
	  (unless found-p
	    (signal-condition 'illegal-monster-data :id id
			      :desc (format nil "Unable to find attack-kind: ~S" (attack.kind i)))))
	
	(when-bind (dmg-type (attack.dmg-type i))
	  (unless (typep dmg-type 'attack-type)
	    (signal-condition 'illegal-monster-data :id id
			      :desc (format nil "Damage-type for attack ~s not known." dmg-type))))
	
	))

    (dolist (i (monster.sp-abilities m-obj))
      (pushnew i *ab-list* :test #'equalp))
    
    ))
    


(defun define-monster-kind (id name &rest keyword-args &key type &allow-other-keys)
  "Defines a critter you might bump into when you least expect it. It uses
the *VARIANT* object so it has to be properly initialised."

  (handler-case
      (progn 
	(assert (or (stringp id) (symbolp id)))
	(assert (stringp name))
	(check-type *variant* variant)
  
	(when (symbolp id)
	  (warn "Deprecated id ~s for object ~s, use a legal string" id name))

	(unless (verify-id id)
	  (signal-condition 'illegal-monster-data :id id :desc "Id is not valid for a monster"))
  
	(let* ((var-obj *variant*)
	       (m-obj (produce-monster-kind var-obj id name :the-kind type)))


	  (initialise-monster-kind! var-obj m-obj keyword-args)

    
	  ;; hackish addition to big object-table
	  (let ((main-obj-table (variant.monsters var-obj))
		(obj-id (get-id m-obj)))
	    (multiple-value-bind (val found-p)
		(gethash obj-id main-obj-table)
	      (declare (ignore val))
	      (when found-p
		(warn "Replacing monster with id ~s" obj-id))
	      (setf (gethash obj-id main-obj-table) m-obj)))

	  ;; applies the filters registered for newly read monsters
	  (apply-filters-on-obj :monsters var-obj m-obj)
	  ;;(add-new-mkind! m-obj id)
    
	  m-obj))
    
    (illegal-monster-data (co)
      (warn "Failed to initialise monster-kind [~a]: ~a"
	    (illegal-data.id co) (illegal-data.desc co))
      nil)))


(defun define-monster-attack (key &key power hit-effect)
  (let ((akind (make-instance 'attack-type :key key)))
    
    (unless (non-negative-integer? power)
      (error-condition 'illegal-attack-data :id key :desc "Power of attack no positive integer."))

    (when (non-negative-integer? power)
      (setf (attack-type.power akind) power))

    (unless (functionp hit-effect)
      (error-condition 'illegal-attack-data :id key :desc "Hit-effect for attack not a function."))

    (when (functionp hit-effect)
      (setf (attack-type.hit-effect akind) (compile nil hit-effect)))
    
    (setf (gethash key (variant.attack-types *variant*)) akind)

    akind))
 

(defmethod appears-in-group? ((variant variant) (level level) (monster active-monster))
  (appears-in-group? variant level (amon.kind monster)))

(defmethod appears-in-group? ((variant variant) (level level) (monster monster-kind))
  (let ((val (monster.in-group monster)))
    (if (functionp val)
	(funcall val level monster)
	nil)))

(defmethod appears-in-group? ((variant variant) (level level) (monster unique-monster))
  nil)

(defmethod trigger-special-ability (variant creature ability target dungeon)
  (declare (ignore variant creature target dungeon))
  (warn "Special ability ~s not implemented" ability)
  t)

(defmethod is-male? (creature)
  nil)

(defmethod is-male? ((creature active-monster))
  (is-male? (amon.kind creature)))

(defmethod is-male? ((creature monster-kind))
  (eq (monster.gender creature) '<male>))

(defmethod is-female? (creature)
  nil)

(defmethod is-female? ((creature active-monster))
  (is-female? (amon.kind creature)))

(defmethod is-female? ((creature monster-kind))
  (eq (monster.gender creature) '<female>))

(defmethod get-resists ((obj active-monster))
  (get-resists (amon.kind obj)))

(defmethod get-immunities ((obj active-monster))
  (get-immunities (amon.kind obj)))

(defmethod get-vulnerabilities ((obj active-monster))
  (get-vulnerabilities (amon.kind obj)))

(defmethod (setf get-resists) (value (obj active-monster))
  (declare (ignore value))
  (error "Call (SETF GET-RESISTS) for MON-KIND."))

(defmethod (setf get-immunities) (value (obj active-monster))
  (declare (ignore value))
  (error "Call (SETF GET-IMMUNITIES) for MON-KIND."))

(defmethod (setf get-vulnerabilities) (value (obj active-monster))
  (declare (ignore value))
  (error "Call (SETF GET-VULNERABILITIES) for MON-KIND."))

(defmethod resists-element? ((object active-monster) element)
  (resists-element? (amon.kind object) element))

(defmethod immune-to-element? ((object active-monster) element)
  (immune-to-element? (amon.kind object) element))

(defmethod vulnerable-to-element? ((object active-monster) element)
  (vulnerable-to-element? (amon.kind object) element))

(defmethod resists-element? ((object monster-kind) element)
  (bit-flag-set? (get-element-flag *variant* element) (get-resists object)))

(defmethod immune-to-element? ((object monster-kind) element)
  (bit-flag-set? (get-element-flag *variant* element) (get-immunities object)))

(defmethod vulnerable-to-element? ((object monster-kind) element)
  (bit-flag-set? (get-element-flag *variant* element) (get-vulnerabilities object)))

(defmethod heal-creature! ((creature active-monster) amount)
  "Heals the monster and adds notify where needed."

  (let ((max-hp (maximum-hp creature)))
  
    (when (< (current-hp creature) max-hp)
      
      (incf (current-hp creature) amount)
      
      (when (< max-hp (current-hp creature)) ;; no more than max..
	(setf (current-hp creature) max-hp))

      t)))

(defmethod modify-creature-state! (creature state &key add subtract new-value)
  t)
