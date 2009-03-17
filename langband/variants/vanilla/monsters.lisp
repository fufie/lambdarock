;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/monsters.lisp - code related to monsters
Copyright (c) 2003 - Stig Erik Sandoe

||#

(in-package :org.langband.vanilla)


(defclass spab ()
  ((id         :accessor spab.id         :initform "dummy")
   (desc       :accessor spab.desc       :initform "special ability")
   (type       :accessor spab.type       :initform 'generic)
   (power      :accessor spab.power      :initform 0)
   (range      :accessor spab.range      :initform 0) ;; 0 is melee
   (effect     :accessor spab.effect     :initform nil)
   (vis-effect :accessor spab.vis-effect :initform nil)
   (damage     :accessor spab.damage     :initform 0) ;; does no direct damage by default
   ;; factors
   (mana-cost  :accessor spab.mana-cost  :initform 0)
   (offensive  :accessor spab.offensive  :initform -1)
   (defensive  :accessor spab.defensive  :initform -1)
   ))

(defclass summon-spab (spab)
  ((what :accessor spab.what :initform nil)))

(defclass ranged-spab (spab)
  ((range      :initform 10)
   (desc       :initform "an arrow")
   (vis-effect :initform "arrow")
   (missile    :accessor spab.missile :initform "arrow")
   ))

(defclass spell-spab (spab)
  ((desc :initform "spell")))

(defclass ball-spell-spab (spell-spab)
  ((desc   :initform "ball")
   (damage :initform 10)
   ))

(defclass bolt-spell-spab (spell-spab)
  ((desc   :initform "bolt")
   (damage :initform 10)
   ))

(defclass dmg-spell-spab (spell-spab)
  ((desc   :initform "damage spell")))

(defclass breath-spab (spab)
  ((desc   :initform "breath")
   (damage :initform 10)
   ))

(defmethod get-visual-projectile ((obj spab))
  (spab.vis-effect obj))


(defvar *registered-spabs* (make-hash-table :test #'equal))

(defun register-spab& (key value)
  (setf (gethash key *registered-spabs*) value)
  value)

(defgeneric define-monster-spab (type subtype &key &allow-other-keys))

(defmethod define-monster-spab ((type (eql '<breath>)) subtype &key breath-type desc
				damage visual mana-cost offensive defensive
				&allow-other-keys)
  
  (assert (is-legal-element? *variant* breath-type))
  ;;(warn "defining spabby ~s ~s ~s" type subtype desc)
  
  (let ((obj (make-instance 'breath-spab)))
    (setf (spab.type obj) breath-type
	  (spab.desc obj) desc)
    
    (when (or (functionp damage) (positive-integer? damage))
      (setf (spab.damage obj) damage))

    (when (non-negative-integer? mana-cost)
      (setf (spab.mana-cost obj) mana-cost))

    (when (non-negative-integer? offensive)
      (setf (spab.offensive obj) offensive))

    (when (non-negative-integer? defensive)
      (setf (spab.defensive obj) defensive))
    
    (when visual
      (unless (is-legal-effect-type? visual)
	(warn "Unknown visual ~s for spab ~s ~s" visual type subtype))
      
      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
	;;(warn "visual lookup is ~s" lookup)
	(setf (spab.vis-effect obj) lookup)))

    (register-spab& (list type subtype) obj)))

(defmethod define-monster-spab ((type (eql '<arrow>)) subtype &key power desc range mana-cost
				offensive defensive missile &allow-other-keys)
  
  (let ((obj (make-instance 'ranged-spab)))
    
    (setf (spab.power obj) power
	  (spab.range obj) range
	  (spab.desc obj) desc)

    (when (non-negative-integer? mana-cost)
      (setf (spab.mana-cost obj) mana-cost))
    
    (when (non-negative-integer? offensive)
      (setf (spab.offensive obj) offensive))

    (when (non-negative-integer? defensive)
      (setf (spab.defensive obj) defensive))

    (when (stringp missile)
      (setf (spab.missile obj) missile))
    
    
    (when-bind (visual (spab.vis-effect obj))
      (unless (is-legal-effect-type? visual)
	(warn "Unknown visual ~s for spab ~s ~s" visual type subtype))
      
      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
	;;(warn "visual lookup is ~s" lookup)
	(setf (spab.vis-effect obj) lookup)))

    (register-spab& (list type subtype) obj)))
   

(defmethod define-monster-spab ((type (eql '<summon>)) subtype &key what mana-cost
				offensive defensive &allow-other-keys)
  
  (let ((obj (make-instance 'summon-spab)))
    
    (unless what
      (setf what subtype))
    
    (when (non-negative-integer? mana-cost)
      (setf (spab.mana-cost obj) mana-cost))

    (when (non-negative-integer? offensive)
      (setf (spab.offensive obj) offensive))

    (when (non-negative-integer? defensive)
      (setf (spab.defensive obj) defensive))
    
    (setf (spab.what obj) what)
    
    (register-spab& (list type subtype) obj)))

(defmethod define-monster-spab ((type (eql '<dmg-spell>)) subtype &key power mana-cost
				offensive defensive &allow-other-keys)
  
  (let ((obj (make-instance 'dmg-spell-spab)))
    
    (setf (spab.power obj) power)
    
    (when (or (functionp mana-cost) (non-negative-integer? mana-cost))
      (setf (spab.mana-cost obj) mana-cost))
    
    (when (or (functionp offensive) (non-negative-integer? offensive))
      (setf (spab.offensive obj) offensive))

    (when (or (functionp defensive) (non-negative-integer? defensive))
      (setf (spab.defensive obj) defensive))

    (register-spab& (list type subtype) obj)))

(defmethod define-monster-spab ((type (eql '<spell>)) subtype &key effect visual
				mana-cost defensive offensive &allow-other-keys)
  
  (let ((obj (make-instance 'spell-spab)))

    (setf (spab.id obj) (format nil "~A" subtype)
	  (spab.type obj) subtype)

    (when (or (functionp mana-cost) (non-negative-integer? mana-cost))
      (setf (spab.mana-cost obj) mana-cost))
    
    (when (or (functionp offensive) (non-negative-integer? offensive))
      (setf (spab.offensive obj) offensive))

    (when (or (functionp defensive) (non-negative-integer? defensive))
      (setf (spab.defensive obj) defensive))

    (when (functionp effect)
      ;;(warn "Assign for ~s" key)
      (setf (spab.effect obj) effect))
    
    (when visual
      (unless (is-legal-effect-type? visual)
	(warn "Unknown visual ~s for spab ~s ~s" visual type subtype))
      
      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
	(setf (spab.vis-effect obj) lookup)))
        
    (register-spab& (list type subtype) obj)))

(defmethod define-monster-spab ((type (eql '<ball-spell>)) subtype &key desc damage visual
				offensive defensive mana-cost &allow-other-keys)
  
  (let ((obj (make-instance 'ball-spell-spab)))

    (assert (is-legal-element? *variant* subtype))
    
    (setf (spab.type obj) subtype
	  (spab.desc obj) desc)
    
    (when (or (functionp damage) (positive-integer? damage))
      (setf (spab.damage obj) damage))

    (when (or (functionp mana-cost) (non-negative-integer? mana-cost))
      (setf (spab.mana-cost obj) mana-cost))

    (when (or (functionp offensive) (non-negative-integer? offensive))
      (setf (spab.offensive obj) offensive))

    (when (or (functionp defensive) (non-negative-integer? defensive))
      (setf (spab.defensive obj) defensive))

    
    (when visual
      (unless (is-legal-effect-type? visual)
	(warn "Unknown visual ~s for spab ~s ~s" visual type subtype))
      
      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
	(setf (spab.vis-effect obj) lookup)))


        
    (register-spab& (list type subtype) obj)))

(defmethod define-monster-spab ((type (eql '<bolt-spell>)) subtype &key desc damage visual
				mana-cost offensive defensive &allow-other-keys)
  
  (let ((obj (make-instance 'bolt-spell-spab)))

    (assert (is-legal-element? *variant* subtype))
    
    (setf (spab.type obj) subtype
	  (spab.desc obj) desc)
    
    (when (or (functionp damage) (positive-integer? damage))
      (setf (spab.damage obj) damage))

    (when (or (functionp mana-cost) (non-negative-integer? mana-cost))
      (setf (spab.mana-cost obj) mana-cost))

    (when (or (functionp offensive) (non-negative-integer? offensive))
      (setf (spab.offensive obj) offensive))

    (when (or (functionp defensive) (non-negative-integer? defensive))
      (setf (spab.defensive obj) defensive))

    
    (when visual
      (unless (is-legal-effect-type? visual)
	(warn "Unknown visual ~s for spab ~s ~s" visual type subtype))
      
      (when-bind (lookup (gethash visual (variant.visual-effects *variant*)))
	(setf (spab.vis-effect obj) lookup)))


        
    (register-spab& (list type subtype) obj)))




(defmethod print-object ((inst spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.id inst)))

  inst)

(defmethod print-object ((inst breath-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.type inst)))
  inst)

(defmethod print-object ((inst bolt-spell-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.type inst)))
  inst)

(defmethod print-object ((inst ball-spell-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.type inst)))
  inst)

(defmethod print-object ((inst summon-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.what inst)))
  inst)

(defmethod print-object ((inst ranged-spab) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (spab.power inst)))
  inst)


(defmacro spab-effect (arguments &body body)
  (assert (= (length arguments) 4))
  (let ((def `(lambda ,arguments
               (declare (ignorable ,@arguments))
	       (block spab-effect
		 ,@body))))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability spab) target dungeon)
  (declare (ignore target dungeon))
  (warn "Vanilla special ability ~s not implemented" ability)
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability spell-spab) target dungeon)

  (when (functionp (spab.effect ability))
    (decf (get-creature-mana creature) (spab.mana-cost ability))
    (warn "FUNCALL: ~A" (spab.id ability))
    (return-from trigger-special-ability
      (funcall (spab.effect ability) creature ability target dungeon)))
  
  ;;(describe ability)

  (case (spab.type ability)
    (otherwise
     (warn "Vanilla spell ability ~s not implemented" ability)))
  
  
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability ball-spell-spab) target dungeon)
  (declare (ignore dungeon))
  ;;(warn "Ball-spell ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? variant *player*)
      (format-message! "~@(~A~) mumbles."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) casts a ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (spab.damage ability))
	(spell-effect (get-spell-effect (spab.type ability))))
    (when (functionp dmg)
      (setf dmg (funcall dmg (get-power-lvl creature))))
    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-fire-ball! creature target spell-effect dmg 3
		    :projected-object ability))
  (decf (get-creature-mana creature) (spab.mana-cost ability))
  
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability breath-spab) target dungeon)
  (declare (ignore dungeon))
  
  ;;(warn "Breath ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? variant *player*)
      (format-message! "~@(~A~) breathes."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) breathes ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (spab.damage ability))
	(spell-effect (get-spell-effect (spab.type ability))))
    (when (functionp dmg)
      (setf dmg (funcall dmg (current-hp creature))))
    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-breath! creature target spell-effect dmg 3
		 :projected-object ability))
  
  t)



(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability bolt-spell-spab) target dungeon)
  (declare (ignore dungeon))
  ;;(warn "Bolt-spell ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? variant *player*)
      (format-message! "~@(~A~) mumbles."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) casts a ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let ((dmg (spab.damage ability))
	(spell-effect (get-spell-effect (spab.type ability))))
    (when (functionp dmg)
      (setf dmg (funcall dmg (get-power-lvl creature))))
    (assert (non-negative-integer? dmg))
    ;;(warn "spell-eff is ~s" spell-effect)
    (van-fire-bolt! creature target spell-effect dmg
		    :projected-object ability))
  (decf (get-creature-mana creature) (spab.mana-cost ability))
  
  t)

(defmethod trigger-special-ability ((variant vanilla-variant) (creature active-monster)
				    (ability ranged-spab) target dungeon)
  (declare (ignore dungeon))
  ;;(warn "Bolt-spell ~A" (spab.type ability))
  
  (disturbance variant *player* creature :major)

  ;; check blindness
  (if (is-blind? variant *player*)
      (format-message! "~@(~A~) makes a strange noise."
		       (get-creature-desc creature #x00))
      (format-message! "~@(~A~) fires ~a."
		       (get-creature-desc creature #x00)
		       (spab.desc ability)
		       ;;(get-creature-desc target #x20)
		       ))
  (let* ((missile (spab.missile ability))
	 (arr-obj (item-table-find (aobj.contains (get-creature-inventory creature))
				   (carries-object? creature missile)))
	 (range (spab.range ability)))

    ;;(warn "Projecting ~s ~s" arr-obj range)
    
    (assert (positive-integer? range))
    ;;(warn "spell-eff is ~s" spell-effect)
    (shoot-a-missile *dungeon* creature target ability arr-obj :range range))
    ;;(van-fire-bolt! creature target spell-effect dmg :projected-object arr-obj #|ability|#))

  
  (block remove-missile
    (when-bind (inv (get-creature-inventory creature))
      (when-bind (container (aobj.contains inv))
	(loop for i from 0
	      for x across (items.objs container)
	      when x
	      do
	      (when (equal (get-id x) (spab.missile ability))
		;;(warn "We had ~s arrows, removing one" (aobj.number x))
		(cond ((> (aobj.number x) 1)
		       (decf (aobj.number x)))
		      (t
		       ;;(warn "Removing arrow")
		       (item-table-remove! container i)))
		(return-from remove-missile t))))))
  
  t)


(defun analyse-special-abilities! (variant mon-kind)
  "Analyses and updates special-abilities info."
  (declare (ignore variant))
  
  (let ((id (get-id mon-kind))
	(sp-abilities (monster.sp-abilities mon-kind))
	(result '())
	(frequency 1))

    (dolist (i sp-abilities)
      (when (and (consp i) (eq (first i) '<frequency>))
	(unless (and (numberp (second i))
		     (> (second i) 0)
		     (<= (second i) 1))
	  (error-condition 'illegal-monster-data :id id
			   :desc "Frequency arg to special-abilities not between <0,1]."))
	(setf frequency (second i))))

    (dolist (ab sp-abilities)

      (cond ((and (consp ab) (eq (first ab) '<frequency>)))
	    (t
	     (let ((lookup (gethash ab *registered-spabs*)))
	       (cond (lookup
		      (push lookup result))
		     (t
		      (warn "~s gave ~s" ab lookup)))))
	    ))

    (when result
      ;; frequency first!
      (setf (monster.sp-abilities mon-kind) (cons (floor (* 100 frequency))
						  result)))

    mon-kind))

(defmethod initialise-monster-kind! ((var-obj vanilla-variant) (m-obj monster-kind) keyword-args)

  (call-next-method)

  (let ((id (get-id m-obj)))

    (when-bind (depth (getf keyword-args :depth))
      (cond ((integerp depth)
	     (unless (non-negative-integer? depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative depth/power-lvl argument for monster"))
	     (setf (monster.power-lvl m-obj) depth))
	    (t
	     (signal-condition 'illegal-monster-data :id id :desc "Unknown depth/power-lvl argument for monster-kind"))))
    
    (let ((depth (getf keyword-args :depth))
	  (rarity (getf keyword-args :rarity)))

      (cond ((and depth rarity)
	     (unless (integerp depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Non-integer depth argument for monster-kind"))
	     (unless (integerp rarity)
	       (signal-condition 'illegal-monster-data :id id :desc "Non-integer rarity argument for monster-kind"))
	     (unless (non-negative-integer? depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative depth argument for monster-kind"))
	     (unless (non-negative-integer? rarity)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative rarity argument for monster-kind"))
	     (push (cons depth rarity) (alloc-locations m-obj)))
	    ((and (eq depth nil) (eq depth rarity)))
	    (t
	     (signal-condition 'illegal-monster-data :id id
			       :desc "Unknown depth + rarity argument for monster-kind"))))


    
#||    
    (when (or (= (monster.power-lvl m-obj) 0)
	      (monster.power-lvl m-obj) 1)
      (setf (monster.sp-abilities m-obj)
	    '(;;(<breath> <nexus>) (<breath> <gravity>) (<breath> <shards>) (<breath> <confusion>)
	      ;;(<spell> <missile>)
	      ;;(<breath> <shards>)
	      ;;(<ball-spell> <cold>) (<breath> <cold>)
	      ;;(<arrow> 2) (<arrow> 3)
	      (<spell> <darkness>)
	      ;;(<ball-spell> <mana>) (<ball-spell> <fire>) (<ball-spell> <acid>)
	      ;;(<breath> <sound>) (<breath> <electricity>) (<breath> <cold>) (<breath> <fire>) (<breath> <acid>)
	      (<frequency> 1/4))))
    ||#
    
    (analyse-special-abilities! var-obj m-obj)

    ;; check that a breather has a resist or immunity
    (dolist (i (monster.sp-abilities m-obj))
      (when (typep i 'breath-spab)
	(unless (or (immune-to-element? m-obj (spab.type i)) (resists-element? m-obj (spab.type i)))
	  (signal-condition 'illegal-monster-data :id id
			    :desc (format nil "Monster breathes ~s but does not resist it." (spab.type i)))
	  )))
    
    m-obj))


(defun van-group-chance (id mon-depth lvl-depth)
  (declare (ignore id))
  (let* ((diff (- lvl-depth mon-depth))
	 (chance (max-cap (if (plusp diff)
			      (* 10 diff)
			      0)
			  60)))
      
;;      (warn "Group chance for ~a (~a) at depth ~a is ~a%" id mon-depth lvl-depth chance)
      
      (if (plusp chance)
	  (< (random 100) chance)
	  nil)))

;; seems to be original depth + 4 which is the basis for when groups appear
(defun van-novice-appears-in-group? (level mon)

  (when (typep mon 'active-monster)
    (setq mon (amon.kind mon)))

  (unless (typep mon 'monster-kind)
    (error "Unknown object ~s given to grouping-function, should be a monster."
	   mon))
  
  (when (typep mon 'unique-monster)
    (error "A unique-monster ~s should not have a grouping-function."
	   (get-id mon)))

  (let ((mon-depth (monster.power-lvl mon)) ;; a bit more tricky than vanilla, but gets increasingly worse
	(lvl-depth (level.depth level)))
    (van-group-chance (get-id mon)
		      mon-depth
		      lvl-depth
		      )))


(defvar *primitive-attacker-ai* (make-instance 'primitive-melee-attacker))

;; we override to add our own stuff
(defmethod produce-active-monster ((variant vanilla-variant) mon-type)
  
  (let ((amon (call-next-method variant mon-type)))

    (unless amon
      (return-from produce-active-monster amon))

    (let ((power (min-cap (get-power-lvl amon) 1))
	  (mon-kind (amon.kind amon)))


      (setf (amon.strategies amon) (list *primitive-attacker-ai*))
    
      (flet ((install-attribute (&rest args)
	       (let ((attr (apply #'make-creature-attribute args)))
		 (unless (is-legal-effect? variant (attr.key attr))
		   (warn "The attribute ~s does not seem legal" attr))
		 (add-creature-attribute amon attr))))
      
	(install-attribute "stun" '<stun> :type :temporary ;; stun has special code
			   :value 0 :default-value 0 :value-type 'integer
			   :update-fun #'%modify-leveled-effect
			   :desc "number, stun-power")
            
	(install-attribute "hasted" '<hasted> :type :temporary
			   :value nil :default-value nil
			   :update-fun #'%modify-boolean-effect
			   :desc "boolean, in vanilla hasted means +10")
      
	(install-attribute "slowed" '<slowed> :type :temporary
			   :value nil :default-value nil
			   :update-fun #'%modify-boolean-effect
			   :desc "boolean, in vanilla slowed means -10")
      
	(install-attribute "sleeping" '<sleeping> :type :temporary
			   :value 0 :default-value 0 :value-type 'integer
			   :update-fun #'%modify-leveled-effect
			   :desc "integer, how sound asleep")

	(install-attribute "confusion" '<confusion> :type :temporary
			   :value 0 :default-value 0 :value-type 'integer
			   :update-fun #'%modify-leveled-effect
			   :desc "integer, how confused")

	(install-attribute "fear" '<fear> :type :temporary
			   :value 0 :default-value 0 :value-type 'integer
			   :update-fun #'%modify-boolean-effect
			   :desc "integer, how afraid")
      
	)

      ;; time to calculate mana for this creature
      (let ((sum 0)
	    (max nil)
	    (dice 2))
      
	(when (is-smart? amon)
	  (setf dice 4))

	(when (find '<max-hitpoints> (monster.abilities mon-kind))
	  (setf max t))
      
	(dolist (i (cdr (monster.sp-abilities mon-kind)))
	  (when (or (typep i 'spell-spab) (typep i 'summon-spab))
	    (incf sum (* power (if max dice (roll-dice 1 dice))))))
      
	(setf (get-creature-mana amon) sum))


      ;; do we need a backpack?
      (dolist (i (cdr (monster.sp-abilities mon-kind)))
	(when (typep i 'ranged-spab)
	  (ensure-monster-has-backpack! amon)
	  (let ((inv (aobj.contains (get-creature-inventory amon)))
		(obj (produce-active-object variant (spab.missile i))))
	    (assert obj)
	    (setf (aobj.number obj) (+ 4 (roll-dice (1+ (int-/ power 10)) 4)))
	    #+debug-ai
	    (warn "Made ~s arrows for ~s" (aobj.number obj) (monster.name amon))
	    (item-table-add! inv obj))))
    
      ;;#+debug-ai
      ;;(warn "Mana for ~s is ~s" (monster.name amon) (get-creature-mana amon))

      ;; hack 
      (setf (get-creature-mana amon) (* 10 (get-creature-mana amon)))
    
      amon)))

(defun ensure-monster-has-backpack! (creature)
  "Adds a backpack to the monster when it has none."
  (unless (get-creature-inventory creature)
    (let ((backpack (create-aobj-from-id "backpack")))
      (setf (get-creature-inventory creature) backpack)))
  (get-creature-inventory creature))


(defmethod roll-saving-throw ((mon active-monster) attack-power)
  "Returns T if saving throw was made."
  ;;(r_ptr->level > randint((dam - 10) < 1 ? 1 : (dam - 10)) + 10)
  (> (get-power-lvl mon)
     (+ 10 (randint attack-power))))

(defmethod get-damage-potential ((creature active-monster) (spab spab))
  (let ((off (spab.offensive spab)))
    (cond ((positive-integer? off))
	  ((functionp off) (funcall off creature))
	  ;;((functionp (spab.damage spab)) (funcall (spab.damage spab) (current-hp creature)))
	  (t 0))))

(defmethod get-defensive-quality ((creature active-monster) (spab spab))
  (let ((off (spab.defensive spab)))
    (cond ((positive-integer? off))
	  ((functionp off) (funcall off creature))
	  (t 0))))

(defmethod get-spab-cost ((creature active-monster) (spab spab))
  (let ((off (spab.mana-cost spab)))
    (cond ((positive-integer? off) off)
	  ((functionp off) (funcall off creature))
	  (t 0))))


(defmethod get-damage-potential ((creature active-monster) (tactic ranged-spab))
  ;;(warn "~s Items ~s" (carries-object? creature "arrow") (aobj.contains (get-creature-inventory creature)))
  ;; check distance to target
  (let ((target *player*)) ;; allow orher targets later
    (if (and (<= (spab.range tactic)
		 (distance (location-x creature) (location-y creature)
			   (location-x target) (location-y target)))
	     (carries-object? creature (spab.missile tactic)))
	(roll-dice (spab.power tactic) 6)
	-1))) ;; can't fire

(defmethod get-damage-potential ((creature active-monster) (tactic spell-spab))

  (unless (>= (get-creature-mana creature) (spab.mana-cost tactic))
    (return-from get-damage-potential -1))

  (let* ((off (spab.offensive tactic))
	 (val (cond ((positive-integer? off) off)
		    ((functionp off) (funcall off creature))
		    (t -1))))
    ;;(warn "val is ~s" val)
    (when (non-negative-integer? val)
      (return-from get-damage-potential val))
    
    (let ((dmg (spab.damage tactic)))
      (cond ((positive-integer? dmg) dmg)
	    ((functionp dmg) (funcall dmg (current-hp creature)))
	    (t 0))) ;; no effect of it
    ))



(defmethod get-damage-potential ((creature active-monster) (tactic breath-spab))

  ;; when it can breath
  (let ((dmg (spab.damage tactic)))
    ;;(warn "creature has ~s dmg ~s hp" dmg (current-hp creature))
    (cond ((positive-integer? dmg) dmg)
	  ((functionp dmg) (funcall dmg (current-hp creature)))
	  (t
	   (warn "Don't know how to treat damage ~s for ~s" dmg tactic)
	   0))))


(defmethod get-tactical-bid (strategy (creature active-monster) (tactic spab) factor)
  (declare (ignore strategy))
  (setf (factor.offensive factor) (get-damage-potential creature tactic)
	(factor.defensive factor) (get-defensive-quality creature tactic)
	(factor.cost factor)      (get-spab-cost creature tactic)) 
  
  t)

(defun is-smart? (creature)
  "Checks if a creature is smart."
  (find '<smart> (monster.abilities (amon.kind creature))))

(defmethod process-single-monster! ((variant vanilla-variant) dungeon player mon)
  (let ((retval (call-next-method)))

    ;; do we bleed?
    (when (< (current-hp mon) (int-/ (maximum-hp mon) 2))
      (let* ((coord (cave-coord dungeon (location-x mon) (location-y mon)))
	     (fl-type (floor.id (coord.floor coord))))

	(cond ((equal fl-type "room-floor")
	       (setf (coord-floor coord) "1-blood-room-floor"))
	      ((equal fl-type "normal-floor")
	       (setf (coord-floor coord) "1-blood-normal-floor")))
	))
    
    retval))
