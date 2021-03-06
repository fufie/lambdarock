;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/player.lisp - code that alters the player object
Copyright (c) 2002-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

(defmethod is-spellcaster? (obj)
  (declare (ignore obj))
  nil)

(defmethod is-spellcaster? ((obj player))
  (is-spellcaster? (player.class obj)))

(defmethod is-spellcaster? ((obj spellcasting-class))
  t)

(defmethod produce-character-class ((variant vanilla-variant) id name &key spells magic-abilities &allow-other-keys)
  ;; we only do stuff if we get magic-abiltiies info, otherwise we assume he is no spell-caster

  (unless (consp magic-abilities)
    (return-from produce-character-class (call-next-method)))
  
  ;; if we have magic abilities:

  (let ((class-obj (make-instance 'spellcasting-class :id id :name name)))
    
    (setf (class.learnt-spells class-obj) (make-array 10 :fill-pointer 0 :initial-element nil))
    
    ;; handle basic magic-info
    (destructuring-bind (&key spell-stat spells-at-level max-armour-weight) magic-abilities
      (when (and spell-stat (symbolp spell-stat))
	(setf (class.spell-stat class-obj) spell-stat))
      (when (and spells-at-level (integerp spells-at-level) (plusp spells-at-level))
	(setf (class.spells-at-level class-obj) spells-at-level))
      (when (and max-armour-weight (integerp max-armour-weight))
	(setf (class.max-armour-weight class-obj) max-armour-weight))
      
      )
    
    ;; handle spells
    (when (consp spells)
      (let ((collected-spells '()))
	(dolist (spell spells)
	  (destructuring-bind (&key id level mana fail xp) spell
	    (let ((spell-obj (make-instance 'spell-classdata)))
	      (if (and id (verify-id id))
		  (setf (spell.id spell-obj) id)
		  (error "Unable to understand spell-info for ~s for class ~s" spell id))
	      (when (and level (integerp level) (plusp level))
		(setf (spell.level spell-obj) level))
	      (when (and mana (integerp mana) (plusp mana))
		(setf (spell.mana spell-obj) mana))
	      (when (and fail (integerp fail) (<= 0 fail))
		(setf (spell.failure spell-obj) fail))
	      (when (and xp (integerp xp) (<= 0 xp))
		(setf (spell.xp spell-obj) xp))
	      
	      ;; check if something matches the def
	      (unless (gethash id (variant.spells variant))
		(warn "Can't find reference to spell-id ~s for class ~s" id name))
	      
	      (push spell-obj collected-spells))))
	
	(assert (plusp (length collected-spells)))
	(let ((spell-array (make-array (length collected-spells))))
	  (loop for i from 0
		for spell in (nreverse collected-spells)
		do
		(setf (aref spell-array i) spell))
	  (setf (class.spells class-obj) spell-array))))
    
    class-obj))

;; this belongs in variant
(defmethod get-class-tile ((variant vanilla-variant) player)
  
  (let ((race (player.race player))
	(class (player.class player))
	(row 0)
	(col 0))

    (setf row (ecase (race.symbol race)
		('<human> 0)
		('<half-elf> 1)
		('<elf> 2)
		('<hobbit> 3)
		('<dunedan> 4)
		('<dwarf> 5)
		('<half-orc> 6)
		('<half-troll> 7)
		('<gnome> 8)
		('<high-elf> 9)
		('<kobold> 10)
		))
    (setf col (ecase (class.symbol class)
		;; no 0
		('<mage> 1)
		;; no 2
		('<paladin> 3)
		('<priest> 4)
		('<ranger> 5)
		('<rogue> 6)
		('<warrior> 7)
		))

    ;; width is 8
    (values +tilefile-classes+ (+ (* 8 row) col))))

(defmethod get-character-picture ((variant vanilla-variant) (player player))

  (let ((race-sym   (race.symbol   (player.race player)))
	(class-sym  (class.symbol  (player.class player)))
	(gender-sym (gender.symbol (player.gender player))))

    (cond  ((and (eq race-sym '<human>) (eq gender-sym '<male>))
	    "people/male-human-warrior.png")
	   ((and (eq race-sym '<human>) (eq gender-sym '<female>))
	    "people/female-human-bard.png")
	   
	   ((and (eq race-sym '<gnome>) (eq gender-sym '<male>))
	    "people/male-gnome-rogue.png")
	   ((and (eq race-sym '<gnome>) (eq gender-sym '<female>))
	    "people/female-gnome-bard.png")
	   
	   ((and (eq race-sym '<dwarf>) (eq gender-sym '<male>))
	    "people/male-dwarf-warrior.png")
	   ((and (eq race-sym '<dwarf>) (eq gender-sym '<female>))
	    "people/female-dwarf-warrior.png")
	   
	   ((and (eq race-sym '<elf>) (eq gender-sym '<male>))
	    "people/male-elf-warrior.png")
	   ((and (eq race-sym '<elf>) (eq gender-sym '<female>))
	    "people/female-elf-rogue.png")
	   
	   ((and (eq race-sym '<half-elf>) (eq gender-sym '<male>))
	    "people/male-halfelf-ranger.png")
	   ((and (eq race-sym '<half-elf>) (eq gender-sym '<female>))
	    "people/female-halfelf-ranger.png")
	   
	   ((and (eq race-sym '<hobbit>) (eq gender-sym '<male>))
	    "people/male-hobbit-rogue.png")
	   ((and (eq race-sym '<hobbit>) (eq gender-sym '<female>))
	    "people/female-hobbit-bard.png")
	   
	   ((and (eq race-sym '<high-elf>) (eq gender-sym '<male>))
	    "people/male-highelf-mage.png")
	   ((and (eq race-sym '<high-elf>) (eq gender-sym '<female>))
	    "people/female-highelf-ranger.png")

	   ;; fix these later
	   ((and (eq race-sym '<dunedan>) (eq gender-sym '<male>))
	    "people/male-human-warrior.png")
	   ((and (eq race-sym '<dunedan>) (eq gender-sym '<female>))
	    "people/female-human-bard.png")
	   
	   ((and (eq race-sym '<half-orc>) (eq gender-sym '<male>))
	    "people/male-halforc-ranger.png")
	   ((and (eq race-sym '<half-orc>) (eq gender-sym '<female>))
	    "people/female-halforc-rogue.png")
	   
	   ((and (eq race-sym '<half-troll>) (eq gender-sym '<male>))
	    "people/male-halftroll-warrior.png")
	   ((and (eq race-sym '<half-troll>) (eq gender-sym '<female>))
	    "people/male-halftroll-warrior.png")

	   (t
	    (warn "Unable to find suitable picture for ~s ~s ~s, falls back to a male gnome."
		 gender-sym race-sym class-sym)
	    ;; default
	    "people/male-gnome-rogue.png"))))

  
(defmethod get-missile-weapon ((crt player))
  (let ((the-eq (player.equipment crt)))
    (check-type the-eq item-table)
    
    (item-table-find the-eq 'eq.bow)))

(defmethod get-light-source ((crt player))
  (let ((the-eq (player.equipment crt)))
    (check-type the-eq item-table)
    
    (item-table-find the-eq 'eq.light)))

(defmethod get-melee-weapon ((crt player))
  (let ((the-eq (player.equipment crt)))
    (check-type the-eq item-table)
    
    (item-table-find the-eq 'eq.weapon)))


(defun %modify-boolean-effect (creature state &key add subtract new-value)
  "Sets the value of a temorary attribute/effect.  Only supports boolean ones."

  (let* ((attr (gethash state (slot-value creature 'temp-attributes)))
	 (old-value (attr.value attr))
	 (new-duration (attr.duration attr))
	 (noticed nil))
    
    (check-type attr temp-creature-attribute)

    (when add
      (cond ((numberp add)
	     (incf new-duration add))
	    (t
	     (warn "Don't know how to handle :add ~s for state ~s"
		   add state))))

    (when subtract
      (cond ((numberp subtract)
	     (decf new-duration subtract))
	    ((eq subtract '<half>)
	     (setf new-duration (int-/ new-duration 2)))
	    (t
	     (warn "Don't know how to handle :subtract ~s for state ~s"
		   subtract state))))

    (when new-value
      (cond ((eq new-value t)
	     (incf new-duration (+ 25 (randint 25))))
	    ((or (eq new-value nil)
		 (and (integerp new-value) (= new-value 0)))
	     (setf new-duration 0))
	    ((integerp new-value)
	     (setf new-duration new-value))
	    (t
	     (error "Weird value given to state ~s :new-value ~s"
		    state new-value))))

    (when (minusp new-duration) ;; never negative
      (setf new-duration 0))

;;    (warn "going berserk [~a,~a] -> [~a,~a]" old-value (attr.duration attr) value new-duration)
    
    (setf (attr.duration attr) new-duration)
    
    (cond ((plusp new-duration)
	   (setf (attr.value attr) t)
	   (when (eq nil old-value)
	     (setf noticed t)
	     (when (attr.turned-on-msg attr)
	       (print-message! (attr.turned-on-msg attr)))))
	  (t
	   (setf (attr.value attr) nil)
	   (when (eq t old-value)
	     (setf noticed t)
	     (when (attr.turned-off-msg attr)
	       (print-message! (attr.turned-off-msg attr))))))

    (when noticed
      (let ((on-upd (attr.on-update attr)))
	(when (and on-upd (functionp on-upd))
	  (funcall on-upd creature attr))))

    ;; call to handle-stuff ?
    t))


(defun %get-cutlvl (cuts)
  "Returns (lvl colour description init-msg)."
  (cond ((> cuts 1000)
	 `(7 ,+term-l-red+  "Mortal wound" "You have been given a mortal wound."))
	((> cuts 200)
	 `(6 ,+term-red+    "Deep gash   " "You have been given a deep gash."))
	((> cuts 100)
	 `(5 ,+term-red+    "Severe cut  " "You have been given a severe cut."))
	((> cuts 50)
	 `(4 ,+term-orange+ "Nasty cut   " "You have been given a nasty cut."))
	((> cuts 25)
	 `(3 ,+term-orange+ "Bad cut     " "You have been given a bad cut."))
	((> cuts 10)
	 `(2 ,+term-yellow+ "Light cut   " "You have been given a light cut."))
	((plusp cuts)
	 `(1 ,+term-yellow+ "Graze       " "You have been given a graze."))
	(t
	 `(0 ,+term-white+  "            " "You are no longer bleeding."))
	))

(defun %get-stunlvl (stun)
  "Returns (lvl colour description init-msg)."
  (cond ((> stun 100)
	 `(3 ,+term-red+    "Knocked out " "You have been knocked out."))
	((> stun 50)
	 `(2 ,+term-orange+ "Heavy stun  " "You have been heavily stunned."))
	((plusp stun)
	 `(1 ,+term-yellow+ "Stun        " "You have been stunned."))
	(t
	 `(0 ,+term-white+  "            " "You are no longer stunned."))
	))

(defun %modify-leveled-effect (creature state &key add subtract new-value)
  "Sets the value of cuts."

  (let* ((attr (gethash state (slot-value creature 'temp-attributes)))
	 (old-duration (attr.duration attr))
	 (new-duration (attr.duration attr))
	 (noticed nil))
    
    (check-type attr temp-creature-attribute)

;;    (warn "Changing ~s from ~s" state old-duration)

    (when add
      (cond ((numberp add)
	     (incf new-duration add))
	    (t
	     (warn "Don't know how to handle :add ~s for state ~s"
		   add state))))

    (when subtract
      (cond ((numberp subtract)
	     (decf new-duration subtract))
	    (t
	     (warn "Don't know how to handle :subtract ~s for state ~s"
		   subtract state))))

    (when new-value
      (cond ((eq new-value t)
	     (incf new-duration (+ 25 (randint 25))))
	    ((eq new-value nil)
	     (setf new-duration 0))
	    ((integerp new-value)
	     (setf new-duration new-value))
	    (t
	     (error "Weird value given to state ~s :new-value ~s"
		    state new-value))))

;;    (warn "going berserk [~a,~a] -> [~a,~a]" old-value (attr.duration attr) value new-duration)
    
    (cond ((minusp new-duration) ;; never negative
	   (setf new-duration 0))
	  ((> new-duration 10000)
	   (setf new-duration 10000)))

;;    (warn "Changed ~s from ~s to ~s" state old-duration new-duration)

    ;; to print various messages if we're player
    (when (typep creature 'player) ;; ugly hack
      (let* ((lvl-fun (ecase state
			(<cut> #'%get-cutlvl)
			(<stun> #'%get-stunlvl)))
	     (old-cutlvl (funcall lvl-fun old-duration))
	     (new-cutlvl (funcall lvl-fun new-duration))
	     (old-lvl (car old-cutlvl))
	     (new-lvl (car new-cutlvl)))
	
	(cond ((> new-lvl old-lvl)
	       (print-message! (fourth new-cutlvl))
	       (setf noticed t))
	      ((< new-lvl old-lvl)
	       (when (= (first new-cutlvl) 0)
		 (print-message! (fourth new-cutlvl)))
	       (setf noticed t)))

	;;      (warn "Cut-val ~s" (get-attribute-value '<cut> (player.temp-attrs player)))
	(when noticed
	  ;; skip disturb
	  (ask-for-update! creature '[bonuses])
	  
	  (ecase state
	    (<cut> (ask-for-redraw! creature '[cut]))
	    (<stun> (ask-for-redraw! creature '[stun])))
	  
	  ;; skip handle-stuff
	  )))

    (setf (attr.duration attr) new-duration)
    (setf (attr.value attr) new-duration)
      
    noticed))



;; we override to add our own stuff
(defmethod produce-player-object ((variant vanilla-variant))
  (let ((player (call-next-method)))

    (setf (player.skills player) (produce-skills-object variant))
    
    (let ((backpack (create-aobj-from-id "backpack"))
	  (eq-slots (player.equipment player)))
      
      (item-table-add! eq-slots backpack 'eq.backpack)
      (setf (get-creature-inventory player) backpack))

   
    ;; ensure that we have hash-tables in place
    (unless (hash-table-p (player.calc-attrs player))
      (setf (player.calc-attrs player) (make-hash-table :test #'eq)))
    (unless (hash-table-p (player.temp-attrs player))
      (setf (player.temp-attrs player) (make-hash-table :test #'eq)))

    (flet ((install-attribute (&rest args)
	     (let ((attr (apply #'make-creature-attribute args)))
	       (unless (is-legal-effect? variant (attr.key attr))
		 (warn "The attribute ~s does not seem legal" attr))
	     (add-creature-attribute player attr)))
	   )
					      
      
      ;; add attributes to the player-object
      (install-attribute "slow digest" '<slow-digest> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
			 :desc "number, specifies how slow digestion is, 0 is normal.")
      
      (install-attribute "feather fall" '<feather-fall> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, are we falling like a feather?")
      
      (install-attribute "glowing" '<glowing> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
 			 :desc "number, specifies radius for player-glow.")
      
      (install-attribute "regenerate" '<regenerate> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
			 :desc "number, specifies regeneration-speed.")
      
      (install-attribute "telepathy" '<telepathy> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
			 :desc "number, specifies radius for telepathy.")
      
      (install-attribute "see invisible" '<see-invisible> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
			 :desc "number, specifies radius.")
      
      (install-attribute "infravision" '<infravision> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
			 :desc "number, specifies radius.")
      
      (install-attribute "free action" '<free-action> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is action free?")
      
      (install-attribute "hold life" '<hold-life> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, has he got a good grasp on his life-force?")
      
      (install-attribute "earthquake" '<earthquake> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, do the blows cause earthquakes?")
      
      (install-attribute "aggravate" '<aggravates> :type :calculated
			 :value 0 :default-value 0 :value-type 'integer
			 :desc "number, specifies radius for aggravation.")
      
      (install-attribute "random teleporting" '<random-teleport> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is the player being randomly teleported?")
      
      (install-attribute "drain xp" '<drain-xp> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is the player being drained of xp?")
      
      (install-attribute "blessed blade" '<blessed-blade> :type :calculated
			 :value nil :default-value nil
			 :desc "boolean, is the player wielding a blessed blade?")
      
      ;; then those attributes that are "temporary" in nature

      (install-attribute "cut" '<cut> :type :temporary ;; cut has special code
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "number, size of cuts")

      (install-attribute "stun" '<stun> :type :temporary ;; stun has special code
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "number, stun-power")

      (install-attribute "see invisible" '<see-invisible> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "Your eyes feel very sensitive!"
			 :turned-off-msg "Your eyes feel less sensitive."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses])
					(ask-for-update! player '[monsters]))
			 :desc "boolean, in vanilla see-inv is on/off")

      (install-attribute "infravision" '<infravision> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "Your eyes begin to tingle!"
			 :turned-off-msg "Your eyes stop tingling."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses])
					(ask-for-update! player '[monsters]))
			 :desc "boolean, in vanilla infravision is on/off")

      
      (install-attribute "hasted" '<hasted> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel yourself moving faster!"
			 :turned-off-msg "You feel yourself slow down."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla hasted means +10")
      
      (install-attribute "slowed" '<slowed> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel yourself moving slower!"
			 :turned-off-msg "You feel yourself speed up."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla slowed means -10")

      (install-attribute "blindness" '<blindness> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are blind!"
			 :turned-off-msg "You can see again."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[forget-view])
					(ask-for-update! player '[update-view])
					(ask-for-update! player '[monsters])
					(ask-for-redraw! player '[map])
					(ask-for-redraw! player '[blind])
					;; skip window
					)			 
			 :desc "boolean, either blinded or not")
      
      (install-attribute "paralysed" '<paralysed> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are paralysed!"
			 :turned-off-msg "You can move again."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-redraw! player '[paralysis]))
			 :desc "boolean, either paralysed or not")

      (install-attribute "confusion" '<confusion> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are confused!"
			 :turned-off-msg "You feel less confused now."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-redraw! player '[confused]))
			 :desc "boolean, either confused or not")

      (install-attribute "fear" '<fear> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are terrified!"
			 :turned-off-msg "You feel bolder now."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-redraw! player '[afraid]))
			 :desc "boolean, either afraid or not")
      
      (install-attribute "hallucinate" '<hallucinate> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel drugged!"
			 :turned-off-msg "You can see clearly again."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-redraw! player '[map]))
			 :desc "boolean, either hallucinating or not")

      (install-attribute "poisoned" '<poisoned> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You are poisoned!"
			 :turned-off-msg "You are no longer poisoned."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-redraw! player '[poisoned]))
			 :desc "boolean, in vanilla you're poisoned or not poisoned")

      (install-attribute "protected from evil" '<prot-from-evil> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel safe from evil!"
			 :turned-off-msg "You no longer feel safe from evil."
			 :update-fun #'%modify-boolean-effect
			 :desc "boolean, in vanilla you're protected or not protected")
      
      (install-attribute "shielded" '<shielded> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "A mystic shield forms around your body!"
			 :turned-off-msg "Your mystic shield crumbles away."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, you're either under a shield-spell or not in vanilla")

      (install-attribute "invulnerability" '<invulnerable> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel invulnerable!"
			 :turned-off-msg "You feel vulnerable once more."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, either invulnerable or not")

      (install-attribute "heroic" '<heroic> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel like a hero!"
			 :turned-off-msg "The heroism wears off."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, hero or not hero")

      (install-attribute "berserk" '<berserk> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel like a killing machine!"
			 :turned-off-msg "You feel less berserk."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, is he berserk?")

      (install-attribute "blessed" '<blessed> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel righteous!"
			 :turned-off-msg "The prayer has expired."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla you're eiether blessed or not")

      (install-attribute "resist acid" '<resist-acid> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to acid!"
			 :turned-off-msg "You feel less resistant to acid."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "resist elec" '<resist-elec> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to electricity!"
			 :turned-off-msg "You feel less resistant to electricity."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "resist fire" '<resist-fire> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to fire!"
			 :turned-off-msg "You feel less resistant to fire."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla temp-resists are on/off")
      
      (install-attribute "resist cold" '<resist-cold> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to cold!"
			 :turned-off-msg "You feel less resistant to cold."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "resist poison" '<resist-poison> :type :temporary
			 :value nil :default-value nil
			 :turned-on-msg "You feel resistant to poison!"
			 :turned-off-msg "You feel less resistant to poison."
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(declare (ignore attr))
					(ask-for-update! player '[bonuses]))
			 :desc "boolean, in vanilla temp-resists are on/off")

      (install-attribute "recalling" '<recalling> :type :temporary
			 :value nil :default-value nil
			 :update-fun #'%modify-boolean-effect
			 :on-update #'(lambda (player attr)
					(when (eq (attr.value attr) nil)
					  (cond ((plusp (player.depth player))
						 (setf (player.depth player) 0
						       (player.leaving? player) :wor)
						 (print-message! "You feel yourself yanked upwards!"))
						(t
						 (setf (player.depth player) (player.max-depth player)
						       (player.leaving? player) :wor)
						 (print-message! "You feel yourself yanked downwards!")))
					  t))
			 
			 :desc "boolean, is recalling or not")


    
      player)))


(defun %van-fill-resists (variant resist-table source-resists value)
  (when source-resists 
    (dolist (x (variant.elements variant))
      (when (bit-flag-set? source-resists (element.bit-flag x))
	(incf (aref resist-table (element.number x)) value)))))



(defun %van-update-resistance (variant resist-array element change)
  (incf (aref resist-array (get-element-number variant element)) change))


(defmethod reset-player-object! ((variant vanilla-variant) (player player))
  (call-next-method)

  (van/reset-skills! variant (player.skills player) 0)

  (flet ((%reset-plattrs (table)
	   (loop for attr being the hash-values of table
		 do
		 (setf (attr.value attr) (attr.default-value attr)))))
    
    (%reset-plattrs (player.calc-attrs player))
;;    (%reset-plattrs (player.temp-attrs player))

    (let ((table (get-resistance-table player)))
      (dotimes (i (length table))
	(setf (aref table i) 0)))
    
    t))

(defmethod calculate-abilities! ((variant vanilla-variant) (player player) (cls character-class))

  ;; do the general stuff first:
  (call-next-method)

  ;; add skills
  (dolist (i (variant.skill-translations variant))
    (van/add-to-a-skill! (cdr i)
			 (player.skills player)
			 (player.power-lvl player)
			 (class.skills cls)))
  
  ;; add resists
  ;; currently resists on race is just an integer
  (let ((resist-array (get-resistance-table player)))
    (%van-fill-resists variant resist-array (get-resists cls) +element-calculated-resistance+)

    ;; hack, add fear resist for warriors
    (when (and (equal (get-id cls) "warrior") (>= (player.power-lvl player) 30))
      (%van-update-resistance variant resist-array '<fear> +element-calculated-resistance+)))

  (bit-flag-add! (get-stat-sustains player) (get-stat-sustains cls))
  )
  

(defmethod calculate-abilities! ((variant vanilla-variant) (player player) (race character-race))

  ;; do the general stuff first:
  (call-next-method)

  ;; fix skill-values
  (dolist (i (variant.skill-translations variant))
      (van/add-to-a-skill! (cdr i)
			   (player.skills player)
			   (player.power-lvl player)
			   (race.skills race)))

  ;; add resists
  ;; currently resists on race is just an integer
  (let ((resist-array (get-resistance-table player)))
    (%van-fill-resists variant resist-array (get-resists race) +element-calculated-resistance+))
  

  (bit-flag-add! (get-stat-sustains player) (get-stat-sustains race))

  (let ((calc-attrs (player.calc-attrs player))
	(race-ab (race.abilities race)))
    
    (dolist (i race-ab)
;;	(warn "checking ~a" i)
	(cond ((consp i)
	       (case (car i)
		 (<infravision>
		  (when (integerp (second i))
		    (modify-attribute! '<infravision> calc-attrs (second i))))
		 (otherwise
		  (warn "Unhandled racial ability ~a" (car i)))))
	      ((symbolp i)
	       (case i
		 (<see-invisible>
		  (modify-attribute! '<see-invisible> calc-attrs +max-sight+)) ;; radius
		 (<free-action>
		  (modify-attribute! '<free-action> calc-attrs t)) ;; boolean
		 (otherwise 
		  (warn "Unhandled racial ability ~a" i))))
	      (t
	       (warn "Unhandled racial ability ~a" i)))
	)
    t))

(defmethod calculate-abilities! ((variant vanilla-variant) (player player) (items items-worn))
  ;; get weight and ac first from parent object
  (call-next-method)

  
  (let ((actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player))
	(resist-array (get-resistance-table player))
	(calc-attrs (player.calc-attrs player))
	)

    (loop for obj across (items.objs items)
	  do
	  (when obj
	    ;; time to get resists
	    (%van-fill-resists variant resist-array (get-resists obj) +element-calculated-resistance+)
	    (%van-fill-resists variant resist-array (get-immunities obj) +element-immunity+)
	    
	      ;; hack, skip weapons
	    (unless (typep obj 'active-object/weapon)
	      ;; to hit
	      (incf (get-tohit-modifier actual-abs) (get-tohit-modifier obj))
	      (when (is-object-known? obj)
		(incf (get-tohit-modifier perc-abs) (get-tohit-modifier obj)))
	      
	      ;; to damage
	      (incf (get-damage-modifier actual-abs) (get-damage-modifier obj))
	      (when (is-object-known? obj)
		(incf (get-damage-modifier perc-abs) (get-damage-modifier obj)))
	      
	      (bit-flag-add! (get-stat-sustains player) (get-stat-sustains obj))
	      
	      (dolist (i (object.abilities (aobj.kind obj)))
		(case i
		  (<feather-fall>
		   (modify-attribute! '<feather-fall> calc-attrs t)) ;; boolean
		  (<free-action>
		   (modify-attribute! '<free-action> calc-attrs t)) ;; boolean
		  (<see-invisible>
		   (modify-attribute! '<see-invisible> calc-attrs +max-sight+))
		  (<random-teleport>
		   (modify-attribute! '<random-teleport> calc-attrs t)) ;; boolean
		  (<slow-digestion>
		   (modify-attribute! '<slow-digest> calc-attrs 10))
		  (<aggravate>
		   (modify-attribute! '<aggravates> calc-attrs 10))
		  (t
		   (warn "Unhandled item-ability ~s for item ~s" i obj))))
	      
	      )))
    ))


(defmethod handle-player-updates! ((variant vanilla-variant) (player player) (old old-player-info))
  
  (let ((calc-attrs (player.calc-attrs player))
	(temp-attrs (player.temp-attrs player))
	(actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player))
	(resist-array (get-resistance-table player))
	(skills (player.skills player))
	)

    ;;; == Go through temporary effects
    
    (let ((stun-value (get-attribute-value '<stun> temp-attrs)))
      (cond ((> stun-value 50)
	     (incf (get-tohit-modifier actual-abs) -20)
	     (incf (get-tohit-modifier perc-abs)   -20)
	     (incf (get-damage-modifier actual-abs) -20)
	     (incf (get-damage-modifier perc-abs)   -20))
	    ((plusp stun-value)
	     (incf (get-tohit-modifier actual-abs) -5)
	     (incf (get-tohit-modifier perc-abs)   -5)
	     (incf (get-damage-modifier actual-abs) -5)
	     (incf (get-damage-modifier perc-abs)   -5))
	    ((= 0 stun-value))
	    (t
	     (error "Stun-value has odd value ~s" stun-value))))

    (when (get-attribute-value '<invulnerable> temp-attrs)
      (incf (get-armour-modifier actual-abs) +100)
      (incf (get-armour-modifier perc-abs)   +100))


    (when (get-attribute-value '<blessed> temp-attrs)
      (incf (get-armour-modifier actual-abs) +5)
      (incf (get-armour-modifier perc-abs)   +5)
      (incf (get-tohit-modifier actual-abs) +10)
      (incf (get-tohit-modifier perc-abs)   +10)
      )

    (when (get-attribute-value '<shielded> temp-attrs)
      (incf (get-armour-modifier actual-abs) +50)
      (incf (get-armour-modifier perc-abs)   +50)
      )

    (when (get-attribute-value '<heroic> temp-attrs)
      (incf (get-tohit-modifier actual-abs) +12)
      (incf (get-tohit-modifier perc-abs)   +12)

      ;; immune to fear
      (%van-update-resistance variant resist-array '<fear> +element-temporary-resistance+))


    
    (when (get-attribute-value '<berserk> temp-attrs)
      (incf (get-armour-modifier actual-abs)     -10)
      (incf (get-armour-modifier perc-abs)       -10)
      (incf (get-tohit-modifier actual-abs) +24)
      (incf (get-tohit-modifier perc-abs)   +24)
      ;; immune to fear
      (%van-update-resistance variant resist-array '<fear> +element-temporary-resistance+))


    (when (get-attribute-value '<hasted> temp-attrs)
      (incf (player.speed player) +10))
    
    (when (get-attribute-value '<slowed> temp-attrs)
      (incf (player.speed player) -10))

    ;; add update of see-inv from current facts
    (let ((temp-see (get-attribute-value '<see-invisible> temp-attrs))
	  (calc-see (get-attribute-value '<see-invisible> calc-attrs)))
      ;; set it to the largest of the two
      ;; hack for vanilla
      (when temp-see
	(setf calc-see +max-sight+)) 
      (setf (player.see-invisible player) calc-see))


    ;; add update of infravision from current facts
    (let ((temp-infra (get-attribute-value '<infravision> temp-attrs))
	  (calc-infra (get-attribute-value '<infravision> calc-attrs)))
      ;; set it to the largest of the two
      (when temp-infra (incf calc-infra)) ;; temporary infra is one better
      (setf (player.infravision player) calc-infra))

    
    ;; go through all temporary resists

    (when (get-attribute-value '<resist-fire> temp-attrs)
      (%van-update-resistance variant resist-array '<fire> +element-temporary-resistance+))
    (when (get-attribute-value '<resist-cold> temp-attrs)
      (%van-update-resistance variant resist-array '<cold> +element-temporary-resistance+))
    (when (get-attribute-value '<resist-elec> temp-attrs)
      (%van-update-resistance variant resist-array '<electricity> +element-temporary-resistance+))
    (when (get-attribute-value '<resist-acid> temp-attrs)
      (%van-update-resistance variant resist-array '<acid> +element-temporary-resistance+))
    (when (get-attribute-value '<resist-poison> temp-attrs)
      (%van-update-resistance variant resist-array '<poison> +element-temporary-resistance+))
    

    ;;; do full weight analysis

    (when (>= (player.satiation player) +food-max+) ;; move slower when gorged
      (decf (player.speed player) 10))

    ;;; add stats bonuses


    ;;; do skills

    (let ((hold (get-stat-info-value variant player '<str> :wpn-limit))) ;; what can he hold?
      ;; analyse bow
      (let ((miss-weapon (get-missile-weapon player))
	    (heavy-bow nil))

	(when miss-weapon
	  (check-type miss-weapon active-object/missile-weapon)
	  (let* ((weight (object.weight miss-weapon))
                 (weight-constant (int-/ weight 10))
                 )
	    (when (< hold weight-constant) ;; heavy
	      (incf (get-tohit-modifier actual-abs) (* 2 (- hold weight-constant)))
	      (incf (get-tohit-modifier perc-abs)   (* 2 (- hold weight-constant)))
	      (setf heavy-bow t))
	    
	    ;; skip calculations
	    ))
	
	;; do check vs old info
	(unless (eq heavy-bow (old.heavy-bow old))
	  (cond (heavy-bow
		 (print-message! "You have trouble wielding such a heavy bow."))
		(miss-weapon
		 (print-message! "You have no trouble wielding your bow."))
		(t
		 (print-message! "You feel relieved to put down your heavy bow."))))
	(setf (old.heavy-bow old) heavy-bow)
	)
      
      ;;; analyse weapon
      (let ((weapon (get-melee-weapon player))
            (too-heavy nil)
            (icky-weapon nil))

        (when weapon
          (check-type weapon active-object/melee-weapon)
          (let* ((weight (object.weight weapon))
                 (weight-constant (int-/ weight 10))
                 )

          (when (< hold weight-constant) ;; heavy
            (incf (get-tohit-modifier actual-abs) (* 2 (- hold weight-constant)))
            (incf (get-tohit-modifier perc-abs)   (* 2 (- hold weight-constant)))
            (setf too-heavy t))

          ;; skip blows calculation

          ;; priest have problems?
          (when (eq (class.symbol (player.class player)) '<priest>)
            ;; skip blessed-blade check
            (unless (typep weapon 'active-object/hafted) ;; only hafted weapons ok
              (incf (get-tohit-modifier actual-abs) -2)
              (incf (get-tohit-modifier perc-abs)   -2)
              (incf (get-damage-modifier actual-abs) -2)
              (incf (get-damage-modifier perc-abs)   -2)

              (setf icky-weapon t)))
          ))

        
        ;; do check vs old info
        (unless (eq too-heavy (old.heavy-weapon old))
          (cond (too-heavy
                 (print-message! "You have trouble wielding such a heavy weapon."))
                (weapon
                 (print-message! "You have no trouble wielding your weapon."))
                (t
                 (print-message! "You feel relieved to put down your heavy weapon."))))
        (setf (old.heavy-weapon old) too-heavy)

        (unless (eq icky-weapon (old.icky-weapon old))
          (cond (icky-weapon
                 (print-message! "You do not feel comfortable with your weapon."))
                (weapon
                 (print-message! "You feel comfortable with your weapon."))
                (t
                 (print-message! "You feel more comfortable after removing your weapon."))))
        (setf (old.icky-weapon old) icky-weapon)
        )
      )

            
        

      

    ;;; [analysis of stats for redraw done by engine]
    ;;; [analysis of speed+armour changes for redraw done by engine]
    

    ;;; check telepathy, see-inv


    ;;; this way to do lookups _sucks_
    
    ;; time to add modifiers from stats
    (let ((stats (variant.stats variant))
	  (active-stats (player.active-stats player)))
	
      (dolist (i stats)
	
	(let* ((cur-val (svref active-stats (stat.number i)))
	       ;;(cur-row (get-stat-row (stat.fields i) cur-val))
	       )
	  (case (stat.symbol i)
	    (<dex> (let ((to-hit (get-stat-info i cur-val :hit-modifier))
			 (to-ac (get-stat-info i cur-val :ac-modifier))
			 (disarm (get-stat-info i cur-val :disarm)))
		     ;; + to-hit
		     (incf (get-tohit-modifier actual-abs) to-hit)
		     (incf (get-tohit-modifier perc-abs)   to-hit)
		     ;; + to ac
		     (incf (get-armour-modifier actual-abs) to-ac)
		     (incf (get-armour-modifier perc-abs)   to-ac)
		     ;; better at disarming
		     (incf (skills.disarming skills) disarm)
		     
		     ))
	    
	    (<str> (let ((to-hit (get-stat-info i cur-val :hit-modifier))
			 (to-dmg (get-stat-info i cur-val :dam-modifier)))
		     (incf (get-tohit-modifier actual-abs) to-hit)
		     (incf (get-tohit-modifier perc-abs)   to-hit)
		     (incf (get-damage-modifier actual-abs) to-dmg)
		     (incf (get-damage-modifier perc-abs)   to-dmg)
		     ;; add digging
		     ))
	    
	    (<int> (let ((disarm (get-stat-info i cur-val :disarm))
			 (device (get-stat-info i cur-val :mag-dev)))
		     ;; better at disarming
		     (incf (skills.disarming skills) disarm)
		     ;; better at devices
		     (incf (skills.device skills) device)
		     ))
	    
	    (<wis> (let ((save (get-stat-info i cur-val :saving-throw)))
		     ;; better at saves
		     (incf (skills.saving-throw skills) save)
		     ))
	    ))
	))

    ;; somehow vanilla-people help players with bonuses
    (incf (skills.stealth skills))

    ;; skip digging modifier
    
    ;; keep stealth within limits
    (cond ((> (skills.stealth skills) 30)
	   (setf (skills.stealth skills) 30))
	  ((minusp (skills.stealth skills))
	   (setf (skills.stealth skills) 0)))

    ;; skip noise
    ;; hold?
    
    (call-next-method)    
    
    t))

 
(defmethod get-creature-state ((player player) state)
  "Very limited so far, but might work for some stuff.  do not rely on it."
  (let* ((temp-attrs (player.temp-attrs player))
	 (attr (gethash state temp-attrs)))

    (unless attr
      (let ((calc-attrs (player.calc-attrs player)))
	(setf attr (gethash state calc-attrs))))

    (if (and attr (typep attr 'creature-attribute))
	(attr.value attr)
	(error "Attribute ~s is not know for player" state))
    ))


;; === hackish
(defvar *known-states* nil)
#||

(defvar *tried-states* nil)

(defmacro modify-creature-state! (creature state &key new-value add subtract)
;;  (warn "State ~s" state)
  (pushnew (if (symbolp state) state
	       (if (and (consp state) (eq (car state) 'quote))
		   (cadr state)
		   state))
	   *tried-states*)
  
  `(modify-creature-state ,creature ,state :new-value ,new-value :add ,add :subtract ,subtract))
||#

;; ===


(defmethod modify-creature-state! (creature state &key add subtract new-value)
  (block modify-creature-state!

    #||
    ;; remove this stuff later
    (unless *known-states*
      (loop for k being the hash-keys of (player.temp-attrs player)
	    do (pushnew k *known-states*))
      (loop for k being the hash-keys of (player.calc-attrs player)
	    do (pushnew k *known-states*)))
    

    ;; move this later
    (let ((known-states *known-states*))
      (unless (find state known-states)
	(warn "Unknown player-state ~s, doing nothing!" state)
	(return-from modify-creature-state! nil)
	))
    ||#

    ;; this is not very good
    (let* ((temp-attrs (slot-value creature 'temp-attributes))
	   (attr (gethash state temp-attrs)))
      
      (cond ((and attr (attr.update-fun attr))
	     (funcall (attr.update-fun attr) creature state :add add
		      :subtract subtract :new-value new-value))
	    (t
	     (warn "Not implemented support for creature-state ~s" state)
	     nil))
      )))

(defmethod gain-power-level! ((variant vanilla-variant) player)

  (ask-for-redraw! player '[study])
  (ask-for-update! player '[mana])
  (ask-for-update! player '[spells])
  (call-next-method variant player))


(defun regenerate-mana! (crt percent)
  ;; clean up later
    
  (let* ((regen-base 1442)
	 (old-mana (current-mana crt))
	 (new-mana (+ (* (maximum-mana crt) percent) regen-base))
	 (max-short 32767)
	 (increase (int-/ new-mana (expt 2 16)))
	 (new-frac (+ (player.fraction-mana crt)
		      (logand new-mana #xffff)))
	 )

    (incf (current-mana crt) increase)

    (when (and (minusp (current-mana crt))
	       (plusp old-mana))
      (setf (current-mana crt) max-short))

    (if (> new-frac #x10000)
	(progn
	  (setf (player.fraction-mana crt) (- new-frac #x10000))
	  (incf (current-mana crt)))
	(setf (player.fraction-mana crt) new-frac))

    (when (>= (current-mana crt)
	      (maximum-mana crt))
      (setf (current-mana crt) (maximum-mana crt)
	    (player.fraction-mana crt) 0))

    (when (/= old-mana (current-mana crt))
;;      (warn "Regenerated..")
      (ask-for-redraw! crt '[mana]))
      
    (current-mana crt)))

(defmethod roll-up-character! ((variant vanilla-variant) (player player))

  (call-next-method)
  (calculate-creature-mana! variant player)
  (setf (current-mana player) (maximum-mana player))

  player)

(defmethod on-pickup-object ((variant vanilla-variant) (player player) (obj active-object))
  (ask-for-update! player '[mana]))

(defmethod on-wear-object ((variant vanilla-variant) (player player) (obj active-object/weapon))
  (format-message! "You are wielding ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-wear-object ((variant vanilla-variant) (player player) (obj active-object/bow))
  (format-message! "You are shooting with ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-wear-object ((variant vanilla-variant) (player player) (obj active-object/light-source))
  (format-message! "Your light source is ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-wear-object ((variant vanilla-variant) (player player) (obj active-object))
  (format-message! "You are wearing ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-take-off-object ((variant vanilla-variant) (player player) (obj active-object/weapon))
  (format-message! "You were wielding ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-take-off-object ((variant vanilla-variant) (player player) (obj active-object/bow))
  (format-message! "You were holding ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-take-off-object ((variant vanilla-variant) (player player) (obj active-object/light-source))
  (format-message! "You were holding ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-take-off-object ((variant vanilla-variant) (player player) (obj active-object))
  (format-message! "You were wearing ~a."
		   (with-output-to-string (s)
		     (write-obj-description variant obj s)))
  (ask-for-update! player '[mana]))

(defmethod on-drop-object ((variant vanilla-variant) (player player) (obj active-object))
  (ask-for-update! player '[mana]))


(defmethod initialise-character-race! ((var-obj vanilla-variant) (race character-race) keyword-args)

  (call-next-method)

  (when-bind (skills (getf keyword-args :skills))
    (setf (race.skills race) (build-skills-obj-from-list var-obj skills)))

  race)

(defmethod initialise-character-class! ((var-obj vanilla-variant) (my-class character-class) keyword-args)

  (call-next-method)

  (when-bind (skills (getf keyword-args :skills))
    (setf (class.skills my-class) (build-skills-obj-from-list var-obj skills)))

  my-class)

(defmethod is-blind? ((variant vanilla-variant) (creature player))
  "Returns T if the creature is blind."
  (let ((temp-attrs (player.temp-attrs creature)))
    (get-attribute-value '<blindness> temp-attrs)))


(defmethod can-melee-attack? ((variant vanilla-variant) (player player) (mon active-monster))
  (if (get-attribute-value '<fear> (player.temp-attrs player))
      (format nil "You're too afraid to attack ~A." (get-creature-desc mon #x00))
      t))

(defmethod handle-turn ((variant vanilla-variant) (player player) (activity (eql :resting)))

  (let ((mode (get-information "rest-mode"))
	(dungeon *dungeon*))
    (move-player! dungeon player 5)

    (when (integerp mode)
      (decf mode)
      (if (plusp mode)
	  (setf (get-information "rest-mode") mode)
	  (setf (get-information "rest-mode") nil
		(get-information "resting") nil)))
    
    (when (eq mode :normal-rest)
      (when (and (= (current-hp player) (maximum-hp player))
		 (= (current-mana player) (maximum-mana player)))
	(setf (get-information "rest-mode") nil
	      (get-information "resting") nil)))

    (when (eq mode :full-rest) ;; everything should be kosher
      (let ((temp-attrs (player.temp-attrs player)))
	(when (and (= (current-hp player) (maximum-hp player))
		   (= (current-mana player) (maximum-mana player))
		   (eq nil (get-attribute-value '<blindness> temp-attrs))
		   (eq nil (get-attribute-value '<confusion> temp-attrs))
		   (= 0 (get-attribute-value '<stun> temp-attrs))
		   (eq nil (get-attribute-value '<paralysed> temp-attrs))
		   (eq nil (get-attribute-value '<fear> temp-attrs))
		   (eq nil (get-attribute-value '<hallucinate> temp-attrs)))
	
	  (setf (get-information "rest-mode") nil
		(get-information "resting") nil))))
      
    t))

;; hack
#-langband-release
(defmethod calculate-abilities! :after ((variant vanilla-variant) (player player) foo)
  (declare (ignore foo))
  (loop for i from 0
	for x across (get-resistance-table player)
	do
	(unless (or (= x 0) (= x +element-calculated-resistance+)
		    (= x +element-calculated-resistance+)
		    (= x #.(+ +element-calculated-resistance+ +element-calculated-resistance+))
		    (= x +element-immunity+))
	  (warn "Illegal player-resist for elm ~s: ~s" i x))))