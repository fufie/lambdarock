;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: player.lisp - code for the character object
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defun is-player? (obj)
  "Is the object a player-object?"
  (typep obj 'player))

(defun player-is-at? (player x y)
  "Checks if the player is at x,y"
  (and (= (location-x player) x)
       (= (location-y player) y)))

(defmethod is-blind? ((variant variant) (creature player))
  nil)

(defmethod is-sleeping? ((variant variant) (creature player))
  nil)

(defmethod is-male? ((creature player))
  (eq (gender.symbol (player.gender creature)) '<male>))

(defmethod is-female? ((creature player))
  (eq (gender.symbol (player.gender creature)) '<female>))


(defun make-old-player-info (variant)
  "creates and returns a freshly init'ed OLD-PLAYER-INFO object."
  (let ((old (make-instance 'old-player-info)))
    (setf (old.stats old) (make-stat-array variant)
	  (old.abilities old) (make-instance 'player-abilities))
    old))


(defun get-attribute-value (key table)
  "Returns the value of the attribute identified by KEY in the TABLE."
  (let ((val (gethash key table)))
    (check-type val creature-attribute)
    (attr.value val)))

(defmethod add-creature-attribute ((player player) attr)
  (ecase (attr.type attr)
    (:calculated (setf (gethash (attr.key attr) (player.calc-attrs player))
		       attr))
    (:temporary (setf (gethash (attr.key attr) (player.temp-attrs player))
		       attr))))

(defun make-creature-attribute (name key
			      &key type desc value (value-type 'boolean) default-value
			      turned-on-msg turned-off-msg
			      update-fun on-update)
  (case type
    (:temporary
     (make-instance 'temp-creature-attribute
		    :name name :key key :type type :desc desc
		    :value value :value-type value-type
		    :default-value default-value
		    :duration 0 :turned-on-msg turned-on-msg
		    :turned-off-msg turned-off-msg
		    :on-update on-update
		    :update-fun update-fun))
    (otherwise
     (make-instance 'creature-attribute
		    :name name :key key :type type :desc desc
		    :value-type value-type
		    :value value :default-value default-value))
    ))


(defmethod (setf creature-alive?) (value (crt player))
  (setf (slot-value crt 'alive?) value)
  (when (eq value nil)
    (setf (player.leaving? crt) :dead)))
    

(defmethod get-xp-value ((crt player))
  (* (player.power-lvl crt) 20))


(defun get-race-name (player)
  "Returns a string with the name of the race."
  (race.name (player.race player)))

(defun get-class-name (player)
  "Returns a string with the name of the class."
  (class.name (player.class player)))

(defun get-gender-name (player)
  "Returns a string with the name of the gender."
  (gender.name (player.gender player)))

(defmethod get-creature-ac ((crt player))
  (let ((actual (player.actual-abilities crt)))
    (+ (get-armour-rating actual)
       (get-armour-modifier actual))
    ))

(defmethod get-weight ((crt player))
  (+ (player.burden crt) (playermisc.weight (player.misc crt))))

(defmethod get-creature-burden ((crt player))
  (player.burden crt))

(defmethod get-creature-energy ((crt player))
  (player.energy crt))

(defmethod (setf get-creature-energy) (val (crt player))
;;  (when (< val (player.energy crt)) (warn "Reducing energy from ~a to ~a" (player.energy crt) val))
  (setf (player.energy crt) val))

(defmethod get-creature-speed ((crt player))
  (player.speed crt))

(defmethod (setf get-creature-speed) (val (crt player))
  (setf (player.speed crt) val))

(defun get-tohit-chance (variant skill the-ac)
  (declare (ignore variant))
  (let* ((ac-factor (int-/ (* 3 the-ac) 4))
	 (calc-chance (int-/ (* 90 (- skill ac-factor)) skill)))
    (if (plusp calc-chance)
	(+ 5 calc-chance)
	5)))


(defun %make-level-array (var-obj)
  (check-type var-obj variant)
  (let ((max-char-level (variant.max-charlevel var-obj)))
    (make-array max-char-level :initial-element 0)))



(defmethod produce-player-object ((variant variant))
  "Creates and returns a PLAYER object."
  (let ((player (make-instance 'player)))
  
    (setf (player.base-stats player)    (make-stat-array variant)
	  (player.cur-statmods player) (make-stat-array variant)
	  (player.modbase-stats player) (make-stat-array variant)
	  (player.active-stats player)  (make-stat-array variant))

    (setf (player.misc player) (make-instance 'misc-player-info) ;; fix to allow variants to override
	  (player.perceived-abilities player) (make-instance 'player-abilities) ;; fix to allow variants to override
	  (player.actual-abilities player) (make-instance 'player-abilities)) ;; fix to allow variants to override
    
    #+langband-extra-checks
    (assert (let ((bstat-table (player.base-stats t-p))
		  (cstat-table (player.cur-statmods t-p))
		  (mstat-table (player.modbase-stats t-p))
		  (astat-table (player.active-stats t-p)))
	      
	      (and (not (eq bstat-table cstat-table))
		   (not (eq bstat-table mstat-table))
		   (not (eq bstat-table astat-table))
		   (not (eq cstat-table mstat-table))
		   (not (eq cstat-table astat-table))
		   (not (eq mstat-table astat-table)))))

    ;; hack to get things moving!
;;    (setf (creature.attributes t-p) (make-instance 'calculated-attributes))
    
    (setf (player.equipment player) (make-equipment-slots variant))
    
    (setf (player.hp-table player) (%make-level-array variant)
	  (player.xp-table player) (%make-level-array variant)
	  )

    
    ;; we want the resist table
    (let ((resist-size (length (variant.elements variant))))
      (setf (get-resistance-table player) (make-array resist-size :initial-element 0))) 

    (setf (get-stat-sustains player) 0)
    
    #||
    (flet ((make-and-assign-backpack! (id)
	     (let ((back-obj (create-aobj-from-id id))
		   (eq-slots (player.equipment t-p)))
	       ;;(warn "eq-slots ~a" eq-slots)
	       (item-table-add! eq-slots back-obj 'eq.backpack)
	       (setf (get-creature-inventory t-p) back-obj))))
      
      (let ((backpack-val (game-parameter-value :initial-backpack)))
	(case backpack-val
	  (:backpack (make-and-assign-backpack! "backpack"))
	  (otherwise
	   (warn "No initial known backpack-setting, assuming \"backpack\"")
	   (make-and-assign-backpack! "backpack")))))

    ;; hack
    ;;    (setf (get-light-radius t-p) 3)
    ||#

    (when (eq nil (slot-value player 'gfx-sym))
      (setf (slot-value player 'gfx-sym) (text-paint-value +term-white+ #\@)))

    (when (eq nil (slot-value player 'text-sym))
      (setf (slot-value player 'text-sym) (text-paint-value +term-white+ #\@)))
    
    player))



(defun add-stat-bonus (base amount)
  "Returns a numeric value with base incremented with amount"
  (declare (type fixnum base amount))
  (let ((retval base))
    (declare (type fixnum retval))
    (if (< amount 0)
	(dotimes (i (abs amount))
	  (cond ((>= retval (+ 18 10))
		 (decf retval 10))
		((> retval 18) ;; hackish
		 (setq retval 18))
		((> retval 3) ;; minimum
		 (decf retval 1))
		(t
		 ;; too low to care
		 )))
	;; positive amount
	(dotimes (i amount)
	  (if (< retval 18)
	      (incf retval)
	      (incf retval 10))))
    retval))
		

(defmethod calculate-creature-hit-points! ((variant variant) (player player))

  t)


(defmethod calculate-creature-light-radius! ((variant variant) (player player))
  (let ((old-val (get-light-radius player))
	(slots (player.equipment player))
	(highest 0))
    (unless slots
      (error "Can't find equipment-slots for player, bad."))
    
    ;; hackish, simplify later
    (flet ((item-iterator (table key obj)
	     (declare (ignore table key))
	     (when obj
	       ;; fix light-radius first
	       (when (> (get-light-radius obj) highest)
		 (setf highest (get-light-radius obj)))
	       )))
      
      (declare (dynamic-extent #'item-iterator))
      (item-table-iterate! slots #'item-iterator))
    
    (when (/= old-val highest)
      (setf (get-light-radius player) highest)
      (ask-for-update! player '[update-view])
      (ask-for-update! player '[monsters])
      t)))



(defmethod get-old-player-info ((variant variant) (player player) &key (reuse-object nil))

  (let ((old (cond ((and reuse-object (typep reuse-object 'old-player-info))
		    reuse-object)
		   (reuse-object
		    (warn "Unknown type ~s for reuse-object, using a new object." (type-of reuse-object)))
		   (t
		    (make-old-player-info variant))))
	(perc-abs (player.perceived-abilities player))
	(active-stats (player.active-stats player)))
    
    (fill-player-abilities! variant (old.abilities old) perc-abs)
    
    (let ((old/stats (old.stats old))
	  (stat-len (variant.stat-length variant)))
      (dotimes (i stat-len)
	(setf (aref old/stats i) (aref active-stats i))))

    (setf (old.see-inv old) (player.see-invisible player))
    (setf (old.speed old) (player.speed player))
    
    old))

(defmethod handle-player-updates! ((variant variant) (player player) (old old-player-info))
  (let ((old/abilities (old.abilities old))
	(perc-abs (player.perceived-abilities player)))

    
    ;; only check perceived changes!
    (when (or (/= (get-armour-rating old/abilities) (get-armour-rating perc-abs))
	      (/= (get-armour-modifier old/abilities) (get-armour-modifier perc-abs)))
      (ask-for-redraw! player '[armour])
      ;; skip window
      )

    (when (/= (player.speed player) (old.speed old))
      (ask-for-redraw! player '[speed]))

    (when (/= (player.see-invisible player) (old.see-inv old))
      (ask-for-update! player '[monsters]))

    (block stat-check
      (let ((old/stats (old.stats old))
	    (a-stats (player.active-stats player)))
	(dotimes (i (variant.stat-length variant))
	  (when (/= (aref a-stats i) (aref old/stats i))
	    (ask-for-redraw! player '[stats])
	    (return-from stat-check nil)))))
    
    player))

#||
(defun %reset-plattr (key table)
  (let ((attr (gethash key table)))
    (check-type attr creature-attribute)
    (setf (attr.value attr) (attr.default-value attr))))
||#


(defmethod reset-player-object! ((variant variant) (player player))

  (let ((actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player))
	(base-stats (player.base-stats player))
	(active-stats (player.active-stats player))
	(modbase-stats (player.modbase-stats player))
	(stat-len (variant.stat-length variant)))

    (setf (get-armour-rating actual-abs) 0
	  (get-armour-modifier actual-abs) 0
	  (get-tohit-modifier actual-abs) 0
	  (get-damage-modifier actual-abs) 0
	  (get-armour-rating perc-abs) 0
	  (get-armour-modifier perc-abs) 0
	  (get-tohit-modifier perc-abs) 0
	  (get-damage-modifier perc-abs) 0
	  (player.burden player) 0
	  (get-stat-sustains player) 0
	  (player.speed player) 110)

    (dotimes (i stat-len)
      ;; active stats are set up right at end of calculation
      (setf (aref active-stats i) 0
	    (aref modbase-stats i) (aref base-stats i))) ;; starts at base, then is modified

    ;; maybe reset resists?
    
    t))

(defun modify-attribute! (key table new-value)
  "Tries to change a named creature-attribute."
  (let ((attr (gethash key table)))
    (check-type attr creature-attribute)
    (setf (attr.value attr) new-value)
    new-value))
    

(defmethod calculate-abilities! ((variant variant) (player player) (race character-race))

  (when-bind (stat-changes (race.stat-changes race))
    (let ((m-stats (player.modbase-stats player)))
      (cond ((is-stat-array? variant stat-changes)
	     (loop for i from 0
		   for x across stat-changes
		   do (incf (aref m-stats i) x)))
	    (t
	     (warn "Unknown format ~s for stat-changes for race ~s" stat-changes race)))))
  
  t)


(defmethod calculate-abilities! ((variant variant) (player player) (cls character-class))

  (when-bind (stat-changes (class.stat-changes cls))
    (let ((m-stats (player.modbase-stats player)))
      (cond ((is-stat-array? variant stat-changes)
	     (loop for i from 0
		   for x across stat-changes
		   do (incf (aref m-stats i) x)))
	    (t
	     (warn "Unknown format ~s for stat-changes for class ~s" stat-changes cls)))))

  t)

(defmethod calculate-abilities! ((variant variant) (player player) (items items-worn))
  "Handles ac and burden."
  ;; we don't use iterator, but access directly
  (let ((actual-abs (player.actual-abilities player))
	(perc-abs (player.perceived-abilities player)))

  (loop for obj across (items.objs items)
	do
	(when obj

	    ;; fix stats
	    (when-bind (stat-changes (get-stat-modifiers (aobj.kind obj)))
	      (let ((m-stats (player.modbase-stats player)))
		(cond ((is-stat-array? variant stat-changes)
		       (loop for i from 0
			     for x across stat-changes
			     do (incf (aref m-stats i) x)))
		      (t
		       (warn "Unknown format ~s for stat-changes for object ~s" stat-changes obj)))))
	    
	    
	    (incf (get-armour-rating actual-abs) (get-armour-rating obj))
	    ;; armour-value always known?  (move to variant?)
	    (incf (get-armour-rating perc-abs) (get-armour-rating obj))
	    (incf (get-armour-modifier actual-abs) (get-armour-modifier obj))
	    ;; sometimes we know bonus
	    (when (is-object-known? obj)
	      (incf (get-armour-modifier perc-abs) (get-armour-modifier obj)))

	    (incf (player.speed player) (aobj.speed-modifier obj))

	    (incf (player.burden player) (object.weight obj))
	    ))
  
;;  (warn "item-calc")
  t))

(defmethod get-weight ((items items-in-container))
  "Returns the weight as a fixnum of the combined total of the container."
  (let ((ret-weight 0))
    (loop for obj across (items.objs items)
	  do
	  (when (and obj (typep obj 'active-object))
	    (incf ret-weight (object.weight obj))))
    ret-weight))

;; move to variant later
(defvar *hack-old/player-info* nil)

(defmethod calculate-creature-bonuses! ((variant variant) (player player))
  "This method is relatively often.  It should not cons!"

  ;;; must be fixed
  (when (eq *hack-old/player-info* nil)
    (setf *hack-old/player-info* (make-old-player-info variant)))
  
  ;; we need to save old values first
  (let ((stat-len (variant.stat-length variant))
	(old (get-old-player-info variant player
				  :reuse-object *hack-old/player-info*))) 
    
    ;;; reset all values that should be filled
    (reset-player-object! variant player)
    
    ;;; then calculate things based on race and class (fairly constant)
    (calculate-abilities! variant player (player.race player))
    (calculate-abilities! variant player (player.class player))

    ;;; calculate based on what you're wearing
    (calculate-abilities! variant player (player.equipment player))

    
    ;; calculate active stats based on modifiers and temporary modifiers
    (let ((a-stats (player.active-stats player))
	  (m-stats (player.modbase-stats player))
	  (c-stats (player.cur-statmods player)))
      (dotimes (i stat-len)
	(setf (aref a-stats i) (+ (aref m-stats i) (aref c-stats i)))))

    
    (when-bind (inventory (get-creature-inventory player))
      (when-bind (worn-items (aobj.contains inventory))
	(let ((backpack-weight (get-weight worn-items)))
	  (incf (player.burden player) backpack-weight))))

    ;; check what has happened, and do necessary updates
    (handle-player-updates! variant player old)
      
    t))

(defmethod roll-hitpoints-for-new-level ((variant variant) (player player))
  (let ((the-class (player.class player))
	(the-race (player.race player)))
    (randint (+ (class.hit-dice the-class) (race.hit-dice the-race)))))

(defmethod get-power-lvl ((player player))
  (player.power-lvl player))

(defmethod gain-power-level! ((variant variant) (player player))
  "lets the player gain a level.. woah!  must be updated later"

  (let* ((the-level (player.power-lvl player))
	 (hp-table (player.hp-table player))
	 (next-hp (aref hp-table the-level)))

    ;; we have been to this level earlier..
    (when (or (eq next-hp nil) (< next-hp 1))
      (setq next-hp (roll-hitpoints-for-new-level variant player))
      (setf (aref hp-table the-level) next-hp))

    (incf (maximum-hp player) next-hp)
    (incf (player.power-lvl player))

    (when (< (player.max-level player) (player.power-lvl player))
      (setf (player.max-level player)  (player.power-lvl player)))

    (format-message! "You attain level ~d and ~d new hitpoints. " (player.power-lvl player) next-hp)

    (ask-for-update! player '[hp])
    (ask-for-update! player '[bonuses])
    
    (ask-for-redraw! player '[basic])
    (ask-for-redraw! player '[extra]) ;; to be on the safe side
    
    ))

(defun find-level-for-xp (xp xp-table)
  "Returns level for given xp according to given xp-table."
  (loop for x across xp-table
	for i of-type fixnum from 1
	do
	(when (> x xp)
;;	  (warn "Returning lvl ~s for xp ~s" (1- i) xp)
	  (return-from find-level-for-xp (1- i))))
  1) ;; fix me later

(defmethod modify-gold! ((player player) amount)
  (when (/= amount 0)
    (incf (player.gold player) amount)
    (ask-for-redraw! player '[gold])
    ))


(defmethod modify-xp! ((player player) amount)
  "Alters the xp for the player with the given amount."

  (assert (numberp amount))
  
  (when (minusp amount)
    (warn "Not implemented reduction in XP yet.")
    (return-from modify-xp! nil))

  (when (= amount 0)
    (return-from modify-xp! nil))
  
  (incf (player.current-xp player) amount)
  (incf (player.maximum-xp player) amount)

  (ask-for-redraw! player '[xp])

  (let ((max-char-level (variant.max-charlevel *variant*)))
    
  
    (loop
     (let* ((cur-level (player.power-lvl player))
	    (cur-xp (player.current-xp player)))

       (cond ((<= max-char-level cur-level)
	      (return-from modify-xp! nil)) ;; already max lvl
	   
	     ;; more than next level?
	     ((>= cur-xp (aref (player.xp-table player) cur-level))
	      (gain-power-level! *variant* player))

	     (t
	      (return-from modify-xp! nil)) ;; no more to do
	     )))
    ))


(defmethod update-xp-table! ((variant variant) (player player))
  "Updates the xp-table on the player, and returns updated player."
  
  (let* ((base-xp-table (variant.xp-table variant))
	 (max-char-level (variant.max-charlevel variant))
	 (the-race (player.race player))
	 (the-class (player.class player))
	 (xp-extra (+ 100
		      (race.xp-extra the-race)
		      (class.xp-extra the-class))))

    (unless (arrayp (player.xp-table player))
      (setf (player.xp-table player) (%make-level-array variant)))

    (let ((xp-table (player.xp-table player)))
      
      (setf (aref xp-table 0) 0)
      (loop for i of-type u-fixnum from 1 below max-char-level
	    do
	    (setf (aref xp-table i) (int-/ (* (aref base-xp-table (1- i)) xp-extra)
					   100))))
    player))

(defmethod update-max-hp! ((variant variant) (player player))
  "Updates the maximum number of hitpoints.  Returns an updated player."

  (let ((lvl (player.power-lvl player))
	(hp-table (player.hp-table player)))
    
    (setf (maximum-hp player)
	  (loop for i from 0 below lvl
		summing (aref hp-table i))))
  player)
	   
(defmethod heal-creature! ((player player) amount)
  "Heals the player and adds notify where needed."

  (let ((max-hp (maximum-hp player)))
  
    (when (< (current-hp player) max-hp)
      
      (incf (current-hp player) amount)
      
      (when (< max-hp (current-hp player)) ;; no more than max..
	(setf (current-hp player) max-hp
	      (player.fraction-hp player) 0))
      
      (ask-for-redraw! player'[hp])
      
      ;; message
      (cond ((< amount 5)
	     (print-message! "You feel a little better."))
	    ((< amount 15)
	     (print-message! "You feel better."))
	    ((< amount 35)
	     (print-message! "You feel much better."))
	    (t
	     (print-message! "You feel very good.")))
      
      t))) ;; it returns nil if when doesn't make sense


(defmethod possible-identify! ((player player) (obj active-object))
  (learn-about-object! player obj :tried)
  ;; fix later
  (learn-about-object! player obj :aware)
  (learn-about-object! player obj :known)
  ;; add xp?
  )

;; Deprecated.. 
(defmethod possible-identify! ((player player) (obj object-kind))
  (warn "Deprecated identify-method called on ~s" obj)
  (learn-about-object! player obj :tried)
  ;; fix later
  (learn-about-object! player obj :aware)
;;  (learn-about-object! player obj :known)
  ;; add xp?
  )


(defun update-player-stat! (player stat action &key (amount 1))
  "Action can be <restore> or a positive or negative integer."

;;  (declare (ignore player stat action))
  
    (let* ((stat-obj (get-stat-obj *variant* stat))
	   (num (stat.number stat-obj))
	   ;;(bs (player.base-stats player))
	   (cur-mods (player.cur-statmods player)))

      
      (ecase action
	(<restore>
	 (when (minusp (aref cur-mods num))
	   (setf (aref cur-mods num) 0)
	   (ask-for-update! player '[bonuses])
	   (format-message! "You feel less ~a" (stat.negative-desc stat-obj))
	   (return-from update-player-stat! t)))
	
	(<increase>
	 
	 (update-player-stat! player stat '<restore>) ;; first restore!
	 
	 (let ((amount (roll-dice 1 4))
	       (cur-value (aref (player.base-stats player) num)))
	   
	   (incf cur-value amount)
	   (when (> cur-value #.(+ 18 100)) ;; hack
	     (setf cur-value #.(+ 18 100)))
	   (setf (aref (player.base-stats player) num) cur-value)
	   
	   (ask-for-update! player '[bonuses])

	   (format-message! "You feel ~a" (stat.positive-desc stat-obj))
	   
	   (return-from update-player-stat! t)))

	(<reduce>
	 (cond ((bit-flag-set? (get-stat-sustains player) (stat.bit-flag stat-obj)) ;; is it sustained?
		(format-message! "You feel very ~a for a moment, but the feeling passes."
				 (stat.negative-desc stat-obj))
		(return-from update-player-stat! t))
	       
	       (t
		;; the alghorithm in regular angband is more sophisticated.. test that later
		(decf (aref cur-mods num) amount)
		(ask-for-update! player '[bonuses])
		(format-message! "You feel very ~a." (stat.negative-desc stat-obj))
		(return-from update-player-stat! t))
	       ))
	)
      
      nil))

(defun %get-hungerlvl (satiation)
  "Returns (lvl colour description init-msg)."
  ;; level colour desc positive-desc negative-desc
  (cond ((< satiation +food-starving+)
	 `(6 ,+term-red+     "Dying   " "IMPOSSIBLE_1" "You're dying of hunger."))
	((< satiation +food-fainting+)
	 `(5 ,+term-orange+  "Fainting" "You're still fainting." "You faint for lack of food."))
	((< satiation +food-weak+)
	 `(4 ,+term-yellow+  "Weak    " "You're still weakened by hunger." "You're severly weakend for lack of food."))
	((< satiation +food-hungry+)
	 `(3 ,+term-yellow+  "Hungry  " "You're still hungry." "You are getting hungry."))
	((< satiation +food-full+)
	 `(2 ,+term-white+   "        " "You're no longer hungry." "You're no longer full."))
	((< satiation +food-max+)
	 `(1 ,+term-l-green+ "Full    " "You are full." "You're no longer gorged."))
	(t
	 `(0 ,+term-l-green+ "Gorged  " "You have gorged yourself." "IMPOSSIBLE_2"))
	))


(defun modify-satiation! (player new-food-amount)
  ;;(warn "Modify satiation ~s" new-food-amount)
  ;; lots of minor pooh

  (when (= 0 new-food-amount)
    (return-from modify-satiation! nil))
  
  (let* ((old-val (player.satiation player))
	 (old-desc (%get-hungerlvl old-val))
	 (old-lvl (car old-desc)))
    
    (incf (player.satiation player) new-food-amount)

    (when (minusp (player.satiation player))
      (setf (player.satiation player) 0))
    
    (let* ((new-val (player.satiation player))
	   (new-desc (%get-hungerlvl new-val))
	   (new-lvl (car new-desc)))
      
      (when (/= new-lvl old-lvl)
	(ask-for-update! player '[bonuses])
	(ask-for-redraw! player '[satiation]))
      
      (when (< new-lvl old-lvl) ;; increase
	(print-message! (fourth new-desc)))
      
      (when (> new-lvl old-lvl) ;; decrease
	(print-message! (fifth new-desc)))
      
      )))

(defmethod copy-player-abilities ((variant variant) (ab player-abilities))
  (let ((new-ab (make-instance 'player-abilities)))
    (fill-player-abilities! variant new-ab ab)
    new-ab))

(defmethod fill-player-abilities! ((variant variant) (to player-abilities) (from player-abilities))
  (dolist (i '(armour-rating armour-modifier tohit-modifier damage-modifier))
    (setf (slot-value to i) (slot-value from i)))
  to)

;; do nothing special as default
(defmethod on-pickup-object ((variant variant) (player player) (obj active-object))
  nil)

(defmethod on-wear-object ((variant variant) (player player) (obj active-object))
  nil)

(defmethod on-take-off-object ((variant variant) (player player) (obj active-object))
  nil)

(defmethod on-drop-object ((variant variant) (player player) (obj active-object))
  nil)

(defmethod on-destroy-object ((variant variant) (player player) (obj active-object))
  nil)

(defmethod get-melee-attack-skill ((variant variant) (player player))
  0)

(defmethod get-ranged-attack-skill ((variant variant) (player player))
  0)

(defmethod get-search-skill ((variant variant) (player player))
  0)

(defun get-monster-knowledge (player monster)
  (let* ((id (etypecase monster
	       (active-monster (get-id (amon.kind monster)))
	       (monster-kind (get-id monster))
	       (string monster)))
	 (obj (gethash id (player.monster-knowledge player))))

    (unless obj
      (setf obj (make-instance 'monster-knowledge :id id))
      (setf (gethash id (player.monster-knowledge player)) obj))

    obj))

(defun get-object-knowledge (player object)
  (let* ((id (etypecase object
	       (active-object (get-id (aobj.kind object)))
	       (object-kind (get-id object))
	       (string object)))
	 (obj (gethash id (player.object-knowledge player))))

    (unless obj
      (setf obj (make-instance 'object-knowledge :id id))
      (setf (gethash id (player.object-knowledge player)) obj))

    obj))

(defun add-monster-knowledge-flag! (player monster flag)
  (let ((know (get-monster-knowledge player monster)))
    (pushnew flag (monster.flags know))))

(defun add-object-knowledge-flag! (player object flag)
  (let ((know (get-object-knowledge player object)))
    (pushnew flag (object.flags know))))


(defmethod get-resists ((obj player))
  (error "Call GET-RESISTANCE-TABLE for PLAYER."))

(defmethod (setf get-resists) (value (obj player))
  (declare (ignore value))
  (error "Call (SETF GET-RESISTANCE-TABLE) for PLAYER."))

(defmethod get-immunities ((obj player))
  (error "Call GET-RESISTANCE-TABLE for PLAYER."))

(defmethod (setf get-immunities) (value (obj player))
  (declare (ignore value))
  (error "Call (SETF GET-RESISTANCE-TABLE) for PLAYER."))

(defmethod get-vulnerabilities ((obj player))
  (error "Call GET-RESISTANCE-TABLE for PLAYER."))

(defmethod (setf get-vulnerabilities) (value (obj player))
  (declare (ignore value))
  (error "Call (SETF GET-RESISTANCE-TABLE) for PLAYER."))

(defmethod resists-element? ((object player) element)
  (plusp (aref (get-resistance-table object) (get-element-number *variant* element))))

(defmethod immune-to-element? ((object player) element)
  (<= 100 (aref (get-resistance-table object) (get-element-number *variant* element))))

(defmethod vulnerable-to-element? ((object player) element)
  (minusp (aref (get-resistance-table object) (get-element-number *variant* element))))
