;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: birth.lisp - character creation
Copyright (c) 2000-2004 - Stig Erik Sandoe

||#

(in-package :org.langband.engine)

(defun %query-for-gender (variant player settings)
  (block query-block
    (let* ((offset-x (setting-lookup settings "offset-x"))
	   (offset-y (setting-lookup settings "offset-y"))

	   (info-col (+ offset-x (setting-lookup settings "info-x")))
	   (info-row (+ offset-y (setting-lookup settings "info-y")))
	   (info-colour (setting-lookup settings "info-attr"))
	   (quest-row (+ offset-y (setting-lookup settings "query-y")))
	   (quest-col (+ offset-x (setting-lookup settings "query-x")))
	   (choice-col (+ offset-x (setting-lookup settings "choice-x")))
	   (choice-row (+ offset-y (setting-lookup settings "choice-y")))
	   (choice-colour (setting-lookup settings "choice-attr"))
	   (choice-tcolour (setting-lookup settings "choice-tattr"))
	   (genders (variant.genders variant))
	   (alt-len (length genders))
	   (inp nil))
      (print-text! info-col info-row info-colour
		   "Your 'gender' does not have any significant gameplay effects."
		   :end-col (+ offset-x
			       (setting-lookup settings "instr-x")
			       (setting-lookup settings "instr-w")))
  
      (block input-loop
	(loop
	 (setf inp (interactive-alt-sel quest-col quest-row
					(mapcar #'gender.name genders)
					:settings settings
					:ask-for "gender"))
	 (cond ((eq inp nil)
		(return-from query-block nil))
	   
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.gender player) (nth inp genders))
		(return-from input-loop nil))
	   
	       (t
		(warn "Unknown return-value from input-loop ~s, must be [0..~s)" inp alt-len))
	       )))
  
      (put-coloured-str! choice-tcolour  "Gender" choice-col choice-row) 
      (put-coloured-str! choice-colour   (get-gender-name player) (+ 7 choice-col) choice-row)

      t)))

(defun %query-for-race (variant player settings &key (race-name "Race"))
  (block query-block
    (let* ((offset-x (setting-lookup settings "offset-x"))
	   (offset-y (setting-lookup settings "offset-y"))
	   (info-col (+ offset-x (setting-lookup settings "info-x")))
	   (info-row (+ offset-y (setting-lookup settings "info-y")))
	   (info-colour (setting-lookup settings "info-attr"))
	   (quest-row (+ offset-y (setting-lookup settings "query-y")))
	   (quest-col (+ offset-x (setting-lookup settings "query-x")))
	   (choice-col (+ offset-x (setting-lookup settings "choice-x")))
	   (choice-row (+ offset-y (setting-lookup settings "choice-y")))
	   (choice-colour (setting-lookup settings "choice-attr"))
	   (choice-tcolour (setting-lookup settings "choice-tattr"))
	   (mod-value (setting-lookup settings "altern-cols"))
	   (cur-races (get-races-as-a-list variant))
	   (alt-len (length cur-races))
	   (inp nil))

      (print-text! info-col info-row info-colour
		   (format nil "Your '~a' determines various intrinsic factors and bonuses."
			   (string-downcase race-name))
		   :end-col (+ offset-x
			       (setting-lookup settings "instr-x")
			       (setting-lookup settings "instr-w")))
      

      (block input-loop
	(loop
	 (setf inp (interactive-alt-sel  quest-col quest-row
					 (mapcar #'race.name cur-races)
					 :display-fun #'(lambda (x)
							  (when (and (numberp x) (>= x 0) (< x alt-len))
							    (race.desc (elt cur-races x))))
					 :ask-for (string-downcase race-name)
					 :settings settings
					 :mod-value mod-value
					 ))
	      
	 (cond ((eq inp nil)
		(return-from query-block nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(setf (player.race player) (nth inp cur-races))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from ~a input-loop ~s, must be [0..~s)" race-name inp alt-len))
	       )))


      (put-coloured-str! choice-tcolour race-name choice-col (+ 1 choice-row))
      (put-coloured-str! choice-colour (get-race-name player) (+ 7 choice-col) (+ 1 choice-row))
      
      t)))

(defun %query-for-class (variant player settings)
  (block query-block
    (let* ((offset-x (setting-lookup settings "offset-x"))
	   (offset-y (setting-lookup settings "offset-y"))
	   (info-col (+ offset-x (setting-lookup settings "info-x")))
	   (info-row (+ offset-y (setting-lookup settings "info-y")))
	   (info-colour (setting-lookup settings "info-attr"))
	   (quest-row (+ offset-y (setting-lookup settings "query-y")))
	   (quest-col (+ offset-x (setting-lookup settings "query-x")))
	   (choice-col (+ offset-x (setting-lookup settings "choice-x")))
	   (choice-row (+ offset-y (setting-lookup settings "choice-y")))
	   ;;(choice-colour (setting-lookup settings "choice-attr"))
	   ;;(choice-tcolour (setting-lookup settings "choice-tattr"))
	   (mod-value (setting-lookup settings "altern-cols"))
	   (cur-classes (race.classes (player.race player)))
	   (other-classes nil)
	   (combined-classes nil)
	   (comb-class-len 0)
	   (class-len 0))
	  

      (print-text! info-col info-row info-colour
		   #.(concatenate 'string
				  "Your 'class' determines various intrinsic abilities and bonuses. "
				  "Any entries inside (parantheses) should only be used by advanced players."
				  )
		   
		   :end-col (+ offset-x
			       (setting-lookup settings "instr-x")
			       (setting-lookup settings "instr-w")))

      (cond ((eq cur-classes t)
	     (setq cur-classes (get-classes-as-a-list variant)))
	    ((consp cur-classes)
	     (let ((all-classes (get-classes-as-a-list variant))
		   (tmp-classes nil))
	       (dolist (i all-classes)
		 ;;		   (warn "Checking for ~s in ~s" (class.symbol i) cur-classes)
		 ;; maybe let this handle symbols and strings?
		 (if (find (class.symbol i) cur-classes :test #'eq)
		     (push i tmp-classes)
		     (push i other-classes)))
	       (setq cur-classes tmp-classes)))
	    (t
	     (warn "Unknown classes ~a for race ~a" cur-classes (get-race-name player))
	     (return-from query-block nil)))

      (setq class-len (length cur-classes))
      (setq combined-classes (append cur-classes other-classes))
      (setq comb-class-len (length combined-classes))
	
      (block input-loop
	(loop
	 (let* ((class-names (loop for x in combined-classes
				   for i from 0
				   collecting 
				   (if (>= i class-len)
				       (concatenate 'string "(" (class.name x) ")")
				       (class.name x))))
		(inp (interactive-alt-sel quest-col quest-row
					  class-names
					  :display-fun #'(lambda (x)
							   (when (and (numberp x) (>= x 0) (< x comb-class-len))
							     (class.desc (elt combined-classes x))))
					  :ask-for "class"
					  :settings settings
					  :mod-value mod-value
					  )))
	     
	   (cond ((eq inp nil)
		  (return-from query-block nil))
		   
		 ((and (numberp inp) (<= 0 inp) (< inp comb-class-len))
		  (setf (player.class player) (nth inp combined-classes))
		  (return-from input-loop nil))
		   
		 (t
		  (warn "Unknown return-value from class input-loop ~s, must be [0..~s)"
			inp comb-class-len))
		 ))))

      (put-coloured-str! +term-white+  "Class" choice-col (+ 2 choice-row))
      (put-coloured-str! +term-l-blue+ (get-class-name player)
			 (+ 7 choice-col) (+ 2 choice-row))
      t)))

(defmethod query-for-character-basics! ((variant variant) (player player)
					settings)
  "Interactive questioning to select the basics of the character.
Modififes the passed player object THE-PLAYER.  This is a long function."

  (let* (;;(info-col (setting-lookup settings "info-x"))
	 (offset-x (setting-lookup settings "offset-x"))
	 (offset-y (setting-lookup settings "offset-y"))

	 (info-row (+ offset-y (setting-lookup settings "info-y")))
	 (instr-col (+ offset-x (setting-lookup settings "instr-x")))
	 (instr-row (+ offset-y (setting-lookup settings "instr-y")))
	 (instr-colour (setting-lookup settings "instr-attr"))
	 (instr-width (+ instr-col (setting-lookup settings "instr-w")))

	 (win *cur-win*))
	 

    (clear-window +full-frame+ +foreground+)
    
    ;; print info on process in upper right corner
    (print-text! instr-col instr-row instr-colour
		 #.(concatenate 'string
				"Please answer the following questions.  "
				"Legal answers are shown below the marked question.  You may use " 
				"arrow-keys to highlight answers and display description, "
				"or you may hit 'Q' to quit, 'S' to start all over or '?' " 
				"to enter the generic help-system.")
		 :end-col instr-width)
  
    (clear-window-from win info-row +foreground+) ;; clears things
    (clear-window-from win info-row +decor+) ;; clears things

    (unless (%query-for-gender variant player settings)
      (return-from query-for-character-basics! nil))
    
    (clear-window-from win info-row +foreground+) ;; clears things
    (clear-window-from win info-row +decor+) ;; clears things

    (unless (%query-for-race variant player settings)
      (return-from query-for-character-basics! nil))
    
    (clear-window-from win info-row +foreground+) ;; clears things
    (clear-window-from win info-row +decor+) ;; clears things

    (unless (%query-for-class variant player settings)
      (return-from query-for-character-basics! nil))
    
    (clear-window-from win info-row +foreground+) ;; clears things
    (clear-window-from win info-row +decor+) ;; clears things
   
    t))

(defmethod roll-stats! ((variant variant) (player player))
  "Rolls stats and modifies given player object.
Returns the base-stats as an array or NIL if something failed."
  
  (setf (player.base-stats player)    (make-stat-array variant)
	(player.cur-statmods player) (make-stat-array variant)
	(player.modbase-stats player) (make-stat-array variant)
	(player.active-stats player)  (make-stat-array variant))
  
  (let* ((stat-len (variant.stat-length variant))
	 (arr-len (* 3 stat-len))
;;	 (bonus 0)
	 (rolls (make-array arr-len)))

    ;; roll 1d3, 1d4, 1d5 series
    (block roller
      (while t
	(dotimes (i arr-len)
	  (setf (svref rolls i) (randint (+ 3 (mod i 3)))))

	;; sum up rolls
	(let ((sum (reduce #'+ rolls)))
	  (when (and (< 42 sum) (> 54 sum)) ;; within acceptable range
	    (return-from roller)))))

    ;; now assign values
    (dotimes (i stat-len)
      (let* ((arr-offset (* 3 i))
	     (stat-val (+ 5
			 (svref rolls (+ 0 arr-offset))
			 (svref rolls (+ 1 arr-offset))
			 (svref rolls (+ 2 arr-offset)))))
	
	(setf (svref (player.base-stats player) i) stat-val
	      (svref (player.cur-statmods player) i) 0)
	)))

  ;; returns the array
  (player.base-stats player))

	    
  

(defmethod roll-up-character! ((variant variant) (player player))
  "Rolls up a character and modifies given PLAYER-object."
  ;; dropping auto-roller
  
  (clear-window-from *cur-win* 10 +foreground+)
  
  (roll-stats! variant player)

  (let ((hit-dice (+ (race.hit-dice (player.race player))
		     (class.hit-dice (player.class player)))))

    ;; first level we have max
    (setf (aref (player.hp-table player) 0) hit-dice)
    
    (setf (maximum-hp player) hit-dice
	  (current-hp player) hit-dice))

  (update-xp-table! variant player) ;; hack
  

  ;; improve?  hackish.
  (calculate-creature-bonuses! variant player)
  
  ;; hack
  (setf (current-hp player) (maximum-hp player))
  
  t)


(defmethod equip-character! ((variant variant) player settings)
  "Equips the character with basic equipment.

The equipment specififed for class and race will be added to the
player.
"
  (declare (ignorable settings))
  ;; trigger an event if something should be done
  ;; before character is equipped
  ;;(trigger-event settings :on-pre-equip (list player nil))
  
  ;; first check race and class
  (let* ((race (player.race player))
	 (class (player.class player))
	 (start-eq-race (race.start-eq race))
	 (start-eq-class (class.start-eq class))
	 ;; avoid duplicate equipment
	 (start-eq (remove-duplicates (append start-eq-race start-eq-class) :test #'equal)))

;;    (warn "Trying to equip [~a,~a] with ~s" race class start-eq)
    
    (flet ((add-obj-to-player! (obj a-player)
	     "Adds the object to the player." 
	     (let* ((backpack (get-creature-inventory a-player))
		    (inventory (aobj.contains backpack))
		    ;;(okind (aobj.kind obj))
		    )
	       ;;(warn "adding ~a to inventory ~a" obj inventory)
	       (learn-about-object! a-player obj :aware)
	       (learn-about-object! a-player obj :known) ;; know the object already
	       (item-table-add! inventory obj)))
;;	   (object-id? (arg)
;;	     (keywordp arg))
	   )

      ;; iterate over possible start-equipment
      (dolist (spec start-eq)
	(let ((obj (create-aobj-from-spec variant spec)))

	  ;; do a few tries to ensure we start with a cool weapon inherited
	  ;; from grandmum
	  (dotimes (count 5)
	    (when (or (not obj)
		      (is-cursed? obj)
		      (is-worthless? obj))
	      (setf obj (create-aobj-from-spec variant spec))))

	  ;; by now we should be ok
	  (if obj
	      (add-obj-to-player! obj player)
	      (warn "Unable to find starting-object with id ~s" spec))))
      

      ;; hack.. give the player some gold
      (setf (player.gold player) (random 200))

      ;; trigger an event that should be done right after equip.
      ;;(trigger-event settings :on-post-equip (list player nil))
      )))

(defun %get-name-input! (the-player)
  (let ((new-name (get-string-input "Enter name for your character: " :max-length 15)))
    (when (and new-name (stringp new-name))
      (setf (player.name the-player) new-name))))
	;;      (warn "Got ~s" new-name))
  

(defmethod interactive-creation-of-player ((variant variant))
  "Creates a character with interactive selection.
Returns the new PLAYER object or NIL on failure."

  (let* ((player (produce-player-object variant))
	 (birth-settings (get-setting variant :birth))
	 (display-settings (get-setting variant :char-display))
	 (note-colour (setting-lookup birth-settings "note-colour" +term-white+))
	 (offset-x 0) ;; offsets when displaying info on the page
	 (offset-y 0)
	 (alts '()))
    
    (clear-window +full-frame+)
    (refresh-window +full-frame+)

    ;; load the background
    (multiple-value-bind (book-x book-y)
	(load-setting-background variant *cur-win* birth-settings :layout :centred)
      (paint-window +full-frame+)
      (flush-window +full-frame+)
      (when (and (non-negative-integer? book-x)
		 (non-negative-integer? book-y))
	
	(setf offset-x book-x
	      offset-y book-y)))


    (setf (setting-lookup birth-settings   "offset-x") offset-x
	  (setting-lookup birth-settings   "offset-y") offset-y
	  (setting-lookup display-settings "offset-x") offset-x
	  (setting-lookup display-settings "offset-y") offset-y)
    
    
    ;; get basics of the character
    (let ((basics (query-for-character-basics! variant player birth-settings)))
      (unless basics
	(return-from interactive-creation-of-player nil)))

    ;; now we should have a race
    (let ((rand-name (generate-random-name variant player (player.race player))))
      (when (and rand-name (stringp rand-name))
	(setf (player.name player) rand-name)))

    (unless (player.name player)
      (setf (player.name player) "Foo"))

    
    ;;do rolling
    (let ((rolling (roll-up-character! variant player)))
      (unless rolling
	(return-from interactive-creation-of-player nil)))

    (block handle-misc
      (let ((misc (player.misc player))
	    (race (player.race player))
	    (my-class (player.class player)))

	;; first do age
	(setf (playermisc.age misc) (race.base-age race))

	(etypecase (race.mod-age race)
	  (integer (incf (playermisc.age misc) (race.mod-age race)))
	  (cons (incf (playermisc.age misc) (roll-dice (car (race.mod-age race))
						       (cdr (race.mod-age race)))))
	  (function (funcall (race.mod-age race) variant player race)))

	(etypecase (class.mod-age my-class)
	  (integer (incf (playermisc.age misc) (class.mod-age my-class)))
	  (cons (incf (playermisc.age misc) (roll-dice (car (class.mod-age my-class))
						       (cdr (class.mod-age my-class)))))
	  (function (funcall (class.mod-age my-class) variant player my-class)))

	;; then fix status
	(setf (playermisc.status misc) (race.base-status race))

	(etypecase (race.mod-status race)
	  (integer (incf (playermisc.status misc) (race.mod-status race)))
	  (cons (incf (playermisc.status misc) (roll-dice (car (race.mod-status race))
							  (cdr (race.mod-status race)))))
	  (function (funcall (race.mod-status race) variant player race)))

	(etypecase (class.mod-status my-class)
	  (integer (incf (playermisc.status misc) (class.mod-status my-class)))
	  (cons (incf (playermisc.status misc) (roll-dice (car (class.mod-status my-class))
							  (cdr (class.mod-status my-class)))))
	  (function (funcall (class.mod-status my-class) variant player my-class)))

	(incf (playermisc.status misc) (roll-dice 1 (get-information "status-roll" :default 60)))

	(let ((max-status (get-information "status-cap" :default 100)))
	  (when (> (playermisc.status misc) max-status)
	    (setf (playermisc.status misc) max-status)))

	(unless (plusp (playermisc.status misc))
	  (setf (playermisc.status misc) 1)) ;; scum

	;; fix this one and move it to variant later!
	(cond ((eq (gender.symbol (player.gender player)) '<female>)
	       (setf (playermisc.height misc) (normalised-random (slot-value race 'f-height)
								 (slot-value race 'f-height-mod))
		     (playermisc.weight misc) (normalised-random (slot-value race 'f-weight)
								 (slot-value race 'f-weight-mod))))
	      (t
	       (setf (playermisc.height misc) (normalised-random (slot-value race 'm-height)
								 (slot-value race 'm-height-mod))
		     (playermisc.weight misc) (normalised-random (slot-value race 'm-weight)
								 (slot-value race 'm-weight-mod)))))
		     
      ))
    
    ;; ok.. ask for name and re-roll?
    (setf alts (make-note-buttons *cur-win*
				  '((#\c "'c' to change name")
				    (#\r "'r' to re-roll stats")
				    (#\Q "'Q' to quit")
				    (#\Escape "ESC to continue"))
				  :text-colour note-colour
				  :button-colour :matte-brown))
    
    (block input-loop
      (loop
       (load-setting-background *variant* *cur-win* (get-setting *variant* :char-display))
       (display-creature variant player)
       
       (loop for i in alts do
	     (buttonify-selectable-ui-object *cur-win* i))
       
       (let ((val (select-displayed-alternative alts)))
	 
	 (cond ((eql val #\Q)
		(quit-game&))
	       ;; start over
	       ((eql val #\S)
		nil)
	       ((eql val +escape+)
		(return-from input-loop t))
	       ((or (eql val #\c) (eql val #\C))
		(%get-name-input! player))
	       ((or (eql val #\r) (eql val #\R))
		(roll-up-character! variant player))
	       (t
		nil)))))
      

    ;; we have a new character ready for prime-time, let's flavour the objects
    (distribute-flavours! variant)
  
    ;; time to give him some equipment
    (equip-character! variant player birth-settings)

;;    (warn "stats are now ~s ~s" (player.base-stats the-player) (ok-object? the-player))
    ;;    (add-object-to-inventory! the-player (create-aobj-from-kind-num 118))

    (texture-background! +full-frame+ "" -1)
    (clear-window +full-frame+ -1) ;; clear all layers
    ;;(warn "going switch");
    
    ;;(warn "switched")
    
    player))
    

(defmethod on-new-player (variant player)
  (declare (ignore variant))
  player)

(defmethod on-game-start (variant player)
  (declare (ignore variant))
  player)
