;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/player.lisp - code dealing with player object
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defmethod get-class-tile ((variant evomyth) player)
  (values +tilefile-classes+ 37))

(defmethod get-character-picture ((variant evomyth) (player player))
  (let (;;(race-sym   (race.symbol   (player.race player)))
	;;(class-sym  (class.symbol  (player.class player)))
	(gender-sym (gender.symbol (player.gender player))))

    (cond ((eq gender-sym '<male>)
	   '(variant-gfx "people/male-copian-spy.png"))
	  ((eq gender-sym '<female>)
	   '(engine-gfx "people/female-human-bard.png")))))

(defmethod interactive-creation-of-player ((variant evomyth))

  (let* ((player (produce-player-object variant))
	 (birth-settings (get-setting variant :birth))
	 (display-settings (get-setting variant :char-display))
	 (note-colour (setting-lookup birth-settings "note-colour" +term-white+))
	 (offset-x 5) ;; offsets when displaying info on the page
	 (offset-y 5)
	 (alts '()))
    
    (clear-window +full-frame+)
    (refresh-window +full-frame+)

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
	(load-setting-background variant *cur-win* (get-setting *variant* :char-display))
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
		 (lb::%get-name-input! player))
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

(defmethod initialise-character-class! ((var-obj evomyth) (my-class character-class) keyword-args)

  (call-next-method)

  (when-bind (skills (getf keyword-args :skills))
    (setf (class.skills my-class) skills)) ;;(build-skills-obj-from-list var-obj skills)))

  my-class)


(defmethod produce-player-object ((variant evomyth))
  (let ((player (call-next-method)))

    ;;(warn "produce..")

    (let ((table (make-hash-table :test #'equal)))
      (loop for x across (variant.skills variant)
	    do
	    (when x
	      (setf (gethash (evo/skill.slot x) table) 0)))

      (setf (player.skills player) table))
		     
    (when-bind (backpack (create-aobj-from-id "backpack"))
      ;;(warn "Adding ~s" backpack)
      (let ((eq-slots (player.equipment player)))
	(item-table-add! eq-slots backpack 'eq.backpack)
	(setf (get-creature-inventory player) backpack)))

    

    #||
    ;; ensure that we have hash-tables in place
    (unless (hash-table-p (player.calc-attrs player))
      (setf (player.calc-attrs player) (make-hash-table :test #'eq)))
    (unless (hash-table-p (player.temp-attrs player))
      (setf (player.temp-attrs player) (make-hash-table :test #'eq)))
    ||#
    
    player))


(defun %query-for-diet (variant player settings)
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
	   (start-diets (list (get-ability variant '<carnivore>)
			      (get-ability variant '<herbivore>)))
	   (alt-len (length start-diets))
	   (inp nil))

      (print-text! info-col info-row info-colour
		   "Your diet is essential for deciding overall strategies for survival as species."
		   :end-col (+ offset-x
			       (setting-lookup settings "instr-x")
			       (setting-lookup settings "instr-w")))

      

      (block input-loop
	(loop
	 (setf inp (interactive-alt-sel  quest-col quest-row
					 (mapcar #'ability.name start-diets)
					 :display-fun #'(lambda (x)
							  (when (and (numberp x) (>= x 0) (< x alt-len))
							    (ability.description (elt start-diets x))))
					 :ask-for "diet"
					 :settings settings
					 :mod-value mod-value
					 ))
	      
	 (cond ((eq inp nil)
		(return-from query-block nil))
	       
	       ((and (numberp inp) (<= 0 inp) (< inp alt-len))
		(warn "CHOSE DIET ~s" (nth inp start-diets))
		;;(setf (player.race player) (nth inp cur-races))
		(return-from input-loop nil))
	       
	       (t
		(warn "Unknown return-value from ~a input-loop ~s, must be [0..~s)" "Diet" inp alt-len))
	       )))


      (put-coloured-str! choice-tcolour "Diet" choice-col (+ 1 choice-row))
      (put-coloured-str! choice-colour "foo" (+ 7 choice-col) (+ 1 choice-row))
      
      t)))

(defmethod query-for-character-basics! ((variant evomyth) (player player) settings)

  "Interactive questioning to select the basics of the character.
Modififes the passed player object THE-PLAYER.  This is a long function."

  (let* ((offset-x (setting-lookup settings "offset-x"))
	 (offset-y (setting-lookup settings "offset-y"))
	 ;;(info-col (setting-lookup settings "info-x"))
	 (info-row (+ offset-y (setting-lookup settings "info-y")))
	 (instr-col (+ offset-x (setting-lookup settings "instr-x")))
	 (instr-row (+ offset-y (setting-lookup settings "instr-y")))
	 (instr-colour (setting-lookup settings "instr-attr"))
	 (instr-width (setting-lookup settings "instr-w"))
	 ;;(choice-col (setting-lookup settings "choice-x"))
	 ;;(choice-row (setting-lookup settings "choice-y"))
	 (win *cur-win*))
	 

    (clear-window +full-frame+)
    
    ;; print info on process in upper right corner
    (print-text! instr-col instr-row instr-colour
		 #.(concatenate 'string
				"Please answer the following questions.  "
				"Legal answers are shown below the marked question.  You may use " 
				"arrow-keys to highlight answers and display description, "
				"or you may hit 'Q' to quit, 'S' to start all over or '?' " 
				"to enter the generic help-system.")
		 :end-col instr-width)
  
    (clear-window-from win info-row) ;; clears things

    (unless (lb-engine::%query-for-gender variant player settings)
      (return-from query-for-character-basics! nil))
    
    (clear-window-from win info-row) ;; clears things
    (unless (%query-for-diet variant player settings)
      (return-from query-for-character-basics! nil))

    (clear-window-from win info-row) ;; clears things
    ;; only one possible race now
    (setf (player.race player) (gethash "man" (variant.races variant)))
    ;; only one possible class now
    (setf (player.class player) (gethash "hunter" (variant.classes variant)))
    
    (clear-window +full-frame+)
    (refresh-window +full-frame+)

    #||
    
    ;; let's figure out the skill-basics
    (let* ((the-class (player.class player))
	   (skills (class.skills the-class)))

      (unless (consp skills)
	(warn "No info on skills for class."))

      (when (consp skills)
	(let ((skill-table (variant.skills variant)))
	  (dolist (i skills)
	    (let ((s-obj (find (first i) skill-table :key #'(lambda (x) (when x
									  (evo/skill.alias x))))))
	      (unless s-obj
		(warn "Unable to find skill ~s" (first i)))
	      (when s-obj
		(setf (gethash (evo/skill.slot s-obj) (player.skills player)) (second i)))))))
      )

    (interactive-skillpoint-distribution variant player settings)
    ||#
    
    (clear-window +full-frame+)
    (refresh-window +full-frame+)
    
    t))

(defun interactive-skillpoint-distribution (variant player settings)
  
  (let* ((skill-table (variant.skills variant))
	 (objs (make-array (length skill-table) :initial-element nil))
	 (scores (make-array (length skill-table) :initial-element 0))
	 (score-diffs (make-array (length skill-table) :initial-element 0))
	 (points-to-allocate 20)
	 (left-side (setting-lookup settings "skill-left-x"))
	 (right-col (setting-lookup settings "skill-right-x"))
	 (start-row (setting-lookup settings "skill-y"))
	 (column-spacing (setting-lookup settings "skill-column-xpad"))
	 )

    (loop for x across skill-table
	  do
	  (when x
	    (let ((val (gethash (evo/skill.slot x) (player.skills player))))
	      (setf (aref scores (evo/skill.idx x)) val))))
    
    (flet ((display-skill (num colour)
	     (let ((x (aref objs num)))
	       (when x
		 (put-coloured-str! +term-green+ (format nil "~2,'0d" (+ (aref scores num)
									 (aref score-diffs num)))
				    (second x) (third x))
		 (put-coloured-str! colour (evo/skill.id (first x)) (+ 5 (second x)) (third x)))))
	   
	   (display-points-left (row)
	     (put-coloured-str! +term-blue+ "Points left:" (1+ left-side) row)
	     (put-coloured-str! +term-green+ (format nil "~2d" points-to-allocate) (+ 14 left-side) row)
	     (put-coloured-str! +term-blue+ "===============" (1+ left-side) (1+ row)))
	   
	   (display-skill-desc (num row)
	     ;; dumbo clear
	     (dotimes (i 12)
	       (put-coloured-str! +term-red+ "                             " left-side (+ i row)))
	     (when-bind (sk (aref skill-table num))
	       (put-coloured-str! +term-blue+ "Skill: " (1+ left-side) row)
	       (put-coloured-str! +term-red+ (evo/skill.id sk) (+ 8 left-side) row)
	       (print-text! left-side (+ 2 row) +term-umber+
			    (evo/skill.desc sk) :end-col (- right-col column-spacing))
	       )))
	     
      (put-coloured-str! +term-blue+ "Assigning skillpoints" (1+ left-side) start-row)
      ;;(put-coloured-str! +term-blue+ "=====================" (1+ left-side) 2)

      (print-text! left-side (+ 2 start-row) +term-umber+ 
		   "Please assign skillpoints to the skills you wish to develop. 
Add a point to a skill with the '+' key and use '-' if you change your mind.  Move 
up and down the skill-list with the arrowkeys.  Hit ESC or 'Q' when 
you're done.  Information about a skill will be shown below.  Leftover points 
can be assigned later." :end-col (- right-col column-spacing))

    
      (loop for i from 0
	    for x across skill-table
	    when x do
	    (progn
	      (setf (aref objs i) (list x right-col (+ start-row i)))))


      (dotimes (i (length objs))
	(display-skill i +term-blue+))

      (block award-points
	(let ((current 0)
	      (last nil)
	      (max (1- (length objs))))
	  (loop
	   (display-skill current +term-l-red+)
	   (display-points-left (+ start-row 20))
	   (display-skill-desc current (+ start-row 23))
	   
	   (setf last current)
	   (let ((key-input (read-one-character)))
	     (case key-input
	       (#\8 (decf current))
	       (#\2 (incf current))
	       ((#\+ #\6) (when (plusp points-to-allocate)
		      (incf (aref score-diffs current))
		      (decf points-to-allocate)))

	       ((#\- #\4)(when (plusp (aref score-diffs current))
		      (decf (aref score-diffs current))
		      (incf points-to-allocate)))
	     
	       (#\Escape (return-from award-points nil))
	       (#\Q (return-from award-points nil))
	       )

	     (when (minusp current)
	       (setf current max))
	     (when (> current max)
	       (setf current 0))

	     ;;(warn "current is ~s, last is ~s, points left ~s" current last points-to-allocate)

	     ;; change colour if there has been a change
	     (if (plusp (aref score-diffs last))
		 (display-skill last +term-red+)
		 (display-skill last +term-blue+))
	     )
	   )))
      )

    ;; add diffs here
    (loop for i from 0
	  for x across score-diffs
	  for s-o = (aref skill-table i)
	  do
	  (when s-o
	    (incf (gethash (evo/skill.slot s-o) (player.skills player))
		  (aref score-diffs i))))
    ;;(warn "player.skills ~s" (player.skills player))

    (pause-last-line!)
    
    t))

(defun interactive-ability-distribution (variant player settings)
  ;;(let ((ability-groups (variant.ability-groups variant)))
    
  #||
  (let* ((skill-table (variant.skills variant))
	 (objs (make-array (length skill-table) :initial-element nil))
	 (scores (make-array (length skill-table) :initial-element 0))
	 (score-diffs (make-array (length skill-table) :initial-element 0))
	 (points-to-allocate 20)
	 (left-side (setting-lookup settings "skill-left-x"))
	 (right-col (setting-lookup settings "skill-right-x"))
	 (start-row (setting-lookup settings "skill-y"))
	 (column-spacing (setting-lookup settings "skill-column-xpad"))
	 )

    (loop for x across skill-table
	  do
	  (when x
	    (let ((val (gethash (evo/skill.slot x) (player.skills player))))
	      (setf (aref scores (evo/skill.idx x)) val))))
    
    (flet ((display-skill (num colour)
	     (let ((x (aref objs num)))
	       (when x
		 (put-coloured-str! +term-green+ (format nil "~2,'0d" (+ (aref scores num)
									 (aref score-diffs num)))
				    (second x) (third x))
		 (put-coloured-str! colour (evo/skill.id (first x)) (+ 5 (second x)) (third x)))))
	   
	   (display-points-left (row)
	     (put-coloured-str! +term-blue+ "Points left:" (1+ left-side) row)
	     (put-coloured-str! +term-green+ (format nil "~2d" points-to-allocate) (+ 14 left-side) row)
	     (put-coloured-str! +term-blue+ "===============" (1+ left-side) (1+ row)))
	   
	   (display-skill-desc (num row)
	     ;; dumbo clear
	     (dotimes (i 12)
	       (put-coloured-str! +term-red+ "                             " left-side (+ i row)))
	     (when-bind (sk (aref skill-table num))
	       (put-coloured-str! +term-blue+ "Skill: " (1+ left-side) row)
	       (put-coloured-str! +term-red+ (evo/skill.id sk) (+ 8 left-side) row)
	       (print-text! left-side (+ 2 row) +term-umber+
			    (evo/skill.desc sk) :end-col (- right-col column-spacing))
	       )))
	     
      (put-coloured-str! +term-blue+ "Assigning skillpoints" (1+ left-side) start-row)
      ;;(put-coloured-str! +term-blue+ "=====================" (1+ left-side) 2)

      (print-text! left-side (+ 2 start-row) +term-umber+ 
		   "Please assign skillpoints to the skills you wish to develop. 
Add a point to a skill with the '+' key and use '-' if you change your mind.  Move 
up and down the skill-list with the arrowkeys.  Hit ESC or 'Q' when 
you're done.  Information about a skill will be shown below.  Leftover points 
can be assigned later." :end-col (- right-col column-spacing))

    
      (loop for i from 0
	    for x across skill-table
	    when x do
	    (progn
	      (setf (aref objs i) (list x right-col (+ start-row i)))))


      (dotimes (i (length objs))
	(display-skill i +term-blue+))

      (block award-points
	(let ((current 0)
	      (last nil)
	      (max (1- (length objs))))
	  (loop
	   (display-skill current +term-l-red+)
	   (display-points-left (+ start-row 20))
	   (display-skill-desc current (+ start-row 23))
	   
	   (setf last current)
	   (let ((key-input (read-one-character)))
	     (case key-input
	       (#\8 (decf current))
	       (#\2 (incf current))
	       ((#\+ #\6) (when (plusp points-to-allocate)
		      (incf (aref score-diffs current))
		      (decf points-to-allocate)))

	       ((#\- #\4)(when (plusp (aref score-diffs current))
		      (decf (aref score-diffs current))
		      (incf points-to-allocate)))
	     
	       (#\Escape (return-from award-points nil))
	       (#\Q (return-from award-points nil))
	       )

	     (when (minusp current)
	       (setf current max))
	     (when (> current max)
	       (setf current 0))

	     ;;(warn "current is ~s, last is ~s, points left ~s" current last points-to-allocate)

	     ;; change colour if there has been a change
	     (if (plusp (aref score-diffs last))
		 (display-skill last +term-red+)
		 (display-skill last +term-blue+))
	     )
	   )))
      )

    ;; add diffs here
    (loop for i from 0
	  for x across score-diffs
	  for s-o = (aref skill-table i)
	  do
	  (when s-o
	    (incf (gethash (evo/skill.slot s-o) (player.skills player))
		  (aref score-diffs i))))
    ;;(warn "player.skills ~s" (player.skills player))

    (pause-last-line!)
||#    
    t)



(defmethod on-new-player ((variant evomyth) (player player))
  ;; in evomyth we add a quest right away, don't we?

  ;; first quest is to get out of valley
  ;; second is to find a partner
  (dolist (i '("leave-valley" "find-partner"))
    (let ((quest (quest:find-quest variant i)))
      (if quest
	  (quest:init-quest variant quest nil player)
	  (warn "Unable to find quest ~a" i))))

  player)

(defmethod roll-hitpoints-for-new-level ((variant evomyth) (player player))
  "Hitpoints are 1d10 + evo/5 for each lvl."
  (+ (randint 10) (round-/ (aref (player.active-stats player) 2) 5)))
