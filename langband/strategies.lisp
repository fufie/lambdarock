;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: strategies.lisp - some default strategies
Copyright (c) 2002-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defclass primitive-melee-attacker (ai-strategy)
  ((id :initform "primitive-melee-attacker")))

(defclass peaceful-mover (ai-strategy)
  ((id :initform "peaceful-mover")
   (destinations :initform '() :accessor strategy.destinations
		 :documentation "destinations is a list of (x y fun) lists, where
x is the x coord, y is the y coord and fun is an otional trigger function that's called
with (player dungeon monster strategy).  When a coordinate has been reached the list for
the coordinate is removed.")))


(defmethod execute-strategy ((strategy primitive-melee-attacker) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))
  (let ((mx (location-x mon))
	(my (location-y mon))
	(*strategy* strategy)
	(px (location-x *player*))
	(py (location-y *player*))
	(player *player*)
	(staggering (is-staggering? mon))
	(moves nil)
	(use-move nil)
	(use-turn nil))

    (declare (type u16b mx my))

    (%ensure-tactic-chooser)

    (unless staggering
      (when-bind (spab (%pick-special-ability strategy mon))
	(when-bind (trigger-retval (trigger-special-ability *variant* mon (tactic-choice-tactic spab) *player* dungeon))
	  (return-from execute-strategy trigger-retval))))

    ;; then we check moves
    (unless staggering
      (setf moves (get-move-direction mx my px py)))

;;    (warn "~a ~a at (~s,~s) -> (~s,~s) ~s"
;;	  (if staggering "staggering" "") (monster.name mon) mx my px py moves)

    (loop named move-attempts
	  for i from 0 to 4
	  do
	  (let* ((dir (if staggering (aref *ddd* (random 8)) (aref moves i)))
		 (nx (+ mx (aref *ddx* dir)))
		 (ny (+ my (aref *ddy* dir)))
		 )

	    (cond ((cave-floor-bold? dungeon nx ny)
		   (setf use-move t))
		  ((bit-flag-set? (floor.flags (cave-floor dungeon nx ny))
				  +floor-flag-wall+)
		   ;; nothing
		   )
		  ;; skip move through walls
		  ;; skip ruin walls
		  ;; skip doors

		  )
	    ;; skip glyph

	    ;; some monsters never attack, even when they can
	    (when (and use-move (has-ability? mon '<never-attack>))
	      ;; skip learn
	      (setf use-move nil))

	    ;; we have the player next to us.. kill him
	    (when (and use-move (= nx px) (= ny py))
	      (monster-attack! mon player dungeon nx ny)
	      (setf use-turn t
		    use-move nil))

	    ;; some monsters never move
	    (when (and use-move (has-ability? mon '<never-move>))
	      ;; skip learn
	      (setf use-move nil))
	    
	    ;; if some monster is in the way, stop.. fix later
	    (when (and use-move (cave-monsters dungeon nx ny))
	      (setf use-move t))

	    ;; skip more treatment of monsters

	    ;; ok, now move
	    (when use-move
	      (setf use-turn t)
	      (swap-monsters! dungeon player mx my nx ny)
	      ;; skip all the special handling of this case with pickup, ..
	      
	      )

	    (when use-turn
	      (return-from move-attempts t))))
    
    ;; skip fallback spellcasting

    ;; skip update

    ;; skip monster learning

    ;; skip 'remove fear'
    t))


(defmethod execute-strategy ((strategy peaceful-mover) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))
  (let ((mx (location-x mon))
	(my (location-y mon))
	(*strategy* strategy)
	(staggering (is-staggering? mon))
	)

    (when-bind (dest (first (strategy.destinations strategy)))
      ;;(warn "try to go ~s" dest)
      (let ((dest-x (first dest))
	    (dest-y (second dest))
	    (moves nil))

	(when (and (= dest-x mx) (= dest-y my))
	  (when-bind (evt (third dest))
	    (funcall evt *player* dungeon mon strategy))
	    (setf (strategy.destinations strategy) (cdr (strategy.destinations strategy)))
	  (setf dest (first (strategy.destinations strategy)))
	  (if (consp dest)
	      (setf dest-x (first dest)
		    dest-y (second dest))
	      (return-from execute-strategy nil)))

	(setf moves (get-move-direction mx my dest-x dest-y))

	(loop named move-attempts
	      for i from 0 to 4
	      do
	      (let* ((dir (if staggering (aref *ddd* (random 8)) (aref moves i)))
		     (nx (+ mx (aref *ddx* dir)))
		     (ny (+ my (aref *ddy* dir)))
		     )
		;;(warn "At (~s,~s) checking (~s,~s) ~s ~s"
		;;      mx my nx ny (cave-floor-bold? dungeon nx ny) (cave-empty-bold? dungeon nx ny))
		(when (and (cave-empty-bold? dungeon nx ny)
			   (not (and (= nx (location-x *player*))
				     (= ny (location-y *player*)))))
		  ;;(warn "Going (~s,~s)" nx ny)
		  (swap-monsters! dungeon *player* mx my nx ny)
		  (return-from execute-strategy t))

		))
	))

    t))
