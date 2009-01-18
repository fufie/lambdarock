;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/rooms.lisp - room-builders that should be common
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(defmethod find-appropriate-room ((variant vanilla-variant)
				  (level random-level)
				  player)
  (declare (ignore player))
  ;; hack
  (let* ((some-val (random 100))
	 (room-id nil)
	 (fallback-id "simple-room")
	 (the-room nil))

    ;; might surprise someone

    (setf room-id (cond ((< some-val 15)
			 "overlapping-room")
			(t
			 "simple-room")))

    (setf the-room (get-room room-id))

    
    (cond (the-room)
	  (t
	   (warn "Unable to find the room ~s, trying to use fallback-room ~s" room-id fallback-id)
	   ;; we must have fallback room available at least
	   (setf the-room (get-room fallback-id))))

    ;;(warn "have ~s with ~s ~s" the-room (room-type.id the-room) (room-type.parent the-room))
    
    (cond ((typep the-room 'room-type)
	   (funcall (room-type.constructor the-room)
		    (room-type.id the-room)))
	  (t
	   (error "Unable to find room ~s, got ~s" room-id the-room)))
    ))


(defmethod build-house! ((level van/town-level) (house house)
			 topleft-x topleft-y
			 &key
			 (door-feature nil)
			 (door-trigger nil)
			 &allow-other-keys)

  (when level
;;    (warn "building house ~a on level ~a at [~a,~a]" house level topleft-x topleft-y)

    (let* ((dungeon (level.dungeon level))
	   (*dungeon* dungeon)
	   (variant *variant*)
	   (y0 topleft-y)
	   (x0 topleft-x)
	   (y1 (- y0 (randint 3)))
	   (y2 (+ y0 (randint 3)))
	   (x1 (- x0 (randint 5)))
	   (x2 (+ x0 (randint 5))))

      (let* (;;(ft-basic  (get-floor *variant* "stone-building"))
	    (ft-top    (get-floor *variant* "town-building-toproof"))
	    (ft-border (get-floor *variant* "town-building-toproof-border"))
	    (ft-middle (get-floor *variant* "town-building-roof"))
	    (ft-bottom (get-floor *variant* "town-building-botroof"))
	    (ft-wall   (get-floor *variant* "town-building-wall"))
	    (ft-window (get-floor *variant* "town-building-wall-window"))
	    (ft-plank (get-floor *variant* "town-building-wall-plank"))
	    (roof-size (- y2 y1)) ;; 1 is for wall
	    (top-size  (1- (floor roof-size 2)))
	    )

	;; fill top row with top roof
	(loop for y-count from 1 to top-size do
	      (loop for x from x1 to x2 do
		    (setf (cave-floor dungeon x (+ y1 y-count))
			  (if (= y-count top-size) ft-border ft-top))))

	;; fill bottom row with wall
	(let ((last nil))
	  (loop for x from x1 to x2 do
		(let ((rand-val (random 4)))
		  (cond ((and (= 0 rand-val) (not (eq last ft-window)))
			 (setf (cave-floor dungeon x y2) ft-window)
			 (setf last ft-window))
			((= 1 rand-val)
			 (setf (cave-floor dungeon x y2) ft-plank)
			 (setf last ft-plank))
			(t
			 (setf (cave-floor dungeon x y2) ft-wall)
			 (setf last ft-wall))
			))
		))

	;; recalculate
	(let ((new-y1 (+ 1 top-size y1))
	      (new-y2 (1- y2)))

	  ;; bottom when there's room
	  (when (> new-y2 new-y1)
	    (loop for x from x1 to x2 do
	      (setf (cave-floor dungeon x new-y2) ft-bottom))
	    (setf new-y2 (1- new-y2)))

	  
	  ;; fill all with middle eventually
	  (loop for y from new-y1 to new-y2 do
		(loop for x from x1 to x2 do
		      (setf (cave-floor dungeon x y) ft-middle)))

	  
	  ))

     ;; add doors
     (let ((tmp (random 4))
	   (x 0)
	   (y 0))

       ;; skip relocating annoying doors
       
       (case tmp
	 ;; bottom
	 (0 (setq y y2
		  x (rand-range x1 x2)))
	 ;; top
	 (1 (setq y y1
		  x (rand-range x1 x2)))
	 ;; right
	 (2 (setq y (rand-range y1 y2)
		  x x2))
	 ;; left
	 (3 (setq y (rand-range y1 y2)
		  x x1))
	 
	 (t
	  (warn "Fall-through in door placement")
	  (setq y y2
		x x2)))

       ;; HACK, always place door at bottom
       (setq y y2)
       
       ;; time to place house number
       
       ;; first check if we got decor
       (cond ((and door-feature (typep door-feature 'active-door))
	      (setf (location-x door-feature) x
		    (location-y door-feature) y)
	      
	      (decor-operation variant door-feature :open :value t)
	      (pushnew door-feature (dungeon.decor dungeon))
	      (setf (cave-decor dungeon x y) door-feature))

	     ;; we got a floor-type
	     ((and door-feature (typep door-feature 'floor-type))

	      (cond ((= y y2)
		     (let ((ft-over  (get-floor *variant* "town-building-door-top"))
			   ;;(ft-under (get-floor *variant* "stone-building-door-bottom"))
			   )
		       
		       (setf (cave-floor dungeon x (1- y)) ft-over)
		       (setf (cave-floor dungeon x y) door-feature)
		       ;;(setf (cave-floor dungeon x y) ft-under)
		       ))
		    (t
		     (setf (cave-floor dungeon x y) door-feature))))

	     ;; we don't know how to do things
	     (t
	      (warn "Built house without a proper door/feature: ~s" door-feature)))
	      
       
       (when door-trigger
	 (setf (get-coord-trigger dungeon x y) door-trigger))
       
       ))

    house))
