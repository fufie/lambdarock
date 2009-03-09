;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/rooms.lisp - various defines that should be loaded as data
Copyright (c) 2000-2003,2009 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defclass simple-room (room-type) ()) ;; might be removed
(defclass overlapping-room (room-type) ()) ;; might be removed

;; id class name
(define-room "simple-room" 'simple-room "simple room"
  :inherits 'room-type
  :size-mod #1A(0 0 -1 1)
  :min-level 1
  :builder (room-builder (room dungeon player x0 y0)
	    
	   (let ( ;;(depth (dungeon.depth dungeon))
		 (light (%room-has-light? room dungeon 25))
		 (y1 (- y0 (randint 4)))
		 (y2 (+ y0 (randint 3)))
		 (x1 (- x0 (randint 11)))
		 (x2 (+ x0 (randint 11)))
		 (room-wall (get-floor-type "room-wall"))
		 (inside-room-wall (get-floor-type "inside-room-wall"))
		 (regular-floor (get-floor-type "room-floor")))
	
	     (unless (and room-wall inside-room-wall regular-floor)
	       (warn "Unable to build room at (~s,~s) because walls cannot be found."
		     x0 y0)
	       (return-from room-builder nil))
        
	     (generate-room dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) light)
	     (generate-draw dungeon (1- x1) (1- y1) (1+ x2) (1+ y2) room-wall)
	     (generate-fill dungeon x1 y1 x2 y2 regular-floor)

	     (cond ((= 0 (random 20)) ;; pillar room
		    (loop for y from y1 to y2 by 2
			  do
			  (loop for x from x1 to x2 by 2
				do
				(setf (cave-floor dungeon x y) inside-room-wall))))
	  
		   ((= 0 (random 50)) ;; ragged
		    (loop for y from (+ y1 2) to (- y2 2) by 2
			  do
			  (setf (cave-floor dungeon x1 y) inside-room-wall
				(cave-floor dungeon x2 y) inside-room-wall))
	   
		    (loop for x from (+ x1 2) to (- x2 2) by 2
			  do
			  (setf (cave-floor dungeon x y1) inside-room-wall
				(cave-floor dungeon x y2) inside-room-wall))
		    ))))
  )

;; id class name
(define-room "overlapping-room" 'overlapping-room "overlapping room"
  :inherits 'room-type
  :size-mod #1A(0 0 -1 1)
  :min-level 1
  :builder (room-builder (room dungeon player x0 y0)
  
	   (let ((light (%room-has-light? room dungeon 25))
		 (a-y1 (- y0 (randint 4)))
		 (a-y2 (+ y0 (randint 3)))
		 (a-x1 (- x0 (randint 11)))
		 (a-x2 (+ x0 (randint 10)))
		 (b-y1 (- y0 (randint 3)))
		 (b-y2 (+ y0 (randint 4)))
		 (b-x1 (- x0 (randint 10)))
		 (b-x2 (+ x0 (randint 11)))
		 (room-wall (get-floor-type "room-wall"))
		 (floor (get-floor-type "room-floor")))
	

	     (generate-room dungeon (1- a-x1) (1- a-y1) (1+ a-x2) (1+ a-y2) light)
	     (generate-room dungeon (1- b-x1) (1- b-y1) (1+ b-x2) (1+ b-y2) light)
	
	     (generate-draw dungeon (1- a-x1) (1- a-y1) (1+ a-x2) (1+ a-y2) room-wall)
	     (generate-draw dungeon (1- b-x1) (1- b-y1) (1+ b-x2) (1+ b-y2) room-wall)
    
	     (generate-fill dungeon a-x1 a-y1 a-x2 a-y2 floor)
	     (generate-fill dungeon b-x1 b-y1 b-x2 b-y2 floor)
	     )))
