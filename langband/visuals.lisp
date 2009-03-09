;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: visuals.lisp - code to handle various visuals
Copyright (c) 2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.engine)


(defun possibly-increment-animation! (animation cur-tick)
  "Checks if the animation needs update, and if it does, increment it."
  ;; check for upgrade of animation
  (when (> cur-tick (anim.next-change animation))
    (incf (anim.current animation))
    (when (>= (anim.current animation) (anim.number animation)) ;; reset
      (setf (anim.current animation) 0))
    (incf (anim.next-change animation) (anim.change-interval animation)))

  animation)

(defun get-animated-sprite (animation)
  "Returns a sprite from an animation for the current moment."
  (let ((sprite (aref (anim.path animation) (anim.current animation))))
    (assert (positive-integer? sprite))
    sprite))

(defun init-animation! (animation base-tick)
  "Inits the animation with the starting tick."
  (setf (anim.next-change animation) (+ base-tick (anim.change-interval animation))))

(defun clear-old-tiles! (win dungeon player)
  "Clears up tiles that may or may not have had an update."
  (when (integerp win)
    (setf win (aref *windows* win)))
  
  (let* ((pvx (player.view-x player))
	 (pvy (player.view-y player))
	 (fh (get-frame-height *map-frame*))
	 (fw (get-frame-width *map-frame*))
	 (kx 0)
	 (ky 0))
    

    (let ((table (dungeon.table dungeon)))
      (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table)) 
      (dotimes (y (dungeon.height dungeon))
	(dotimes (x (dungeon.width dungeon))
	  (when (eq (coord.repaint (aref table x y)) t)
	    (setf kx (- x pvx)
		  ky (- y pvy))
	    
	    (when (and (<= 0 kx)
		       (<= 0 ky)
		       (< ky fh)
		       (< kx fw))
	      (draw-to-map dungeon x y kx ky)
	      (paint-coord win kx ky))
	    
	    (setf (coord.repaint (aref table x y)) nil))
	  )))
    ))

(defun add-old-tile! (dungeon x y)
  "Add a tile to the list of tiles that need refreshing."
  (let ((table (dungeon.table dungeon)))
    (setf (coord.repaint (aref table x y)) t)))


(defun tag-tiles-near (win dungeon x y xoff yoff)
  "Tries to tag tiles near x,y (with offsets) for refreshing."
  (let ((tile-wid (window.tile-width win))
	(tile-hgt (window.tile-height win)))
    
    (add-old-tile! dungeon x y)
    
    (when (> (+ yoff tile-hgt) tile-hgt)
      (add-old-tile! dungeon (+ x 0) (+ y 1)))
    
    (when (> (+ xoff tile-wid) tile-wid)
      (add-old-tile! dungeon (+ x 1) (+ y 0)))
    
    (when (and (> (+ yoff tile-hgt) tile-hgt)
	       (> (+ xoff tile-wid) tile-wid))
      (add-old-tile! dungeon (+ x 1) (+ y 1)))

  t))

(defmethod init-visual-event ((evt sprite-movement) dungeon player cur-tick)
  (declare (ignorable dungeon player))

  (let* ((win (visevent.window evt))
	 (obj (visevent.object evt))
	 (cur-x (visevent.current-x evt))
	 (cur-y (visevent.current-y evt))
	 (tile-wid (window.tile-width win))
	 (tile-hgt (window.tile-height win))
	 (current-x (+ (x-offset obj) (* tile-wid cur-x)))
	 (current-y (+ (y-offset obj) (* tile-hgt cur-y)))
	 ;;(tbl (visevent.data evt))
	 (anim (visevent.animation evt))
	 )

    (setf (visevent.mode evt) :active)
    
    (setf (visevent.last-x evt) current-x
	  (visevent.last-y evt) current-y
	  (visevent.last-move-x evt) cur-tick
	  (visevent.last-move-y evt) cur-tick)
	  
    #||
    (warn "Walk (~s,~s) -> (~s,~s) at angle ~s"
    (visevent.source-x evt) (visevent.source-y evt)
    (visevent.target-x evt) (visevent.target-y evt)
    (visevent.angle evt))
    ||#
    
    (when anim
      (init-animation! anim cur-tick))
    
    evt))


(defconstant +movement-tolerance+ 2)

(defmethod trigger-visual-event ((evt sprite-movement) dungeon player cur-tick)
  (declare (ignorable dungeon player))

  (let* ((win (visevent.window evt))
	 (obj (visevent.object evt))
	 (cur-x (visevent.current-x evt))
	 (cur-y (visevent.current-y evt))
	 (tile-wid (window.tile-width win))
	 (tile-hgt (window.tile-height win))
	 (current-x (+ (x-offset obj) (* tile-wid cur-x)))
	 (current-y (+ (y-offset obj) (* tile-hgt cur-y)))
	 ;;(tbl (visevent.data evt))
	 (anim (visevent.animation evt))
	 )


    (when anim
      (possibly-increment-animation! anim cur-tick))

    
    ;; should we change where we are?
    (let* ((delta-x (/ (* (cos (visevent.angle evt)) (visevent.move-speed evt) (- cur-tick (visevent.last-move-x evt)))
		       +tick-precision+))
	   (new-x (+ (visevent.last-x evt) delta-x))
	   (new-ix (floor new-x))

	   (delta-y (/ (* (sin (visevent.angle evt)) (visevent.move-speed evt) (- cur-tick (visevent.last-move-y evt)))
		       +tick-precision+))
	   (new-y (+ (visevent.last-y evt) delta-y))
	   (new-iy (floor new-y)))

      ;;(warn "We're at (~s,~s) (~s,~s) -> (~4,2f,~5,2f,~s ~4,2f,~5,2f,~s)"
      ;;cur-x cur-y current-x current-y delta-x new-x new-ix delta-y new-y new-iy)


      ;; slow
      (tag-tiles-near win dungeon (visevent.current-x evt) (visevent.current-y evt)
		      (x-offset obj) (y-offset obj))

      
      (when (and (/= new-ix current-x)
		 (/= current-x (visevent.target-x evt)))
	
	(setf (visevent.last-move-x evt) cur-tick)
	(setf (visevent.last-x evt) new-x)
	(setf (visevent.current-x evt) (int-/ new-ix tile-wid))
	(setf (display-x obj) (visevent.current-x evt))
	(setf (x-offset obj) (- new-ix (* tile-wid (visevent.current-x evt))))
	)
      
      (when (and (/= new-iy current-y)
		 (/= current-y (visevent.target-y evt)))

	(setf (visevent.last-move-y evt) cur-tick)
	(setf (visevent.last-y evt) new-y)
	(setf (visevent.current-y evt) (int-/ new-iy tile-hgt))
	(setf (display-y obj) (visevent.current-y evt))
	(setf (y-offset obj) (- new-iy (* tile-hgt (visevent.current-y evt))))

	)

      (tag-tiles-near win dungeon (visevent.current-x evt) (visevent.current-y evt)
		      (x-offset obj) (y-offset obj))

      
      )

    ;; we just want to move to where we're supposed to
    (when (and (<= (abs (- (+ (* (visevent.current-x evt) tile-wid) (x-offset obj))
			   (* (visevent.target-x evt) tile-wid)))
		   +movement-tolerance+) ;; tolerance of +-2
	       (<= (abs (- (+ (* (visevent.current-y evt) tile-hgt) (y-offset obj))
			   (* (visevent.target-y evt) tile-hgt)))
		   +movement-tolerance+) ;; tolerance of +-2
	       )
      #||
      (warn "Ended walk after ~s msec at ~s ~s" (- cur-tick (gethash 'first-tick tbl))
	    (visevent.target-x evt) (visevent.target-y evt))
      ||#
      
      (setf (display-x obj) nil
	    (display-y obj) nil
	    (x-offset obj) 0
	    (y-offset obj) 0)
	    
      (setf (visevent.mode evt) :done)) ;; end it
    
    t))

(defmethod finalise-visual-event ((evt sprite-movement) dungeon player cur-tick)
  (declare (ignorable dungeon player))

  (setf (visevent.mode evt) :dead)
  
  evt)


(defun make-walk-movement (obj win sx sy tx ty &key speed)
  "Creates a SPRITE-MOVEMENT object with the most common settings for walking."
  
  (when (integerp win)
    (setf win (aref *windows* win)))
  
  (let* ((dsx (* (window.tile-width win) sx))
	 (dsy (* (window.tile-height win) sy))
	 (dtx (* (window.tile-width win) tx))
	 (dty (* (window.tile-height win) ty))
	 (diffx (- dtx dsx))
	 (diffy (- dty dsy)) 
	 (move-dir 'right)
	 (evt (make-instance 'walking-movement 
			     :id "simple walk" 
			     :object obj
			     :window win
			     :current-x sx
			     :current-y sy
			     :blocking? t
			     :move-speed (if (positive-integer? speed)
					     speed
					     (* 2 +walk-speed+)) ;; hack, fix later
			     :source-x sx
			     :source-y sy
			     :target-x tx
			     :target-y ty
			     :angle (atan diffy diffx)

			     )))

    (setf (display-x obj) sx
	  (display-y obj) sy)
    
    ;;(warn "Go from ~s,~s to ~s,~s" dsx dsy dtx dty)

    (when (is-player? obj)
      
      (let ((anim (make-animation :id "movement" :current 0
				  ;;:number 9
				  :change-interval +walk-animation-interval+)))
	
	;; select animation, improve later 
	(cond ((plusp diffx) ;; right
	       (setf move-dir 'right))
	      ((minusp diffx) ;; left
	       (setf move-dir 'left))
	      ((plusp diffy) ;; down
	       (setf move-dir 'down))
	      ((minusp diffy) ;; up
	       (setf move-dir 'up))
	      (t
	       (error "Don't know how to move!!")))
    

      
	(cond ((eq move-dir 'right)
	       (setf (anim.number anim) 9)
	       (setf (anim.path anim)
		     (coerce (loop for i from 0 below 9
				   collecting (tile-paint-value 43 i))
			     'vector)))
	    
	      ((eq move-dir 'left)
	       (setf (anim.number anim) 9)
	       (setf (anim.path anim)
		     (coerce (loop for i from 10 below 19
				   collecting (tile-paint-value 43 i))
			     'vector)))
	    
	      ((eq move-dir 'up)
	       (setf (anim.number anim) 6)
	       (setf (anim.path anim)
		     (coerce (loop for i from 20 below 26
				   collecting (tile-paint-value 43 i))
			     'vector)))
	    
	      ((eq move-dir 'down)
	       (setf (anim.number anim) 7)
	       (setf (anim.path anim)
		     (coerce (loop for i from 30 below 37
				   collecting (tile-paint-value 43 i))
			     'vector)))
	    
	      #-sbcl
	      (t
	       (error "No move-dir.")))
		
	(setf (visevent.animation evt) anim)
	(when (typep obj 'creature)
	  (setf (slot-value obj 'gfx-sym) anim))
	))
    
	      
    evt))

(defun make-thrown-visual (win obj sx sy tx ty &key (speed nil))
  "Creates a THROWN-OBJECT-MOVEMENT object with the most common settings."
  (make-instance 'thrown-object-movement
		 :id "thrown object"
		 :object obj
		 :window (cond ((is-window? win)
				win)
			       ((non-negative-integer? win)
				(aref *windows* win))
			       (t
				(error "Unknown window ~s" win)))
		 
		 :current-x sx
		 :current-y sy
		 :blocking? t
		 :move-speed (if (positive-integer? speed)
				 speed
				 +throw-speed+)
		 :source-x sx
		 :source-y sy
		 :target-x tx
		 :target-y ty
		 :angle (atan (- ty sy) (- tx sx))
		 ))

#||

(defun sprite-movement (obj from-x from-y to-x to-y &key speed)
  (push (make-walk-movement obj *map-frame* from-x from-y to-x to-y :speed speed) *visevents*))


(defun sprite-movement* (obj from-x from-y to-x to-y)
  (multiple-value-bind (fx fy)
      (get-relative-coords from-x from-y)
    (multiple-value-bind (tx ty)
	(get-relative-coords to-x to-y)
      (when (and (non-negative-integer? fx)
		 (non-negative-integer? fy)
		 (non-negative-integer? tx)
		 (non-negative-integer? ty))
	(push (make-walk-movement obj *map-frame* fx fy tx ty) *visevents*)))))
||#
