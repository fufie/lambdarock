;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: project.lisp - code for doing projections
Copyright (c) 2001-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defun project-path (dungeon max-range path-array x1 y1 x2 y2 flag)
  "Tries to project a path from (x1,y1) to (x2,y2)."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     ))
	   

  (declare (type fixnum x1 y1 y2 x2 flag max-range))
		     
  (when (and (= x1 x2)
	     (= y1 y2))
    (return-from project-path 0))
  

  (let ((ax 0) ;; absolute
	(ay 0)
	(sx 0) ;; offset
	(sy 0))

    (declare (type fixnum ax ay)
	     (type fixnum sx sy))
    
    (if (< y2 y1)
	(setq ay (the fixnum (- y1 y2))
	      sy -1)
	(setq ay (the fixnum (- y2 y1))
	      sy 1))
    (if (< x2 x1)
	(setq ax (the fixnum (- x1 x2))
	      sx -1)
	(setq ax (the fixnum (- x2 x1))
	      sx 1))

    (flet ((go-direction (dir frac x y)
	     (declare (type fixnum frac x y))
	     (let* ((m (* frac 2))
		    (len 0)
		    (k 0)
		    (half (the fixnum (* ay ax)))
		    (full (the fixnum (* half 2))))
	       
	       (declare (type fixnum m len k half full))
	       
	       (loop
		(vector-push (grid x y) path-array)
		(incf len)
		(when (eq dir :diagonal)
		  (setq k len))
		
		(when (or (>= (+ len (int-/ k 2)) max-range)
			  (and (not (bit-flag-set? flag +project-through+))
			       (and (= x x2)
				    (= y y2)))
			  (and (> len 0)
			       (not (cave-floor-bold? dungeon x y)))
			  (and (bit-flag-set? flag +project-stop+)
			       (and (> len 0)
				    (cave-monsters dungeon x y))))
		  ;;(warn "return ~s ~s,~s ~s ~s" len x y (bit-flag-set? flag +project-through+) (cave-floor-bold? dungeon x y))
		  (return-from go-direction len))
		
		(unless (= m 0)
		  (assert (not (eq :diagonal dir)))
		  (incf frac m)
		  (when (>= frac half)
		    (ecase dir
		      (:vertical   (incf x sx))
		      (:horisontal (incf y sy)))
		    
		    (decf frac full)
		    (incf k)))
		
		(ecase dir
		  (:vertical   (incf y sy))
		  (:horisontal (incf x sx))
		  (:diagonal (incf y sy)
			     (incf x sx)))
		)))
	   
	   )
      #-sbcl
      (declare (dynamic-extent go-direction))
      
      (cond ((> ay ax) ;; vertical
	     (go-direction :vertical
			   (the fixnum (* ax ax))
			   x1
			   (the fixnum (+ y1 sy))
			   ))
	    ((> ax ay) ;; horisontal
	     (go-direction :horisontal
			   (the fixnum (* ay ay))
			   (the fixnum (+ x1 sx))
			   y1
			   ))
	    (t	     
	     (go-direction :diagonal
			   0
			   (the fixnum (+ x1 sx))
			   (the fixnum (+ y1 sy))
			   ))
	    )
      )))


(defun is-legal-target? (dungeon target)
  "Checks if it is a legal target and it's to a projectable location"
  (and target
       (typep target 'target)
       (if (typep (target.obj target) 'active-monster)
	   (creature-alive? (target.obj target))
	   t)
			    
       (projectable? dungeon
		     (location-x *player*) (location-y *player*)
		     (target.x target) (target.y target))))

(defun is-targeting? (source poss-target)
  "Checks if the possible target is targeted by the source."
  (check-type source player)
  (check-type poss-target active-monster)
  (when-bind (target (player.target source))
    (eq (target.obj target) poss-target)))


(defun get-destination-coords (source destination &optional (mult-factor 1))
  "Returns dest-x and dest-y for projection in given destination.
Destination of 5 or a target object will use (current) target."

  (let ((dest-x nil)
	(dest-y nil)
	(check-target nil))

    (when (is-player? source) ;; only relevant if the player is the source
      (when (and (integerp destination) (= destination 5))
	(setf check-target t))
      (when (typep destination 'target)
	(setf check-target t)))
    
    (cond ((and check-target (is-legal-target? *dungeon* (player.target source)))
	   (let ((target (player.target source)))
	     (setf dest-x (target.x target)
		   dest-y (target.y target))))
	  ((or (typep destination 'active-monster)
	       (typep destination 'player))
	   (setf dest-x (location-x destination)
		 dest-y (location-y destination)))
	  (t
	   (setf dest-x (+ (* mult-factor (aref *ddx* destination)) (location-x source))
		 dest-y (+ (* mult-factor (aref *ddy* destination)) (location-y source)))
	   ))

    (warn "Returning ~s,~s from ~s" dest-x dest-y destination)
    (values dest-x dest-y)))


(defun projectable? (dungeon x1 y1 x2 y2)
  "Is it projectable from (x1,y1) to (x2,y2)"
  
  (let* ((max-range 50)
	 (path-arr (make-array (1+ max-range) :fill-pointer 0))
	 (path-len (project-path dungeon max-range path-arr x1 y1 x2 y2 0)))

    (when (plusp path-len)
      (let* ((last-g (aref path-arr (1- path-len)))
	     (last-x (grid-x last-g))
	     (last-y (grid-y last-g)))
	(when (and (cave-floor-bold? dungeon last-x last-y)
		   (= x2 last-x)
		   (= y2 last-y))
	  t)))))

;; deprecated!
(defun display-moving-object (dungeon x y text-sym gfx-sym)
  "Prints the OBJ-CHAR with OBJ-ATTR at given coordinates with proper delay."
  ;;(warn "reimplement moving-object (~s,~s) ~s ~s" x y text-sym gfx-sym)

  (multiple-value-bind (rx ry)
      (get-relative-coords x y)
    (when (and rx ry)
      (let* ((window (aref *windows* *map-frame*))
	     (sym (if (window.gfx-tiles? window) gfx-sym text-sym)))
	
	(setf (window-coord window +effect+ rx ry) sym)
	(paint-coord window rx ry)
	;; this is inefficient but it works for now
	;;(refresh-window window)
	(flush-window window)
	;; ---
	(delay +draw-delay+)
	(light-spot! dungeon x y) ;; clear up
	))))

;; deprecated
(defun draw-relative-effect! (dungeon x y text-sym gfx-sym)
  "Draws a relative effect on a coordinate, no extra delay or relighting."
  (declare (ignore dungeon))
  (multiple-value-bind (rx ry)
      (get-relative-coords x y)
    (when (and rx ry)
      (let* ((window (aref *windows* *map-frame*))
	     (sym (if (window.gfx-tiles? window) gfx-sym text-sym)))
	
	(setf (window-coord window +effect+ rx ry) sym)
	(paint-coord window rx ry)))))



(defmethod do-projection (source target-x target-y flag &key
				 (effect nil) (projected-object nil)
				 (damage 0) (radius 0) (range +max-range+)
				 (sound nil))

  ;;(warn "projection from ~s,~s to ~s,~s" (location-x source) (location-y source) target-x target-y)
  
  (let ((source-x (location-x source))
	(source-y (location-y source))
	(player *player*)
	(dungeon *dungeon*)
	(notice nil) ;; can the player see anything?
	(drawn-beam nil) ;; have we drawn anything?
	(drawn-blast nil) ;; have we drawn anything?
	(blind-player nil) ;; fix
	(draw-delay +draw-delay+)
	(start-x 0) ;; start-coordinate
	(start-y 0) ;; start-coordinate
	(dest-x 0)
	(dest-y 0)
	(cur-x target-x)
	(cur-y target-y)
	(affected-grids (make-array 256 :fill-pointer 0))
	)


    
    ;; hack, jump directly to target
    (cond ((bit-flag-set? flag +project-jump+)
	   (setf start-x target-x
		 start-y target-y)
	   ;; clear flag
	   (bit-flag-remove! flag +project-jump+))
	  ((or (is-player? source) (is-monster? source))
	   (setf start-x source-x
		 start-y source-y))
	  (t
	   (warn "Fell through source-check in PROJECT with source ~s" source)
	   ;; assume target is source
	   (setf start-x target-x
		 start-y target-y)
	   ))

    ;; default
    (setf dest-x target-x
	  dest-y target-y)

    ;; hack, a verify
    (when (and (bit-flag-set? flag +project-through+)
	       (= start-x dest-x)
	       (= start-y dest-y))
      (bit-flag-remove! flag +project-through+))

    ;; skip radius-thing

    ;; initial pos
    (setf cur-x start-x
	  cur-y start-y)
    

    ;; if we're a beam, include this spot
    (when (bit-flag-set? flag +project-beam+)
      (vector-push (grid cur-x cur-y) affected-grids))
	
    (let* ((max-range range)
	   (path-arr (make-array max-range :fill-pointer 0)) ;; set to max-range
	   (path-len (project-path dungeon max-range path-arr start-x start-y dest-x dest-y flag))
	   
	   (max-explosion 25)
	   (explosion-area (make-array max-explosion :initial-element nil))
	   )

      (when sound
	(play-sound sound))
      
      ;;(warn "Projection [~s] from (~s,~s) to (~s,~s) [target: ~s,~s]" path-len start-x start-y dest-x dest-y target-x target-y)

      (let ((gfx-sym (tile-paint-value 4 48)) ;; hack
	    (text-sym (text-paint-value +term-red+ #\*)) ;; hack
	    (diff-x 0)
	    (diff-y 0)
	    (proj-dir 0)
	    (visual (get-visual-projectile projected-object))
	    (gfx-path nil)
	    (text-path nil)
	    (gfx-beam 0)
	    (text-beam 0))

	(when visual
	  (setf gfx-path (projectile.gfx-path visual)
		text-path (projectile.text-path visual))
	  (setf gfx-beam (projectile.gfx-beam visual)
		text-beam (projectile.text-beam visual)))

	(unless (positive-integer? gfx-beam)
	  (setf gfx-beam gfx-sym))
	(unless (positive-integer? text-beam)
	  (setf text-beam text-sym))


	;; this code must be re-written to handle new loop
	(loop named path-tracer
	      for i from 0 to (1- path-len)
	      do

	      (let* ((old-x cur-x)
		     (old-y cur-y)
		     (new-g (aref path-arr i))
		     (new-x (grid-x new-g))
		     (new-y (grid-y new-g)))

		;; if we're going to make a ball, stop one step before a wall!
		(when (and (not (cave-floor-bold? dungeon new-x new-y))
			   (plusp radius))
		  (return-from path-tracer t))
	      
		(setf cur-x new-x
		      cur-y new-y)

		;; if we're a beam, include this spot
		(when (bit-flag-set? flag +project-beam+)
		  (vector-push (grid cur-x cur-y) affected-grids))

		(when (and (not blind-player) (not (bit-flag-set? flag +project-hide+)))

		  (cond ((and (viewport-contains? player cur-x cur-y)
			      (player-has-los-bold? dungeon cur-x cur-y))

			 (setf diff-x (- cur-x old-x)
			       diff-y (- cur-y old-y))
			 (setf proj-dir (get-direction-from-coord-diff diff-x diff-y))
			       
			 
			 (when visual
			   (setf gfx-sym (aref gfx-path proj-dir)
				 text-sym (aref text-path proj-dir)))
			 
			 (display-moving-object dungeon cur-x cur-y
						text-sym gfx-sym)
			 
			 (when (bit-flag-set? flag +project-beam+)
			   (draw-relative-effect! dungeon cur-x cur-y
						  text-beam gfx-beam))
			 
			 (setf drawn-beam t))
		      
			(drawn-beam
			 ;; add delay
			 (delay +draw-delay+)
			 ;;(warn "beam ~s ~s" cur-x cur-y)
			 )))
		
			    
		
		;;	      (format t "~&At (~3s,~3s)~%" cur-x cur-y)
		)))

      ;; hack
      (setf dest-x cur-x
	    dest-y cur-y)
      
					#||
      ;;      (loop for g across path-arr
      ;;	    do
      ;;	    (format t "~&>At (~3s,~3s)~%" (grid-x g) (grid-y g)))

      (loop for g across affected-grids
      do
      (format t "~&-Effect (~3s,~3s)~%" (grid-x g) (grid-y g)))
      ||#

					#||
      ;; hack.. 
      (when (bit-flag-set? flag +project-beam+)
      (setf (fill-pointer affected-grids) (1- (fill-pointer affected-grids))))
      ||#

      ;; get blast area
      (loop for dist from 0 to radius
	    do
	    (loop for y from (- dest-y dist) to (+ dest-y dist)
		  do
		  (loop for x from (- dest-x dist) to (+ dest-x dist)
			do
			(when (and (in-bounds? dungeon x y) ;; skip illegal
				   (= (distance dest-x dest-y x y) dist) ;; circular
				   (projectable? dungeon dest-x dest-y x y)
				   )
			  (push (grid x y) (aref explosion-area dist))
			  ))))
					#||
      (format t "~&Explosion [~s]: ~s~%" radius explosion-area)
				   
      
      
      (warn "Went from (~s,~s,~s) to (~s,~s,~s) with [~s ~s]"
      start-x start-y #\.
      dest-x dest-y #\.
      path-len path-arr)
      ||#
      ;; do a check here if we have explosion


      ;; rewrite this too
      ;; should we show the explosion?

      (when (and (not blind-player) (not (bit-flag-set? flag +project-hide+)))
	(let ((gfx-sym (tile-paint-value 4 60))	;; hack
	      (text-sym (text-paint-value +term-red+ #\*))) ;; hack
	  (loop for i from 0 to radius
		do
		(let ((cur-val (aref explosion-area i))
		      (window (aref *windows* *map-frame*)))
		  (when (consp cur-val)
		    (dolist (j cur-val)
		      (let ((loc-x (grid-x j))
			    (loc-y (grid-y j)))
			(when (and (viewport-contains? player loc-x loc-y)
				   (player-has-los-bold? dungeon loc-x loc-y))
			  ;;(warn "ball at ~s,~s" loc-x loc-y)

			  
			  (when-bind (vis (get-visual-projectile projected-object))
			    (when (plusp (projectile.gfx-explosion vis))
			      (setf gfx-sym (projectile.gfx-explosion vis)))
			    (when (plusp (projectile.text-explosion vis))
			      (setf text-sym (projectile.text-explosion vis))))

			  (multiple-value-bind (rx ry)
			      (get-relative-coords loc-x loc-y)
			    (when (and rx ry)
			      (setf (window-coord window +effect+ rx ry) gfx-sym) ;; fix
			      (paint-coord window rx ry)))

			  (setf drawn-blast t)
			  ;; FIX add a real draw-call here
			  ;;(print-relative! dungeon loc-x loc-y #\* +term-red+)
			  
			  )
			)))
		  ;;(put-cursor-relative! dungeon dest-x dest-y)
		  (refresh-window *map-frame*)
		  (when (or drawn-blast drawn-beam)
		    (delay draw-delay) 
		    )
		  ;;		(light-spot! dungeon x y)
		  ;;		(refresh-window)
		  ))))
      

      ;; fix this too
      (when drawn-beam
	;; clean up
	(loop for i across affected-grids
	      for gx = (grid-x i)
	      for gy = (grid-y i)
	      do
	      (light-spot! dungeon gx gy)))
      
      
      (when drawn-blast
	;; clean up
	(loop for i from 0 to radius
	      do
	      (let ((cur-val (aref explosion-area i)))
		;;		(warn "Cur-val for ~s is ~s" i cur-val)
		(when (consp cur-val)
		  (dolist (j cur-val)
		    (let ((loc-x (grid-x j))
			  (loc-y (grid-y j)))
		      (light-spot! dungeon loc-x loc-y)
		      )))
		)))
      
      (when (or drawn-beam drawn-blast)
	(put-cursor-relative! dungeon dest-x dest-y)
	(refresh-window *map-frame*))
	

      (setf notice (apply-projection-effect! *variant* source path-arr
					     :explosion-area explosion-area
					     :flag flag :damage damage :effect effect))

      )

    notice))

(defmethod apply-projection-effect-to-target! ((variant variant) source target &key
					       (x 0) (y 0) (damage 0)
					       (effect nil) (distance 0))
  (declare (ignore x y damage effect distance source target))
  ;;(warn "Fell through APPLY-PROJ-EFFECT-TO-TARGET ~s ~s ~s" source target effect)
  
  nil)

(defmethod apply-projection-effect-to-target! ((variant variant) source (target active-object)
					       &key
					       (x 0) (y 0) (damage 0)
					       (effect nil) (distance 0))
  (declare (ignore x y damage effect distance source))
  ;;(warn "OBJ: Applying effect to ~s" target)
  nil)

(defmethod apply-projection-effect-to-target! ((variant variant) source (target floor-type)
					       &key
					       (x 0) (y 0) (damage 0)
					       (effect nil) (distance 0))
  (declare (ignore x y damage effect distance source))
  ;;  (warn "FLOOR: Applying effect to ~s" (floor.name target))
  nil)



(defmethod apply-projection-effect! ((variant variant) source path-array
				     &key (explosion-area nil) (flag 0) 
				     (distance 0) (damage 0) (effect nil))
  (declare (ignore distance))
  
  (let ((notice nil)
	(dungeon *dungeon*))

    ;;(warn "APE(V): We're going to dish out effect from ~s" (get-creature-name source))
    #||
    (flet ((apply-to! (target distance grid)
	     (when target
	       (when (apply-projection-effect-to-target! variant source target
							 :x (grid-x grid) :y (grid-y grid)
							 :damage damage :effect effect
							 :distance distance)
		 (setf notice t)))))
    ||#
    (macrolet ((apply-to! (target dist grid)
		 (let ((tgt-sym (gensym "target"))
		       (grid-sym (gensym "grid")))
		   `(let ((,tgt-sym ,target)
			  (,grid-sym ,grid))
		     (when ,tgt-sym
		       (setf notice (apply-projection-effect-to-target! variant source
									,tgt-sym :x (grid-x ,grid-sym)
									:y (grid-y ,grid-sym)
									:damage damage :effect effect
									:distance ,dist))
		       )))))
      
    
      ;; check floors
      (when (bit-flag-set? flag +project-grid+)
	  
	  ;; first do beam
	  (loop for grid across path-array do
		(apply-to! (cave-floor dungeon (grid-x grid) (grid-y grid)) 0 grid)
		(apply-to! (cave-decor dungeon (grid-x grid) (grid-y grid)) 0 grid))

	  ;; then do explosion
	  (loop for i from 0
		for x across explosion-area
		do
		(dolist (grid x) ;; coordinates
		  (apply-to! (cave-floor dungeon (grid-x grid) (grid-y grid)) i grid)
		  (apply-to! (cave-decor dungeon (grid-x grid) (grid-y grid)) i grid))
		))
	  

      ;; do updates
      ;; check objects
      (when (bit-flag-set? flag +project-item+)
	(flet ((apply-to-objs! (grid distance)
		 (let ((obj-table (cave-objects dungeon (grid-x grid) (grid-y grid))))
		   (when obj-table
		     (dolist (i (items.objs obj-table))
		       (apply-to! i distance grid))))))
	  
	  ;; first do beam
	  (loop for g across path-array do
		(apply-to-objs! g 0))
	  ;; then do explosion
	  (loop for i from 0
		for x across explosion-area
		do
		(dolist (c x) ;; coordinates
		  (apply-to-objs! c i)))
	  ))
      
      ;; check monsters
      (when (bit-flag-set? flag +project-kill+)
	(flet ((apply-to-monsters! (grid distance)
		 (let ((loc-x (grid-x grid))
		       (loc-y (grid-y grid)))

		   (when (cave-floor-bold? dungeon loc-x loc-y)	;; monsters in wall are safe
		     (dolist (m (cave-monsters dungeon loc-x loc-y))
		       (unless (eq source m)
			 (apply-to! m distance grid)))
		     (when (player-is-at? *player* loc-x loc-y)
		       (apply-to! *player* distance grid))
		     ))))
		   
	  ;; first do beam
	  (loop for g across path-array do
		(apply-to-monsters! g 0))

	  ;; then do explosion
	  (loop for i from 0
		for x across explosion-area
		do
		(dolist (c x) ;; coordinates
		  (apply-to-monsters! c i)))
	  ))
	
      ;; check player
            
   
    
      notice)))

(defun apply-effect-to-area (dungeon left-x left-y w h effect)

  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array t (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (loop for x of-type fixnum from left-x below (+ left-x w)
	  do
	  (loop for y of-type fixnum from left-y below (+ left-y h)
		do
		(when (in-bounds? dungeon x y)
		  (funcall effect (aref table x y) x y))))
    t))

(defun apply-effect-to-circle (dungeon centre-x centre-y radius effect)

  (let ((table (dungeon.table dungeon))
	(left-x (- centre-x radius))
	(left-y (- centre-y radius)))

    (declare (type (simple-array t (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (loop for x of-type fixnum from left-x below (+ left-x radius radius)
	  do
	  (loop for y of-type fixnum from left-y below (+ left-y radius radius)
		do
		(when (and (in-bounds? dungeon x y)
			   (< (distance centre-x centre-y x y) radius))
		  (funcall effect (aref table x y) x y))))
    
    t))

