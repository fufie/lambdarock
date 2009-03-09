;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: view.lisp - code for figuring out what is seen
Copyright (c) 2000-2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defstruct (vinfo-type (:conc-name vinfo-type.)
		       (:predicate nil)
		       (:copier nil))
  grids ;; 8
  bits  ;; 4 -> 8 (length +vinfo-bit-fields+)
  next-0
  next-1
  x
  y
  d
  r)

(defstruct (vinfo-hack (:conc-name vinfo-hack.)
		       (:predicate nil)
		       (:copier nil))
  num-slopes
  slopes
  slopes-min
  slopes-max)

(defvar *vinfo* (make-array +vinfo-max-grids+))
(declaim (type (simple-vector #.+vinfo-max-grids+) *vinfo*))

;;(defvar *view-size* 0)
;;(defvar *view-array* (make-array +view-max+))

(defun %create-vinfo-hack ()
  "A function that creates and inits a vinfo-hack."
  (let ((hack (make-vinfo-hack)))
    (setf (vinfo-hack.num-slopes hack) 0
	  (vinfo-hack.slopes hack) (make-array +vinfo-max-slopes+)
	  (vinfo-hack.slopes-max hack) (make-array (list (1+ +max-sight+)
							 (1+ +max-sight+)))
	  (vinfo-hack.slopes-min hack) (make-array (list (1+ +max-sight+)
							 (1+ +max-sight+))))
    hack))
	  

(defun vinfo-init-aux (hack x y m)
  "Helper function for vinfo-init."
  
  (let ((i 0)
	(slope-num (vinfo-hack.num-slopes hack)))
    
    (when (and (> m 0)
	       (<= m +scale+))
      (loop named inner
	    for j from 0 below slope-num
	    do
	    (incf i)
	    (when (= m (svref (vinfo-hack.slopes hack) j))
	      ;;(warn "hit on ~a" j)
	      (setq i j)
	      (return-from inner nil)))

;;      (warn "compare ~a ~a" i slope-num) 
      (when (= i slope-num)
	(assert (< slope-num +vinfo-max-slopes+))
	(setf (svref (vinfo-hack.slopes hack) slope-num) m)
	(incf (vinfo-hack.num-slopes hack))))

    (when (< m (aref (vinfo-hack.slopes-min hack) x y))
      (setf (aref (vinfo-hack.slopes-min hack) x y) m))

    (when (> m (aref (vinfo-hack.slopes-max hack) x y))
      (setf (aref (vinfo-hack.slopes-max hack) x y) m))))

;;(trace vinfo-init-aux)

(defun vinfo-init& ()
  "Inits the *vinfo* object."
  
  (let ((hack (%create-vinfo-hack))
	(vinfo *vinfo*)
	(num-grids 0))

    (loop for y from 0 to +max-sight+
	  do
	  ;;(warn "Going ~a" y)
	  (loop for x from y to +max-sight+
		do
		(unless (> (distance 0 0 x y) +max-sight+)
		  ;; set wild values
		  (setf (aref (vinfo-hack.slopes-max hack) x y) 0
			(aref (vinfo-hack.slopes-min hack) x y) 999999999)

		  (assert (< num-grids +vinfo-max-grids+))

		  (incf num-grids)
		  (let ((ty (* 1000 y))
			(tx (* 1000 x)))
	      
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (- ty 500))
					   (+ tx 500)))
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (- ty 500))
					   (- tx 500)))
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (+ ty 500))
					   (+ tx 500)))
		    (vinfo-init-aux hack x y
				    (int-/ (* +scale+ (+ ty 500))
					   (- tx 500))))
		  )))


    (assert (>= num-grids +vinfo-max-grids+))
    (assert (>= (vinfo-hack.num-slopes hack) +vinfo-max-slopes+))

    (setf (vinfo-hack.slopes hack)
	  (stable-sort (vinfo-hack.slopes hack) #'<))

;;    (print (vinfo-hack.slopes hack))

;;    (warn "last part")
    (loop for i from 0 below +vinfo-max-grids+
	  do
	  (setf (svref vinfo i) (make-vinfo-type :grids (make-array +vinfo-grid-field-len+ :initial-element 0)
						 :bits (make-array +vinfo-bit-field-len+
								   :element-type 'vinfo-bit-type
								   :initial-element 0)
						 :x 0
						 :y 0
						 :d 0
						 :r 0
						 )))
    
    (let ((queue-head 0)
	  (queue-tail 0)
	  (queue (make-array (* 2 +vinfo-max-grids+) :initial-element nil))
	  (num-slopes (vinfo-hack.num-slopes hack)))
      
      (declare (type u-fixnum queue-head queue-tail)) ;; maybe even fixnum
      
      (setf (svref queue queue-tail) (svref vinfo 0))
      (incf queue-tail)

      (while (< queue-head queue-tail)
;;	(warn "~a vs ~a -> ~s" queue-head queue-tail (vinfo-type.grids (svref vinfo queue-head)))
;;	(when (> (incf counter) 160)
;;	  (error "blah"))
	(let* ((e queue-head)
	       ;; get next
	       ;;(p (svref queue queue-head))
	       (vinfo-obj (svref vinfo e))  ;; vinfo(e)
	       (grid-array (vinfo-type.grids vinfo-obj))
	       (g (svref grid-array 0))
	       (x (grid-x g))
	       (y (grid-y g)))

	  (incf queue-head)
	  
;;	  (warn "Looping e=~d qh=~d qt=~d g=~d y=~d x=~d"
;;		e queue-head queue-tail g y x)
	  
	  (setf (svref grid-array 0) (grid x y)
		(svref grid-array 1) (grid y x)
		(svref grid-array 2) (grid (- y) x)
		(svref grid-array 3) (grid (- x) y)
		(svref grid-array 4) (grid (- x) (- y))
		(svref grid-array 5) (grid (- y) (- x))
		(svref grid-array 6) (grid y (- x))
		(svref grid-array 7) (grid x (- y)))

	  ;; do slopes
	  (dotimes (i num-slopes)
	    (let ((m (svref (vinfo-hack.slopes hack) i)))
	      (when (and (> e 0)
			 (> m (aref (vinfo-hack.slopes-min hack) x y))
			 (< m (aref (vinfo-hack.slopes-max hack) x y)))
		(let ((div-16 (int-/ i 16)))
		  
		  (bit-flag-add! (aref (vinfo-type.bits vinfo-obj) div-16)
				 (expt 2 (mod i 16)))
		  ))))
	  
	  (setf (vinfo-type.next-0 vinfo-obj) (svref vinfo 0))

	  (when (<= (distance 0 0 (1+ x) y)
		    +max-sight+)
	    (let* ((g (grid (1+ x) y))
		   (last-qobj (svref queue (1- queue-tail)))
		   (f-grdval (svref (vinfo-type.grids last-qobj)
						      0)))
	      (unless (= g f-grdval)
		(let ((some-vinfo (svref vinfo queue-tail)))
		  (setf (svref (vinfo-type.grids some-vinfo) 0)
			g)
		 
		  (setf (svref queue queue-tail) some-vinfo)
		  (incf queue-tail)))

	      (setf (vinfo-type.next-0 vinfo-obj) (svref vinfo (1- queue-tail)))
	      ))
	  
	  (setf (vinfo-type.next-1 vinfo-obj) (svref vinfo 0))
	  
	  (when (<= (distance 0 0 (1+ x) (1+ y))
		    +max-sight+)
	    (let* ((g (grid (1+ x) (1+ y)))
		   (last-qobj (svref queue (1- queue-tail)))
		   (f-grdval (svref (vinfo-type.grids last-qobj)
				    0)))
	      (unless (= g f-grdval)
		(let ((some-vinfo (svref vinfo queue-tail)))
		  (setf (svref (vinfo-type.grids some-vinfo) 0)
			g)
		 
		(setf (svref queue queue-tail) some-vinfo)
		(incf queue-tail)))

	      (setf (vinfo-type.next-1 vinfo-obj) (svref vinfo (1- queue-tail)))
	      ))

	  ;; hack
	  (when (= y x)
	    (setf (vinfo-type.next-0 vinfo-obj)
		  (vinfo-type.next-1 vinfo-obj)))
	  
	  
	  (setf (vinfo-type.y vinfo-obj) y
		(vinfo-type.x vinfo-obj) x
		(vinfo-type.d vinfo-obj) (if (> y x)
					(+ y (int-/ x 2))
					(+ x (int-/ y 2)))
		(vinfo-type.r vinfo-obj) (if (= y 0)
					x
					(if (= x 0)
					    y
					    (if (= x y)
						y
						0))))
	  
					
	  )))

;;    (%hidden-dump)
    
    (values)))

;; these are here to avoid consing up new ones..
(defvar *array-view-size* 0) ;; this is the "fill-pointer"/size of *array-view*
(defvar *array-view* (make-array +view-max+ :element-type 'fixnum :initial-element 0))
(declaim (type u-fixnum *array-view-size*))

(defvar *temp-view-size* 0) ;; this is the "fill-pointer"/size of *temp-view*
(defvar *temp-view* (make-array +view-max+ :element-type 'fixnum :initial-element 0))
(declaim (type u-fixnum *temp-view-size*))

(defvar *temp-bit-arr* (make-array +vinfo-bit-field-len+ :element-type 'vinfo-bit-type))
(defvar *temp-queue* (make-array (* 2 +vinfo-max-grids+) :initial-element nil))


(defun update-view! (dungeon player)
  "Updates the view from the given player."

  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     ;;#+cmu (ext:inhibit-warnings 3)
		     ))

  (let* ((py (location-y player))
	 (px (location-x player))
	 (pg (grid px py))
	 (radius (get-light-radius player))
	 (fast-view *array-view*)
	 (fast-temp *temp-view*)
	 (vinfo *vinfo*)
	 (vinfo-bit-fields *vinfo-bit-fields*)
	 (dun-wid (dungeon.width dungeon))
	 (dun-hgt (dungeon.height dungeon))
	 ;;(fast-view (make-map dungeon))
	 )

    (declare (type u-fixnum pg py px radius dun-wid dun-hgt)
	     (type (simple-array fixnum (#.+view-max+)) fast-view fast-temp)
	     (type (simple-vector 8) vinfo-bit-fields))
    
;;    (declare (:explain :boxing))

;;    (warn "Rad: ~a" radius)
    
    (when (> radius 0)
      (incf radius))
    
    (setf *temp-view-size* 0)

    (macrolet ((add-coord-info (grid flag)
		 (declare (ignore flag))
		 (let ((idx (gensym "view-idx")))
		   `(let ((,idx *array-view-size*))
		     (setf (aref fast-view ,idx) ,grid)
		     (incf *array-view-size*)
		     ,idx)))
	       (add-temp-info (grid flag)
		 (declare (ignore flag))
		 (let ((idx (gensym "temp-idx")))
		   `(let ((,idx *temp-view-size*))
		     (setf (aref fast-temp ,idx) ,grid)
		     (incf *temp-view-size*)
		     ,idx)))
	       (get-real-flag (grid)
		 `(cave-flags dungeon (grid-x ,grid) (grid-y ,grid)))
	       (update-real-flags (grid flag)
		 `(setf (cave-flags dungeon (grid-x ,grid) (grid-y ,grid)) ,flag)))
      
;;      (warn "player at {~a,~a}" px py)
    
      ;; step 0

      (block save-old-flags
	(let ((grid 0)
	      (flag 0)
	      (size *array-view-size*))
	  (declare (type u-fixnum flag size)
		   (type fixnum grid))

	  (dotimes (view-cnt size)
	    (setq grid (aref fast-view view-cnt))
	    (setq flag (get-real-flag grid))
	    
	    (when (bit-flag-set? flag +cave-seen+)
	      (bit-flag-add! flag +cave-temp+)
	      (add-temp-info grid flag))
	    
	    ;; clear
	    (bit-flag-remove! flag #.(logior +cave-view+ +cave-seen+))
	    (update-real-flags grid flag))))

      (setf *array-view-size* 0)
      
      ;; step 1 - player grid
      
      (block player-grid
	(let* ((coord (cave-coord dungeon px py))
	       (flag (coord.flags coord))
	       )

	  (declare (type u-fixnum flag))
	  ;; assume viewed
	  (bit-flag-add! flag +cave-view+)

	  (cond ((< 0 radius)
		 ;; torch
		 ;;(warn "player seen.")
		 (bit-flag-add! flag +cave-seen+))
		
		((bit-flag-set? flag +cave-glow+)
		 (bit-flag-add! flag +cave-seen+)))

	  (add-coord-info pg flag)
	  ;;(warn "fast view is ~a" fast-view)
	  (setf (coord.flags coord) flag)))

      
      
      ;; skip blindness

      (block octant-run
	;; octants
	(loop for o2 of-type u-fixnum from 0 below +vinfo-grid-field-len+ ;; size of the grid array
	      do
	    
	      ;; (warn "octant ~a" o2)
	    
	      (let ((queue-head 0)
		    (queue-tail 0)
		    (queue-len #.(* 2 +vinfo-max-grids+))
		    (queue *temp-queue*)
		    ;;(num-slopes (vinfo-hack.num-slopes hack))
		    (bit-arr *temp-bit-arr*)
		    (last-v (svref vinfo 0))
		    ;;(count 0)
		    )

		(declare (type (simple-array vinfo-bit-type (#.+vinfo-bit-field-len+)) bit-arr)
			 (type vinfo-type last-v)
			 (type u-fixnum queue-head queue-tail))

		(dotimes (i queue-len)
		  (setf (svref queue i) nil))
;;		(warn "fish")
		(dotimes (i +vinfo-bit-field-len+)
		  (setf (aref bit-arr i) (aref vinfo-bit-fields i)))
		
		
;;		(warn "go..")
		
		(setf (svref queue queue-tail) (svref vinfo 1))
		(incf queue-tail)
		(setf (svref queue queue-tail) (svref vinfo 2))
		(incf queue-tail)
	      
		(while (< queue-head queue-tail)
		
;;		  (when (= (incf count) 40)
;;		    (return))
		  (assert (< queue-head queue-len))
		  
		  (let* ((e queue-head)
			 ;; get next
			 ;;(p (svref queue queue-head))
			 (vinfo-obj (svref queue e))
			 (its-d (vinfo-type.d vinfo-obj))
			 (bits (vinfo-type.bits vinfo-obj))
			 (grid-array (vinfo-type.grids vinfo-obj))
			 (g (+ pg (the fixnum (svref grid-array o2))))
			 (x (grid-x g))
			 (y (grid-y g))
			 )
		    
		    (declare (type u-fixnum x y its-d)
			     (type fixnum g)
			     (type (simple-array vinfo-bit-type (#.+vinfo-bit-field-len+)) bits))
		    
		    (incf queue-head)

		    ;;(warn "Bit-arr[~a]: ~s vs ~s" queue-head bit-arr bits)

		    ;; optimise this!!!
		    (when (and (block check-equals
				 (dotimes (i +vinfo-bit-field-len+)
				   (when (bit-flag-and (aref bit-arr i)
						       (aref bits i))
				     (return-from check-equals t)))
				 nil)
			       (< x dun-wid)
			       (< y dun-hgt))
		      #-sbcl
		      (when (minusp g)
			(error "{pg=~s,px=~s,py=~s} -> {g=~s,x=~s,y=~s} -> {o2=~s,e=~s} -> ~s"
			       pg px py g x y o2 e grid-array))
		      
		      (let* ((coord (cave-coord dungeon x y))
			     (info (coord.flags coord)))

			(declare (type u-fixnum info))

;;			(warn "bit ok at {~a,~a} -> ~a" x y info)
		      
			;; we have a wall!
			(cond ((bit-flag-set? info +cave-wall+)

			       (dotimes (i +vinfo-bit-field-len+)
				 (bit-flag-remove! (aref bit-arr i)
						   (aref bits i)))
			       ;;(warn "{~a,~a} -> a wall" x y)

			       ;; we just do stuff to new walls
			       (unless (bit-flag-set? info +cave-view+)
				 (bit-flag-add! info +cave-view+)

				 (cond ((< its-d radius)
					;;(warn "see")
					(bit-flag-add! info +cave-seen+))
				     
				       ((bit-flag-set? info +cave-glow+)
					(let* (;;(x (grid-x g))
					       ;;(y (grid-y g))
					       (xx (if (< x px)
						       (1+ x)
						       (if (> x px)
							   (1- x)
							   x)))
					       (yy (if (< y py)
						       (1+ y)
						       (if (> y py)
							   (1- y)
							   y)))
					       (cave-flags (cave-flags dungeon xx yy)))
					  (declare (type fixnum cave-flags))
					  (when (bit-flag-set? cave-flags +cave-glow+)
					    (bit-flag-add! info +cave-seen+)))))
					      
			       

				 (add-coord-info g info)
				 (setf (coord.flags coord) info)))
				 
			       
			      
			      (t
			       ;; no wall
			       (let ((n-0 (vinfo-type.next-0 vinfo-obj))
				     (n-1 (vinfo-type.next-1 vinfo-obj)))
				 ;; n-0 et al is of type VINFO-TYPE
				 (declare (type vinfo-type n-0 n-1))
				 (when (not (eql last-v n-0))
				   (setf (svref queue queue-tail) n-0
					 last-v n-0)
				   (incf queue-tail))
			       
				 (when (not (eql last-v n-1))
				   (setf (svref queue queue-tail) n-1
					 last-v n-1)
				   (incf queue-tail))


				 ;; newly found non-wall
				 (unless (bit-flag-set? info +cave-view+)
				   (bit-flag-add! info +cave-view+)
				   (cond ((< its-d radius)
					  (bit-flag-add! info +cave-seen+))
					 
					 ((bit-flag-set? info +cave-glow+)
					  (bit-flag-add! info +cave-seen+)))

				   (add-coord-info g info)
				   (setf (coord.flags coord) info))
				 )))

		      
			)))

		  )))
	)

      (block handle-blindness
	(when (is-blind? *variant* player)
	  (let ((the-grid 0)
		(size *array-view-size*)
		(coord nil))
	    (declare (type u-fixnum size)
		     (type fixnum the-grid))
	    (dotimes (blind-cnt size)
	      (setq the-grid (aref fast-view blind-cnt))
	      (setq coord (cave-coord dungeon (grid-x the-grid) (grid-y the-grid)))
	      (bit-flag-remove! (coord.flags coord) +cave-seen+)))))
      

      (block process-new-grids
	(let ((the-grid 0)
	      (the-flag 0)
	      (size *array-view-size*)
	      (x 0)
	      (y 0))
	  
	  (declare (type u-fixnum size the-flag x y)
		   (type fixnum the-grid))
	  ;; fix the new view
	  (dotimes (new-cnt size)
	    (setq the-grid (aref fast-view new-cnt))
	    (setq the-flag (get-real-flag the-grid))
	    
	    ;; was not seen, is now seen
	    (when (and (bit-flag-set? the-flag +cave-seen+)
		       (not (bit-flag-set? the-flag +cave-temp+)))
	      (setq x (grid-x the-grid)
		    y (grid-y the-grid))
	      (note-spot! dungeon x y)
	      (light-spot! dungeon x y)))))


      (block process-old-grids
	(let ((x 0)
	      (y 0)
	      (coord nil)
	      (flag 0)
	      (grid 0)
	      (size *temp-view-size*))
	  (declare (type u-fixnum flag x y size)
		   (type fixnum grid))

	  (dotimes (old-cnt size)
	    (setq grid (aref fast-temp old-cnt))
	    (setq x (grid-x grid)
		  y (grid-y grid))
	    (setq coord (cave-coord dungeon x y))
	    (setq flag (coord.flags coord))
	    
	    ;; remove temp-flag
	    (bit-flag-remove! flag +cave-temp+)
	    (setf (coord.flags coord) flag)
	    ;; was seen, no more
	    (unless (bit-flag-set? flag +cave-seen+)
	      (light-spot! dungeon x y))
	    )))
      
      (values)
      )))
		  
	
(defun forget-view! (dungeon player)
  "Clears everything currently viewed."
  (declare (ignore player))
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     ;;#+cmu (ext:inhibit-warnings 3)
		     ))

;;  (warn "(forget-view!)")
  (let ((the-array *array-view*)
	(size *array-view-size*)
	(x 0)
	(y 0)
	(flag 0)
	(the-grid 0))
    (declare (type u-fixnum size x y flag)
	     (type fixnum the-grid)
	     (type (simple-array fixnum (#.+view-max+)) the-array))

    (dotimes (i size)
      (setq the-grid (aref the-array i))
      (setq x (grid-x the-grid)
	    y (grid-y the-grid))
      (setq flag (cave-flags dungeon x y))
      
      (setf (cave-flags dungeon x y) (bit-flag-remove! flag #.(logior +cave-view+ +cave-seen+)))
      (light-spot! dungeon x y))
      

    (setf *array-view-size* 0)
    
    ;;(setf (fill-pointer the-array) 0)
    ))
