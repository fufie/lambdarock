;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/levels.lisp - level customisation for Vanilla
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)


(defun van-create-bare-town-level-obj ()
  "Returns a bare town-level."
  (make-instance 'van/town-level :depth 0 :rating 0))


(defmethod level-ready? ((level van/town-level))
  (when (level.dungeon level)
    t))


(defun van/visit-shop (dungeon x y house-num)
  "Visits shop.."
  (declare (ignore dungeon x y))
;;  (warn "triggered..")
  (let ((house (get-house house-num)))
    (visit-house *level* house)))

(defmethod display-house ((player player) (home players-home) &key (offset 0))

  (declare (ignore offset))
  (let ((store-name "Your home")
	(left-col 1)
	(desc-line 3)
	(last-line (get-last-window-line *cur-win*)))

    (clear-window *cur-win*) ;; hack

    (put-coloured-str! +term-yellow+ (format nil "~a" store-name) left-col 1)
  
    (put-coloured-str! +term-white+ "Item Description" 3 desc-line)
    (put-coloured-str! +term-white+ "Weight" 60 desc-line)
    ;;      (put-coloured-str! +term-white+ "Price" 72 line)
    
    ;; should have max here too
    
    (item-table-print (house.items home) :store nil :start-y (+ 2 desc-line))

    ;; make these relative to last line
    (put-coloured-str! +term-yellow+ "ESC" 1 last-line)
    (put-coloured-str! +term-white+ ") Exit from building." 4 last-line)
    (put-coloured-str! +term-yellow+ "g" 31 last-line)
    (put-coloured-str! +term-white+ ") Get item." 32 last-line)
    (put-coloured-str! +term-yellow+ "d" 51 last-line)
    (put-coloured-str! +term-white+ ") Drop item." 52 last-line)
    
    (put-coloured-str! +term-l-red+ "You may: " 0 (- last-line 1))
    
    t))

(defun %home-input-loop (player level home)
  (let ((dungeon (level.dungeon level)))
    
  (block input-loop
    (flet ((drop-item ()
	     (when-bind (selection (select-item dungeon player '(:backpack :equip)
						:prompt "Drop which item? "
						:where :backpack))
	       
	       (let* ((the-table (get-item-table dungeon player (car selection)))
		      (removed-obj (item-table-remove! the-table (cdr selection) :only-single-items t)))
		 (when removed-obj
		   ;; check if full!
		   (item-table-add! (house.items home) removed-obj)
		   t))))
	   
	   (get-item ()
	     (let ((item-len (items.cur-size (house.items home))))
	       (put-coloured-str! +term-white+
				  (format nil "(Items ~a-~a, ESC to exit) Which item are you interested in?"
					  (i2a 0) (i2a (1- item-len)))
				  0 0)
	       (let* ((selected (select-item-from-store home 0 (1- item-len))))
		 (when (and selected (numberp selected))
		   (let* ((items (house.items home))
			  (act-obj (item-table-find items selected))
			  (backpack (aobj.contains (get-creature-inventory player))))
		   
		   (cond ((= 1 (aobj.number act-obj))
			  
			  (item-table-add! backpack act-obj)
			  (item-table-remove! items act-obj)
			  t)
			 
			 ((> (aobj.number act-obj) 1)
			  (let ((new-obj (copy-active-object *variant* act-obj)))
			    (decf (aobj.number act-obj))
			    (setf (aobj.number new-obj) 1)
			    (item-table-add! backpack new-obj)
			    t))
			 )))
		 ))))

      (loop
       ;; should depend on size, ...
       (set-cursor-to *cur-win* :input 10 21)       
       (let ((val (read-one-character)))
	 (cond ((or (eql val #\g)
		    (eql val #\p))
		(when (get-item)
		  (display-house player home)))
	       
	       ((or (eql val #\d)
		    (eql val #\s))
		(when (drop-item)
		  (display-house player home)))
	       
	       ((or (eql val #\Escape)
		    (eql val #\Q))
		
	      (return-from input-loop t))
	       
	       (t
		(warn "Unknown key read: ~s" val)))
	 
	 ;;     (put-coloured-line! +term-white+ "" 0 0)
	 )))
    )))
 

(defmethod visit-house (level (house players-home))
  "Visit the player home."

  (unless (activated? house)
    (activate-object house))
  
  (let ((player *player*))
    (with-dialogue ()
      (clear-window *cur-win*)
      (display-house player house :offset 0)
      (%home-input-loop player level house))
      ))


(defmethod generate-level! ((variant vanilla-variant) (level van/town-level) player)
  "Generates a town and returns it.  If the dungeon
argument is non-NIL it will be re-used and returned as
part of the new level."

  (let* ((*level* level)
	 ;;	 (var-obj *variant*)
	 (settings (get-setting variant :random-level))	;; hack
	 (max-dungeon-width  (setting-lookup settings "max-width"))
	 (max-dungeon-height (setting-lookup settings "max-height"))
	 (dungeon (create-dungeon max-dungeon-width
				  max-dungeon-height
				  :its-depth 0))
	 ;;(term-height (get-frame-height *map-frame*))
	 ;;(term-width (get-frame-width *map-frame*))
	 (town-height 22)
	 (town-width 66)
	 (qy 0)
	 (qx 0)
	 ;;	 (build-stores nil) ;; for testing
	 (build-stores t)
	 )
    
    (declare (type u-fixnum qy qx))
    
    ;;    (warn "Generating town with random state equal to ~s" *random-state*)
    
    ;;    (setf (player.map player) (make-map dungeon))
    
    (fill-dungeon-with-floor! dungeon "permanent-outer-wall")
    
    (fill-dungeon-part-with-floor! dungeon "town-floor"
				   (cons 1 (1- town-width))
				   (cons 1 (1- town-height)))

    
    (setf (level.dungeon level) dungeon)
    
    (when build-stores
      ;; we need stores
      (let* ((y-offset 1)
	     (x-offset 1)
	     (store-num (level.num-stores level))
	     (store-numbers (shuffle-array! (get-array-with-numbers store-num :fill-pointer t)
					    store-num))
	     ;;	   (stores (loop for x from 0 to (1- store-num) collecting (create-store (1+ x))))
	     )
	
	;; move actual stores to level object
	;;      (warn "Stores is ~a" store-numbers)

	
	(dotimes (y 2)
	  (dotimes (x 4)
	    (let* ((house-num (1+ (vector-pop store-numbers)))
		   (the-house (get-house house-num)))
	      (when the-house
		(let ((x0 (+ x-offset (* x 14) 12))
		      (y0 (+ y-offset (* y 9) 6))
		      (shop-id (format nil "shop~d" house-num)))
		  ;;(warn "building ~s [~a]" the-house house-num)
		  (build-house! level the-house x0 y0
				:door-feature (get-floor-type shop-id)
				;;(get-door variant "closed-door") ;; fix this to a proper door later
				:door-trigger (make-coord-event (format nil "house-event-~d" house-num)
								#'van/visit-shop house-num))
		  ;;	(warn "Entering shop ~a, ~a" house-num the-house)))
		  )))
	    ))))
    
    ;; time to check that the shops are kosher
    (dotimes (i 8)
      (let ((house (get-house (1+ i))))
	(unless (activated? house)
	  (activate-object house))

	(when (and (is-store? house) (store-empty? variant house))
	  (fill-up-store! variant house))))

    
    ;; this is just wrong!
    (loop named place-the-player
	  for x of-type u-fixnum = (+ qx (rand-range 3 (- max-dungeon-width 4)))
	  for y of-type u-fixnum = (+ qy (rand-range 3 (- max-dungeon-height 4)))
	  do
	  (when (can-place? dungeon x y :stair)
	    (setf (cave-floor dungeon x y) (get-floor-type "stair-down"))
	    (place-player! dungeon player x y)
	    (return-from place-the-player nil)))

    ;; decoration
    (dotimes (i 10)
      (let ((x (+ 2 (random (- town-width 4))))
	    (y (+ 2 (random (- town-height 4)))))
	(when (equal (floor.id (cave-floor dungeon x y)) "town-floor")
	  (setf (cave-floor dungeon x y) (get-floor variant "town-floor-with-tree")))))

    #||
    ;; decoration
    (dotimes (i 5)
      (let ((x (1+ (random (- town-width 2))))
	    (y (1+ (random (- town-height 2)))))
	(when (cave-empty-bold? dungeon x y)
	  (setf (cave-floor dungeon x y) (get-floor variant "town-floor-with-fountain")))))
    ||#

    
    level))

(defun van-make-town-level-obj (variant player)
  "A sucky function which should be simplified greatly."

  (flet ((do-generation (seed)
	   (let* ((builder (get-level-builder "town-level"))
		  (town (funcall builder))
		  (cl:*random-state* (cl:make-random-state seed)))

	     (generate-level! variant town player))))
  
    ;; we already have a saved value
    (cond ((variant.town-seed variant)
	   (do-generation (variant.town-seed variant)))
	 
	  ;; this is the first time
	  (t
	   (let* ((new-state (cl:make-random-state t)))
	     (setf (variant.town-seed variant) new-state)
	     (do-generation new-state)))
	  )))


(defmethod create-appropriate-level ((variant vanilla-variant) old-level player depth)

  (declare (ignore old-level))
  (let ((level nil))

    (cond ((= depth 0)
	   (setf level (van-make-town-level-obj variant player)))
	  (t ;; the rest
	   (let ((builder (get-level-builder "random-level")))
	     (unless builder
	       (error "Can't find random-level builder"))
	     (setf level (funcall builder)))))
    
    ;; we set the depth now.
    (setf (level.depth level) depth)

    (unless (level-ready? level)
      ;; (warn "Generating level ~a" level)
      (generate-level! variant level player))
    
    ;;(treat-map (level.dungeon level))

    (assert (level-ready? level))
    
    level))


(defmethod activate-object :after ((level van/town-level) &key)
  
  (let* ((dungeon (level.dungeon level))
	 (player *player*)
	 (var-obj *variant*)
	 (turn (variant.turn var-obj))
	 (mod-time (mod turn (variant.day-length var-obj)))
	 ;; quick hack to check how light should behave in town
	 (time-of-day (if (< mod-time (variant.twilight var-obj))
			  'day
			  'night)))

    ;; we need to ensure that shop-triggers are placed
    (with-dungeon (dungeon (coord x y))
      (let* ((floor (coord.floor coord))
	     (num-id (floor.numeric-id floor)))
	(when (and (> num-id 700)
		   (< num-id 709))
	  (let ((trigger (get-coord-trigger dungeon x y))
		(house-num (- num-id 700)))
	    (unless trigger
	      ;;(warn "Making trigger for ~s" house-num) 
	      (setf (get-coord-trigger dungeon x y)
		    (make-coord-event (format nil "house-event-~d" house-num)
				      #'van/visit-shop house-num)))))))

    ;; hackish
    (let ((resident-num (if (eq time-of-day 'day) 4 8)))
	
      ;; add some inhabitants
      (dotimes (i resident-num)
	(allocate-monster! var-obj dungeon player 3 t)))

    ;; only light up vanilla town
    (van/town-illuminate! dungeon player time-of-day)

    level))


(defun van/town-illuminate! (dungeon player time-of-day)
  "Illuminates the town according to the time-of-day."
  
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (let* ((feat (coord.floor coord))
	   (flags (floor.flags feat)))
	     
      ;; slightly interesting grids
      (cond ((not (bit-flag-set? flags +floor-flag-floor+)) ;; non floors actually
	     (bit-flag-add! (coord.flags coord) #.(logior +cave-glow+ +cave-mark+)))
	    ;; day-time
	    ((eq time-of-day 'day)
	     (bit-flag-add! (coord.flags coord) +cave-glow+))
	    ;; at night
	    (t
	     (bit-flag-remove! (coord.flags coord) +cave-glow+))
	    ))
    ;; skip doorways yet
    )

  (ask-for-update! player '[forget-view])
  (ask-for-update! player '[update-view])
  
  (ask-for-redraw! player '[map]))


(defmethod generate-level! ((variant vanilla-variant) (level random-level) player)
  (call-next-method variant level player))


;; this one does the job for all.. only one table for objects in vanilla
(defmethod get-otype-table ((var-obj vanilla-variant) level)
  (declare (ignore level))
  (get-named-gameobj-table var-obj "level" 'objects-by-level))

(defmethod get-mtype-table ((var-obj vanilla-variant) (level random-level))
  (get-named-gameobj-table var-obj level 'monsters-by-level))

(defmethod get-mtype-table ((var-obj vanilla-variant) (level van/town-level))
  (get-named-gameobj-table var-obj level 'monsters-by-level))

;; when we pass a string
(defmethod get-mtype-table ((var-obj vanilla-variant) (level string))
  (get-named-gameobj-table var-obj level 'monsters-by-level))

(defmethod get-floor ((variant vanilla-variant) key)
  (multiple-value-bind (floor found-p)
      (gethash key (variant.floor-types variant))
    (unless found-p
      (warn "Unable to find floor with id/key: ~s" key))
    floor))
   
(defmethod register-level! ((var-obj vanilla-variant) (id string) &key ego-filter &allow-other-keys)

  (call-next-method)

  ;; do the ego-table

  (let ((ego-table (make-game-obj-table)))
    
    (setf (gobj-table.obj-table ego-table) (make-hash-table :test #'equal))

    (setf (gethash id (variant.ego-items-by-level var-obj)) ego-table)

    (when ego-filter
      (if (not (functionp ego-filter))
	  (lang-warn "Ego-filter ~s for level ~s is not a function." ego-filter id)
	  (pushnew (cons id ego-filter)
		   (gethash :ego-items (variant.filters var-obj))
		   :key #'car)))

    
    t))
#||
(defun %get-map-idx (coordmap)
  (flet (;;(a (x) (aref coordmap x))
	 (cave (x) (eq (aref coordmap x) 'cave-wall))
	 (corridor (x) (eq (aref coordmap x) 'corridor))
	 )
    
    (let ((wall-start 0)
	  )

      (cond ((cave 5)
	     (cond
		   ((and (cave 4) (cave 2) (corridor 6) (corridor 8)) (+ wall-start 7))
		   ((and (cave 6) (cave 2) (corridor 4) (corridor 8)) (+ wall-start 4))
		   ((and (cave 4) (cave 8) (corridor 2) (corridor 6)) (+ wall-start 5))
		   ((and (cave 6) (cave 8) (corridor 2) (corridor 4)) (+ wall-start 6))
		   (t wall-start)
		   ))
	    

	    )
	     
	     
      ))
  )


(defun %fill-coordmap (coordmap table i j)

  (let ((alts '(("normal-floor" . corridor)
		("cave-wall" . cave-wall)
		;;("room-wall" . cave-wall)
		)))

    (dotimes (i 10)
      (setf (aref coordmap i) nil))
    
    (dolist (cnt '(1 2 3 4 5 6 7 8 9))
      (let* ((floor (coord.floor (aref table (+ i (aref *ddx* cnt))
				       (+ j (aref *ddy* cnt)))))
	     (which (assoc (floor.id floor) alts :test #'equal)))
	
	(when which
	  (setf (aref coordmap cnt) (cdr which)))))

    coordmap))


(defun treat-map (dungeon)
  (warn "Treating map")
  ;; hack to fix map..
  (let ((table (dungeon.table dungeon))
	(wid (1- (dungeon.width dungeon)))
	(hgt (1- (dungeon.height dungeon)))
	(coordmap (make-array 10 :initial-element nil))
	(used-indexes (make-hash-table :test #'equal))
	)

    (loop for i from 0 below (dungeon.width dungeon)
	  do
	  (loop for j from 0 below (dungeon.height dungeon)
		do
		(when (and (> i 0) (< i wid)
			   (> j 0) (< j hgt))
		  (let ((point (coord.floor (aref table i j))))
		    
		      (%fill-coordmap coordmap table i j)
		      (let ((idx (%get-map-idx coordmap)))
			(when (integerp idx)
			  (let ((found (gethash idx used-indexes)))
			    (unless found
			      (warn "Make new floor at ~s,~s with idx ~s" i j idx)
			      (setf (gethash idx used-indexes)
				    (make-instance 'floor-type :id (floor.id point) :flags (floor.flags point)
						   :text-sym (text-sym point)
						   :gfx-sym (tile-paint-value 42 idx)))
			      (setf found (gethash idx used-indexes)))
			    
			    (setf (coord.floor (aref table i j)) found)
			    )))
		      ))
		))
    dungeon))
||#
