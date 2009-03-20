;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/levels.lisp - code dealing with various levels
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defmethod level-ready? ((level evo/valley))
  (when (level.dungeon level)
    t))

(defun create-bare-valley-level-obj ()
  "Returns a bare valley-level."
  (make-instance 'evo/valley :depth 3 :rating 0))

(defun make-valley-level-obj (variant player)
  (declare (ignore variant player))
  (funcall (get-level-builder "valley")))


(defmethod create-appropriate-level ((variant evomyth) old-level player depth)

  (warn "old-level is ~s, depth is ~s, leave ~s" old-level depth (player.leaving? player))

  (let ((level nil)
	(where-to (player.leaving? player)))

    (setf level (make-valley-level-obj variant player))

    (warn "level for ~s is ~s ~s" where-to level (level.depth level))
    
    ;; we set the depth now.
    ;;(setf (level.depth level) depth)

    (unless (level-ready? level)
      ;; (warn "Generating level ~a" level)
      (generate-level! variant level player))
    
    
    (assert (level-ready? level))
    
    level))

(defun evo/place-person (id x y)
  (let ((person (produce-active-monster *variant* id)))
    (if person
	(place-single-monster! *dungeon* *player* person x y nil)
	(warn "Unable to find person/monster ~s" id))
    person))


(defmethod generate-level! ((variant evomyth) (level evo/valley) player)
  (let ((dungeon nil)
        (depth-constant (level.depth level))
        (*level* level))
    
    (warn "Reading map")
    (setf dungeon  (read-map variant "variants/evomyth/maps/valley.lmap"))
    (warn "read map")
    (setf dungeon (treat-map dungeon)) ;; inefficient
    
    (setf (level.dungeon level) dungeon
          (dungeon.depth dungeon) (level.depth level))

    (warn "DUN depth ~s" (dungeon.depth dungeon))

    (let ((px (flag "last-town-px"))
	  (py (flag "last-town-py")))
      (unless (positive-integer? px)
	(setf px 7))
      (unless (positive-integer? py)
	(setf py 7))
      (place-player! dungeon player px py))

    (flet ((place-person (id x y)
             (let ((*variant* variant)
                   (*dungeon* dungeon)
                   (*player* player))
               (evo/place-person id x y))))

      (place-person (if (is-female? player) "grandma" "grandpa") 12 12))

          ;; we want monsters, but at least 20 away from player
      (dotimes (i (+ depth-constant 10 (randint 8)))
	(allocate-monster! variant dungeon player 20 t))
    
    level))

(defmethod get-otype-table ((var-obj evomyth) level)
  (declare (ignore level))
  (get-named-gameobj-table var-obj "level" 'objects-by-level))

(defmethod get-mtype-table ((var-obj evomyth) (level evo/valley))
  (get-named-gameobj-table var-obj level 'monsters-by-level))

(defmethod get-mtype-table ((var-obj evomyth) (level string))
  (get-named-gameobj-table var-obj level 'monsters-by-level))


(defmethod print-depth ((variant evomyth) (level level) setting)
  "prints current depth somewhere"
  (declare (ignorable setting))
  (with-frame (+misc-frame+)
    (let ((column (- (get-frame-width +misc-frame+) 8))) ;;(setting-lookup setting "depth")))
      (put-coloured-line! +term-l-blue+ (format nil "~d ft" (* 50 (level.depth level))) column 0))))

(defmethod activate-object :after ((level evo/valley) &key)
  (let* ((dungeon (level.dungeon level))
	 (player *player*)
	 ;;(var-obj *variant*)
	 )

    (let ((evt (make-coord-event "left-valley"
				 #'(lambda (dun x y)
				     (declare (ignorable dun x y))
				     (when (doing-quest? *player* "leave-valley")
				       (warn "We are doing the leave-valley quest")
				       (let* ((var-obj *variant*))
					 (advance-quest var-obj "leave-valley" *player*))))

				 nil)))
      (setf (get-coord-trigger (level.dungeon level) 5 5) evt
	    (get-coord-trigger (level.dungeon level) 5 6) evt))

    
    
    (illuminate-town! dungeon player 'day)))


(defun illuminate-town! (dungeon player time-of-day)
  "Illuminates the town according to the time-of-day."
  (warn "illuminating")
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
  (ask-for-redraw! player '[map])


  )
(defun %get-map-idx (coordmap)
  (flet (;;(a (x) (aref coordmap x))
	 (water (x) (eq (aref coordmap x) 'water))
	 (grass (x) (eq (aref coordmap x) 'grass))
	 (bridge (x) (eq (aref coordmap x) 'bridge))
	 (road (x) (eq (aref coordmap x) 'road))
    	 (perm-solid (x) (eq (aref coordmap x) 'perm-solid)))
    
    (let ((water-start 206)
	  (bridge-start 384)
	  (road-start 416)
	  (mix-start 270))

      (cond ((water 5)
	     (cond
		   ((and (grass 4) (bridge 2)) (+ bridge-start 0))
		   ((and (grass 6) (bridge 2)) (+ bridge-start 2))
		   ((and (grass 4) (bridge 8)) (+ bridge-start 8))
		   ((and (grass 6) (bridge 8)) (+ bridge-start 10))
		   

		   ((and (grass 2) (grass 4)) (+ water-start 2))
		   ((and (grass 8) (grass 4)) (+ water-start 16))
		   ((and (grass 8) (grass 6)) (+ water-start 7))
		   ((and (grass 2) (grass 6)) (+ water-start 0))

		   ((grass 6) (+ water-start 9))
		   ((grass 4) (+ water-start 20))
		   ((grass 8) (+ water-start 25))
		   ((grass 2) (+ water-start 4))

		   #||
		   ((and (bridge 1) (bridge 2) (bridge 3)) (+ bridge-start 1))
		   ((and (bridge 1) (bridge 4) (bridge 7)) (+ bridge-start 23))
		   ((and (bridge 3) (bridge 6) (bridge 9)) (+ bridge-start 21))
		   ((and (bridge 7) (bridge 8) (bridge 9)) (+ bridge-start 9))
		   ||#

		   ((and (bridge 4) (bridge 7) (bridge 8)) (+ bridge-start 28))
		   ((and (bridge 6) (bridge 9) (bridge 8)) (+ bridge-start 27))
		   ((bridge 2) (+ bridge-start 1))
		   ((bridge 4) (+ bridge-start 23))
		   ((bridge 6) (+ bridge-start 21))
		   ((bridge 8) (+ bridge-start 9))
		   
		   ((bridge 1) (+ bridge-start 4))
		   ((bridge 3) (+ bridge-start 3))
		   ((bridge 7) (+ bridge-start 20))
		   ((bridge 9) (+ bridge-start 19))		   
		   		     
		   (t (+ 328 (random 3)))
		   ))
	    
	    ((grass 5)
	     (cond ((and (water 6) (water 3) (water 2) (water 1)) (+ mix-start 28))
		   ((and (water 4) (water 7) (water 2) (water 1)) (+ mix-start 26))
		   ((and (water 4) (water 3) (water 2) (water 1)) (+ mix-start 26))
		   ((and (water 6) (water 9) (water 8) (water 3)) (+ mix-start 22))
		   ((and (water 4) (water 9) (water 8) (water 7)) (+ mix-start 14))
		   ((and (water 9) (water 6) (water 8)) (+ mix-start 3))
		   ((and (water 9) (water 6) (water 3)) (+ mix-start 20))
		   ((and (water 7) (water 4) (water 1)) (+ mix-start 9))

		   ;; several of these are dubious, but I lack proper graphics
		   ((and (water 8) (water 6)) (+ mix-start 2))
		   ((and (water 8) (water 4)) (+ mix-start 0))
		   ((and (water 8) (water 9)) (+ mix-start 2))
		   ((and (water 8) (water 7)) (+ mix-start 0))
		   
		   ((and (water 2) (water 6)) (+ mix-start 16))
		   ((and (water 2) (water 4)) (+ mix-start 7))
		   ((and (water 2) (water 3)) (+ mix-start 16))
		   ((and (water 2) (water 1)) (+ mix-start 7))

		   ((and (water 6) (water 9)) (+ mix-start 2))
		   ((and (water 6) (water 3)) (+ mix-start 16))
		   ((and (water 4) (water 7)) (+ mix-start 0))
		   ((and (water 4) (water 1)) (+ mix-start 7))
		   

		   ((and (road 7) (road 8) (road 9) (road 6) (road 3)) (+ road-start 6))
		   ((and (road 7) (road 8) (road 9) (road 4) (road 1)) (+ road-start 5))
		   ((and (road 7) (road 4) (road 1) (road 2) (road 3)) (+ road-start 10))
		   ((and (road 9) (road 6) (road 1) (road 2) (road 3)) (+ road-start 11))
		   ((and (road 4) (road 1) (road 2) (road 3)) (+ road-start 4))
		   ;;((and (road 6) (road 4) (road 2)) (+ road-start 6))

		   ((and (road 1) (road 2) (road 3)) (+ road-start 3))
		   ((and (road 7) (road 8) (road 9)) (+ road-start 13))
		   ((and (road 1) (road 4) (road 7)) (+ road-start 9))
		   ((and (road 3) (road 6) (road 9)) (+ road-start 7))
		   
		   ((and (road 2) (road 4)) (+ road-start 4))
		   ((and (road 2) (road 6)) (+ road-start 2))
		   ((and (road 8) (road 4)) (+ road-start 14))
		   ((and (road 8) (road 6)) (+ road-start 12))

		   ((and (water 6) (perm-solid 8)) (+ mix-start 2))
		   
		   (t (+ 356 (random 4)))
		   ))

	    )
	     
	     
      ))
  )


(defun %fill-coordmap (coordmap table i j)

  (let ((alts '(("water" . water) ("grass" . grass)
		("bridge" . bridge) ("road" . road)
		("perm-solid" . perm-solid)
		("pier" . bridge))))

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
                      (setf (gethash idx used-indexes)
                            (make-instance 'floor-type :id (floor.id point) :flags (floor.flags point)
                                           :text-sym (text-sym point)
                                           :gfx-sym (tile-paint-value 41 idx)))
                      (setf found (gethash idx used-indexes)))
			    
                    (setf (coord.floor (aref table i j)) found)
                    )))
              ))
          ))
    dungeon))

(defun evo/register-levels! (var-obj)
  "registering the levels this variant will use."
  
  (register-level! var-obj "level"
		   :object-filter
		   #'(lambda (var-obj obj)
		       (let* ((table (get-otype-table var-obj "level"))
			      (id (slot-value obj 'id))
			      (obj-table (gobj-table.obj-table table)))
			 (multiple-value-bind (val f-p)
			     (gethash id obj-table)
			   (declare (ignore val))
			   (if f-p
			     (error "Object-id ~s already exists in system, obviously not a unique id."
				    id)
			     (setf (gethash id obj-table) obj))))

			 t))


  (register-level! var-obj "valley"
		   :monster-filter
		   #'(lambda (var-obj obj)
		       ;; all below 0
		       (when (> (slot-value obj 'power-lvl) 0)
			 (let* ((which-lvl "valley")
				(table (get-mtype-table var-obj which-lvl))
				(id (slot-value obj 'id))
				(mon-table (gobj-table.obj-table table)))
			   (multiple-value-bind (val f-p)
			       (gethash id mon-table)
			     (declare (ignore val))
			     (if f-p
				 (error "Monster-id ~s already exist for ~s, not unique id."
					id which-lvl)
				 (setf (gethash id mon-table) obj))))
			 t)))
  )

