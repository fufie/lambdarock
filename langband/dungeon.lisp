;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: dungeon.lisp - basic code for the dungeon
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)


(defun invoke-on-dungeon (dungeon fun)
  "calls given FUN with three arguments:
a COORD
the X-coordinate of the COORD
the Y-coordinate of the COORD
"
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table)) 
    (dotimes (y (dungeon.height dungeon))
      (dotimes (x (dungeon.width dungeon))
	(funcall fun (the dungeon-coord (aref table x y)) x y)))
    ))

(defmacro with-dungeon (parameters &body code)
  "a WITH- construct.  parameters should be: (dungeon-variable (arg-names to INVOKE))."

 `(invoke-on-dungeon ,(car parameters) #'(lambda ,(cadr parameters) ,@code))) 

(defun invoke-on-dungeon-monsters (dungeon fun)
  "Calls FUN with TWO arguments:
the dungeon object and the monster"

  (let ((monster-list (dungeon.monsters dungeon)))
    (dolist (i monster-list)
      (when (creature-alive? i)
	(funcall fun dungeon i)))))
	
(defmacro with-dungeon-monsters ((dungeon-var monster-var) &body code)
  `(invoke-on-dungeon-monsters ,dungeon-var
    #'(lambda (,dungeon-var ,monster-var) ,@code)))

(defun invoke-on-scatter-area (dungeon x y fun)
  "Invokes FUN on x y and on surrounding coordinates.
Calls FUN with THREE arguments: the dungeon, the new x, the new y"

  (let ((start-x x)
	(start-y y)
	(ddx-ddd *ddx-ddd*)
	(ddy-ddd *ddy-ddd*))
    (funcall fun dungeon start-x start-y)
    (dotimes (i +normal-direction-number+)
      (funcall fun dungeon (+ start-x (svref ddx-ddd i))
	       (+ start-y (svref ddy-ddd i))))))

(defmacro with-scatter-area (((dungeon-var x-var y-var) dungeon x y) &body code)
  `(invoke-on-scatter-area ,dungeon ,x ,y
    #'(lambda (,dungeon-var ,x-var ,y-var) ,@code)))

(defun legal-coord? (dungeon x y)
  (and (> x 0)
       (> y 0)
       (< x (dungeon.width dungeon))
       (< y (dungeon.height dungeon))))

;; hackish
(defun (setf coord-floor) (val coord)
  "this is an evil hack."

  (let* ((variant *variant*)
	 (ft (etypecase val
	       (integer (gethash val (variant.floor-types variant)))
	       (floor-type val)
	       (string (gethash val (variant.floor-types variant)))
	       ))
	 #||
	 (numeric-id (etypecase val
	 (integer val)
	 (floor-type (floor.numeric-id val))
	 (string (floor.numeric-id ft))
	 ))
	 ||#
	 )
		    
		     
    ;; fix me
    ;;(setf (coord.floor coord) numeric-id)
    (setf (coord.floor coord) ft)
  
    (if (bit-flag-set? (floor.flags ft) +floor-flag-wall+)
	;; this is a wall or a door..
	(bit-flag-add! (coord.flags coord) +cave-wall+)
	;; we're not a a wall or a door.. 
	(bit-flag-remove! (coord.flags coord) +cave-wall+)
	)
    ))

(defun clean-coord! (coord)
  "Cleans the given coordinate."
  
  (setf (coord-floor    coord) (get-floor-type "nothing")
	(coord.flags    coord) 0
	(coord.objects  coord) nil
	(coord.monsters coord) nil
	(coord.decor    coord) nil)
  coord)



(defun clean-dungeon! (dungeon)
  "Clears all flags and everything from the dungeon."

;;  (warn "!!! cleaning dungeon..")
  
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (clean-coord! coord))

  (setf (dungeon.rooms dungeon) nil
	(dungeon.monsters dungeon) nil
	(dungeon.triggers dungeon) nil)

  dungeon)


(defun create-dungeon (width height &key its-depth) 
  "Creates and returns a dungeon of specified size"

  (when (< +max-dungeon-width+ width)
    (error "To large width for dungeon"))
  (when (< +max-dungeon-height+ height)
    (error "To large height for dungeon"))
  
;;  (warn "Creating dungeon of size [~s ~s]" width height)
  
  (let ((d (make-dungeon :height height :width width)))

    (unless *dungeon-table*
      (let ((table (make-array (list +max-dungeon-width+ +max-dungeon-height+))))
	(dotimes (j +max-dungeon-width+)
	  (dotimes (i +max-dungeon-height+)
	    (setf (aref table j i) (make-dungeon-coord))))
	(setf *dungeon-table* table)))

    (when its-depth
      (setf (dungeon.depth d) its-depth))
    
    (setf (dungeon.table d) *dungeon-table*)

    (clean-dungeon! d)

    ;; we guess that 40 is a good starting value
    (setf (dungeon.action-queue d) (lb-ds:make-pq)) ;; maybe have size argument?
    
    d))




(defun make-map (dungeon)
  "Returns a map based on a given dungeon."
  (make-array (list (dungeon.width dungeon)
		    (dungeon.height dungeon))
	      :element-type 'fixnum :initial-element 0))

#||
Warning:
apparently ACL expands e.g (setf (cave-flags dungeon x y) foo)
with CAVE-fLAGS first as a macro, ending up with
(setf (coord ....) foo)
and not using the (SETF CAVE-FLAGS) function which does some
extra tricks.
||#

#+compiler-that-inlines
(defsubst cave-coord (dungeon x y)
  (declare (type fixnum x y))
;;  (assert (and (< x (dungeon.width dungeon))
  ;;	       (< y (dungeon.height dungeon))))
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (the dungeon-coord (aref table x y))))
  
#-compiler-that-inlines
(defmacro cave-coord (dungeon x y)
  (let ((table (gensym "table"))) 
    `(locally (declare (optimize (safety 0) (speed 3) (debug 0)
			#+sbcl (sb-ext:inhibit-warnings 3)
			#+cmu (ext:inhibit-warnings 3)))
      (let ((,table (dungeon.table ,dungeon)))
	(declare (type (simple-array t (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
	(the dungeon-coord (aref ,table ,x ,y))))))

;;#+compiler-that-inlines
(defun cave-floor (dungeon x y)
  (declare (type fixnum x y))
  (coord.floor (cave-coord dungeon x y)))

(defun cave-decor (dungeon x y)
  (declare (type fixnum x y))
  (coord.decor (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-floor (dungeon x y)
;;  `(coord.floor (cave-coord ,dungeon ,x ,y)))

;;#+compiler-that-inlines
(defun cave-flags (dungeon x y)
  (declare (type fixnum x y))
  (coord.flags (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-flags (dungeon x y)
;;  `(coord.flags (cave-coord ,dungeon ,x ,y)))

;;#+compiler-that-inlines
(defun cave-objects (dungeon x y)
  (declare (type fixnum x y))
  (coord.objects (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-objects (dungeon x y)
;;  `(coord.objects (cave-coord ,dungeon ,x ,y)))

;;#+compiler-that-inlines
(defun cave-monsters (dungeon x y)
  (declare (type fixnum x y))
  (coord.monsters (cave-coord dungeon x y)))

;;#-compiler-that-inlines
;;(defmacro cave-monsters (dungeon x y)
;;  `(coord.monsters (cave-coord ,dungeon ,x ,y)))

(defun (setf cave-monsters) (val dungeon x y)
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (declare (type fixnum x y))
  ;; add smart stuff later..
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (setf (coord.monsters (the dungeon-coord (aref table x y)))
	  (if (listp val) val (list val)))))


(defun (setf cave-objects) (val dungeon x y)
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (setf (coord.objects (the dungeon-coord (aref table x y))) val)))

(defun (setf cave-flags) (val dungeon x y)
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (setf (coord.flags (the dungeon-coord (aref table x y))) val)))

(defun (setf cave-decor) (val dungeon x y)
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (unless (or (eq val nil) (typep val 'decor))
    (warn "Assigning odd value ~s to ~s,~s as decor" val x y))
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    (setf (coord.decor (the dungeon-coord (aref table x y))) val)))


(defun (setf cave-floor) (val dungeon x y)
;;  (warn "Setting ~a ~a to ~a" x y val)
  ;;  (declare (type fixnum val))
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (let ((table (dungeon.table dungeon)))
    (declare (type (simple-array dungeon-coord (#.+max-dungeon-width+ #.+max-dungeon-height+)) table))
    
    (let ((coord (the dungeon-coord (aref table x y))))
      
      ;; hack
      (setf (coord-floor coord) val)
      
      (when (dungeon.active dungeon)
	(light-spot! dungeon x y)
	(note-spot! dungeon x y))
      
      
      ;; redraw
      )))


(defun fill-dungeon-with-floor! (dungeon floor)
  "Cleans and then fills the dungeon with a given floor.
Returns nothing."
  
;;  (warn "filling up dungeon with floor ~s.." floor)

;;  (check-type floor floor-type)
  
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (clean-coord! coord)
    (setf (coord-floor coord) floor))

  
  ;; check
  #+never
  (with-dungeon (dungeon (coord x y))
    (declare (ignore x y))
    (assert (and (= (coord.floor coord) floor)
		 ;;(= (coord.flags coord) 0)
		 (eq (coord.monsters coord) nil)
		 (eq (coord.objects coord) nil))))
  
  (values))

(defun fill-dungeon-part-with-floor! (dungeon floor width-range height-range)
  "height-range and width-range should be conses where
car is start and cdr is the non-included end  (ie [start, end> )"

  #-allegro
  (declare (type (cons fixnum fixnum) width-range height-range))
  
;;  (warn "Filling h: ~a and w: ~a" height-range width-range)
  
  (loop for i of-type fixnum from (car height-range)
	      below (the fixnum (cdr height-range))
	do
	(loop for j of-type fixnum from (car width-range)
	      below (the fixnum (cdr width-range))
	      do
	      (setf (cave-floor dungeon j i) floor)))
  (values))

(defun draw-to-map (dungeon x y tx ty)
  "WRONG DOC! Returns two values, attr and char, use M-V-B to get the values."
  (declare (type fixnum x y))
  ;; maybe get the coord in one go..
  (let* ((coord (cave-coord dungeon x y)) ;; faster
	 (feat (coord.floor coord))
	 (flags (coord.flags coord))
	 (decor (coord.decor coord))
	 (using-gfx (window-allows-gfx-tiles? *map-frame*))
	 (sym-fun (if using-gfx #'gfx-sym #'text-sym))
	 (win (aref *windows* *map-frame*))
	 (floor-sym 0)
	 (mid-sym 0)

	 ;; flip-dependents
	 #-flip-mode
	 (mon (coord.monsters coord))
	 #-flip-mode
	 (obj-table (coord.objects coord))
	 #-flip-mode
	 (pl-obj *player*)
	 #-flip-mode
	 (mon-sym 0)
	 
	 )

    (clear-coord win tx ty)
    
    ;; hackish, fix later
    #-flip-mode
    (when (consp mon)
      (setf mon (car mon)))

    ;; first of all we need to decide if we can see the floor at all
    (cond ((or (bit-flag-set? flags +cave-mark+)
	       (bit-flag-set? flags +cave-seen+))
	   (setf floor-sym (funcall sym-fun feat)))

	  ;; otherwise not seen
	  (t
	   (setf floor-sym (funcall sym-fun (get-floor-type "nothing")))
	   ))

    (when (plusp floor-sym)
      (setf (window-coord win +background+ tx ty) floor-sym))

    ;; this code should check if it has been seen
    ;; places with decor, but must be seen
    (when (and (or (bit-flag-set? flags +cave-mark+)
		   (bit-flag-set? flags +cave-seen+))
	       (typep decor 'decor) (decor.visible? decor))
      (setf mid-sym (funcall sym-fun decor)))


    ;; let's see if any objects are on top
    #-flip-mode
    (when (and obj-table (typep obj-table 'item-table))

      (let (;;(objs (items.objs obj-table))
	    (obj-len (items.cur-size obj-table))
	    (first-obj (item-table-find obj-table 0)))

	(cond ((= obj-len 0)
	       ;; clean it up now
	       (setf (coord.objects coord) nil))

	      ((and (> obj-len 1) (aobj.marked first-obj))
	       ;; bad hack
	       (setf mid-sym (if using-gfx
				 (tile-paint-value 10 54)
				 (text-paint-value +term-white+ #\&))))

	      ((aobj.marked first-obj)
	       (setf mid-sym (funcall sym-fun first-obj)))
	      
	      (t
	       ))
	))
    

    (cond ((plusp mid-sym)
	   (setf (window-coord win +decor+ tx ty) mid-sym)))


    #-flip-mode
    (progn
    ;; do we have monsters and can see it?
    
    (when (and mon (amon.seen-by-player? mon))
      (setf mon-sym (funcall sym-fun (amon.kind mon))))

    
    ;; remove this entry later..
    
    (when (and pl-obj ;; only when we have a player
	       (eql x (location-x pl-obj))
	       (eql y (location-y pl-obj)))
      (setf mon-sym (funcall sym-fun pl-obj)))

    
    (cond ((plusp mon-sym)
	   (setf (window-coord win +foreground+ tx ty) mon-sym)))
    

    ;; hack to bring a long target
    (when-bind (target (player.target pl-obj))
      (when (eq mon (target.obj target))      
	(display-target dungeon target)))
    )
    
    nil))

(defun draw-screen-obj (win obj vx vy sym-fun)
  "Don't use this one!"
  (let* ((loc-x (display-x obj))
	 (loc-y (display-y obj))
	 (screen-x 0)
	 (screen-y 0))

    (unless loc-x
      (setf loc-x (location-x obj)))
    (unless loc-y
      (setf loc-y (location-y obj)))

    (setf screen-x (- loc-x vx))
    (setf screen-y (- loc-y vy))
  
    ;; paint the earlier coordinate
    ;;(paint-coord win screen-x screen-y)

    ;; blit the tile
    (let* ((tile-wid (window.tile-width win))
	   (tile-hgt (window.tile-height win))
	   (the-x (+ (* screen-x tile-wid) (x-offset obj)))
	   (the-y (+ (* screen-y tile-hgt) (y-offset obj)))
	   )
      (org.langband.ffi:c-transparent-blit (window.num-id win)
					   the-x the-y
					   (funcall sym-fun obj) 0)
      #-flip-mode
      (org.langband.ffi:c-flush-coords! (window.num-id win)
					the-x the-y
					tile-wid tile-hgt)

      )))


(defun draw-creatures (win dungeon player)
  "Draws all creatures that are in the viewport."
  
  (when (integerp win)
    (setf win (aref *windows* win)))
  
  (let* ((vx (player.view-x player))
	 (vy (player.view-y player))
	 (max-vx (+ vx (window.width win)))
	 (max-vy (+ vy (window.height win)))
	 (using-gfx (window-allows-gfx-tiles? win))
	 (sym-fun (if using-gfx #'gfx-sym #'text-sym))
	 (loc-x 0)
	 (loc-y 0))

    ;; check which monsters are seen and in the viewport
    (dolist (mon (dungeon.monsters dungeon))
      (when (and (amon.seen-by-player? mon) (creature-alive? mon))
	(setf loc-x (display-x mon)
	      loc-y (display-y mon))
	(unless loc-x
	  (setf loc-x (location-x mon)))
	(unless loc-y
	  (setf loc-y (location-y mon)))
		
	(when (and (>= loc-x vx)
		   (>= loc-y vy)
		   (< loc-x max-vx)
		   (< loc-y max-vy))
	    ;; actual drawing
	    (draw-screen-obj win mon vx vy sym-fun)
	    )))

    ;; player is always in viewport I think
    (draw-screen-obj win player vx vy sym-fun)
    
    ))

(defun draw-active-objects (win dungeon player)
  "Draws all active objects that are in the viewport."
  
  (when (integerp win)
    (setf win (aref *windows* win)))
  
  (let* ((vx (player.view-x player))
	 (vy (player.view-y player))
	 (max-vx (+ vx (window.width win)))
	 (max-vy (+ vy (window.height win)))
	 (using-gfx (window-allows-gfx-tiles? win))
	 (sym-fun (if using-gfx #'gfx-sym #'text-sym))
	 (loc-x 0)
	 (loc-y 0))

    ;; check which monsters are seen and in the viewport
    (dolist (aobj (dungeon.objects dungeon))
      (when (aobj.marked aobj) ;; doesn't handle stacks
	(setf loc-x (display-x aobj)
	      loc-y (display-y aobj))
	(unless loc-x
	  (setf loc-x (location-x aobj)))
	(unless loc-y
	  (setf loc-y (location-y aobj)))
		
	(when (and (>= loc-x vx)
		   (>= loc-y vy)
		   (< loc-x max-vx)
		   (< loc-y max-vy))
	    ;; actual drawing
	    (draw-screen-obj win aobj vx vy sym-fun)
	    )))

    ))


(defun cave-is-room? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-room+))


(defun can-place? (dungeon x y type &optional allow-existing)
  (let* ((coord (cave-coord dungeon x y))
	 (fl-type (coord.floor coord)))

    (ecase type
      (:object (cond (allow-existing
		      (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-items+))
		     (t
		      (and (eq nil (coord.objects coord))
			   (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-items+)))))
      (:creature (and (eq nil (coord.monsters coord))
		      (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-creatures+)))
      (:trap (and (eq nil (coord.monsters coord))
		  (eq nil (coord.objects coord))
		  (eq nil (coord.decor coord))))
      (:stair (and (eq nil (coord.monsters coord))
		   (eq nil (coord.objects coord))
		   (eq nil (coord.decor coord))
		   (bit-flag-set? (floor.flags fl-type) +floor-flag-floor+)
		   (bit-flag-set? (floor.flags fl-type) +floor-flag-allow-items+)
		   ))
      
      )))
	  
       
       
    


;; just crap, use a better predicate
(defun cave-floor-bold? (dungeon x y)
  (declare (type fixnum x y))
  (not (bit-flag-set? (cave-flags dungeon x y)
		      +cave-wall+)))

;; just crap, use a better predicate
(defun cave-empty-bold? (dungeon x y)
  (and (cave-floor-bold? dungeon x y)
       (eq (cave-monsters dungeon x y) nil)))

(defun cave-icky? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-icky+))


(defun player-has-los-bold? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-view+))

(defun player-can-see-bold? (dungeon x y)
  (declare (type fixnum x y))
  (bit-flag-set? (cave-flags dungeon x y)
		 +cave-seen+))

(defun in-bounds? (dungeon x y)
  "Checks that the coordinate is well within the dungeon"
  (declare (type fixnum x y))
  ;; strict check, fails some places
;;  (legal-coord? dungeon x y)
  ;; liberal check
  (and (>= y 0)
       (>= x 0)
       (< y (dungeon.height dungeon))
       (< x (dungeon.width dungeon)))
  )



(defun in-bounds-fully? (dungeon x y)
  "Checks that the coordinate is well within the dungeon"
  (declare (type fixnum x y))
  (and (> x 0)
       (> y 0)
       (< x (1- (dungeon.width dungeon)))
       (< y (1- (dungeon.height dungeon)))))


(defun is-closed-door? (dungeon x y)
  (when-bind (decor (cave-decor dungeon x y))
    (when (and (typep decor 'active-door)
	       (decor.visible? decor))
      (door.closed? decor))))


;; hackish.. not very good
(defun is-open-door? (dungeon x y)
  (when-bind (decor (cave-decor dungeon x y))
    (when (and (typep decor 'active-door)
	       (decor.visible? decor)) 
      (not (door.closed? decor)))))


(defun viewport-contains? (player x y)
  "Returns T if the viewport contains the coordinate."
  (let ((x-off (- x (player.view-x player)))
	(y-off (- y (player.view-y player))))
	
    (and (>= x-off 0)
	 (>= y-off 0)
	 (< x-off (get-frame-width *map-frame*))
	 (< y-off (get-frame-height *map-frame*)))))


(defun place-player! (dungeon player x y)
  (declare (ignore dungeon))
  (declare (type fixnum x y))
  ;; fix me later
  (setf (location-y player) y
	(location-x player) x)

  player)


(defun print-map (dungeon player frame)
  "Prints a map of the given dungeon to the wanted frame"
;;  (declare (optimize (safety 0) (speed 3) (debug 0)
;;		     #+cmu (ext:inhibit-warnings 3)))


  (let* (;;(*player* player)
	 (wy (player.view-y player))
	 (wx (player.view-x player))
	 (ty (+ wy (get-frame-height frame)))
	 (tx (+ wx (get-frame-width frame)))
	 )

    (declare (type fixnum ty tx))

;;    (warn "printing map (~s ~s)" (location-x player) (location-y player))
    
    (assert (eq player *player*))
    
    (loop for vy of-type fixnum from 0
	  for y of-type fixnum from wy below ty
	  do
	  
	  (loop for vx of-type fixnum from 0
		for x of-type fixnum from wx below tx
		do
		(when (in-bounds? dungeon x y)
		  (draw-to-map dungeon x y vx vy))
		))
    
    (paint-window frame) ;; this should maybe be moved elsewhere, but it's here now
    ;;(warn "printed map")
    ;;(draw-creatures *map-frame* dungeon player)
	  
    ))
 

(defun adjust-viewport! (dungeon player vx vy)
  "maybe let variants override this one to avoid the town-hack below?"
  
  (let ((town-width 66)
	(town-height 22) ;; hacks
	(pl-depth (dungeon.depth dungeon))
	(term-height (get-frame-height *map-frame*))
	(term-width (get-frame-width *map-frame*))
	(dun-height (dungeon.height dungeon))
	(dun-width (dungeon.width dungeon)))

    ;; hack
    (incf pl-depth)
    
    (cond ((= pl-depth 0)
	   (cond ((> vy (- town-height term-height)) ;; hack
		  (setf vy (- town-height term-height)))
		 ((minusp vy)
		  (setf vy 0))))
	  ((> vy (- dun-height term-height))
	   (setf vy (- dun-height term-height))))
  
    (cond ((= pl-depth 0)
	   (cond ((> vx (- town-width term-width)) ;; hack
		  (setf vx (- town-width term-width)))
		 ((minusp vx)
		  (setf vx 0))))
	
	  ((> vx (- dun-width term-width))
	   (setf vx (- dun-width term-width))))

    (when (minusp vy)
      (setf vy 0))

    (when (minusp vx)
      (setf vx 0))

    (when (or (/= (player.view-x player) vx)
	      (/= (player.view-y player) vy))
      ;;    (warn "modify panel to ~s ~s" vx vy)
      (setf (player.view-x player) vx
	    (player.view-y player) vy)
      (ask-for-redraw! player '[map])
      ;; skip window

      t)
    ))


(defun verify-viewport (dungeon player)
  "verifies that the viewport is correct and scrolls as needed
to have the player in there."

  (let ((p-y (location-y player))
	(p-x (location-x player))
	(v-y (player.view-y player))
	(v-x (player.view-x player))
	(term-height (get-frame-height *map-frame*))
	(term-width (get-frame-width *map-frame*))
	;;(centre-on-player nil)
	;;(centre-on-player t)
	)

    ;; FIX!!! This code is _very_ broken!
    
    ;; hackish, update when running arrives
    (cond #+centre-on-player
	  ((/= p-y (+ v-y (int-/ term-height 2)))
	   (setf v-y (- p-y (int-/ term-height 2))))
	  
	  ((or (< p-y (+ v-y 3))
	       (>= p-y (+ v-y term-height -3)))
	   (setf v-y (- p-y (int-/ term-height 3)))))
    ;;* (int-/ (- p-y (int-/ panel-height 2))
;;			       panel-height)
;;			panel-height))))

    (cond #+centre-on-player
	  ((/= p-x (+ v-x (int-/ term-width 2)))
	   (setf v-x (- p-x (int-/ term-width 2))))
	  
	  ((or (< p-x (+ v-x 3))
	       (>= p-x (+ v-x term-width -3)))
	   (setf v-x (- p-x (int-/ term-width 3)))))
;;	   (setf v-x (* (int-/ (- p-x (int-/ panel-width 2))
;;			       panel-width)
;;			panel-width))))

    (adjust-viewport! dungeon player v-x v-y)))



(defun distance (x1 y1 x2 y2)
  "returns a fixnum"
  (declare   (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3))
	     (type fixnum x1 x2 y1 y2))
;;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  
  (let ((ay (if (> y1 y2)
		(the fixnum (- y1 y2))
		(the fixnum (- y2 y1))))
	(ax (if (> x1 x2)
		(the fixnum (- x1 x2))
		(the fixnum (- x2 x1)))))
    
    (declare (type fixnum ax ay))
    
    (if (> ay ax)
	(the fixnum (+ ay (the fixnum (int-/ ax 2))))
	(the fixnum (+ ax (the fixnum (int-/ ay 2)))))
    ))
	     
  

(defun note-spot! (dungeon x y)
  "noting the spot."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3))
	   (type fixnum x y))

  (let* ((coord (cave-coord dungeon x y))
	 (flag (coord.flags coord)))

    ;; require it to be seen
    (when (bit-flag-set? flag +cave-seen+)

      (when-bind (objs (cave-objects dungeon x y))
	(dolist (i (items.objs objs))
	  (setf (aobj.marked i) t)))
      
      (unless (bit-flag-set? flag +cave-mark+)
	(let* (;;(decor (coord.decor coord))
	       (ft (coord.floor coord))
	       (floor-flag (floor.flags ft)))
	  (declare (type fixnum floor-flag))
	  (cond ((bit-flag-set? floor-flag +floor-flag-floor+)
		 (when (bit-flag-set? flag +cave-glow+)
		   (bit-flag-add! (coord.flags coord) +cave-mark+)))
		 
		(t
		 (bit-flag-add! (coord.flags coord) +cave-mark+)))))
  
      )))

(defun light-spot! (dungeon x y)
  "lighting up the spot.."
  (declare (type fixnum x y))

  (let ((player *player*))
    
    (unless (and player dungeon *dungeon*) ;; hackish!
      (return-from light-spot! nil))

    (let* ((pvx (player.view-x player))
	   (pvy (player.view-y player))
	   (kx (- x pvx))
	   (ky (- y pvy))
	   (win (aref *windows* *map-frame*)))
      
      (declare (type fixnum pvx pvy kx ky))

      ;; sometimes we cross a screen
      (when (and (<= 0 kx)
		 (<= 0 ky)
		 (< ky (get-frame-height *map-frame*))
		 (< kx (get-frame-width  *map-frame*)))

	(draw-to-map dungeon x y kx ky)
	;; maybe add paint here too
	(paint-coord win kx ky)
	;;(queue-map-char! kx ky the-attr the-char trans-attr trans-char)
	(setf (window.repaint? win) t)
      ))
  ))



(defun get-coord-trigger (dungeon x y)
  "Tries to find a trigger at a given point."
;;  (warn "looking for trigger at ~d,~d -> ~s" x y (dungeon.triggers dungeon))
  (dolist (i (dungeon.triggers dungeon))
    (let ((place (car i)))
      (when (and (= (car place) x)
		 (= (cdr place) y))
	(return-from get-coord-trigger i))))
  nil)


(defun (setf get-coord-trigger) (val dungeon x y)
  "Adds a trigger to the given dungeon."

;;  (warn "Adding ~s at ~d,~d" val x y)
  (block setf-coord-trigger
    (dolist (i (dungeon.triggers dungeon))
      (let ((place (car i)))
	(when (and (= (car place) x)
		   (= (cdr place) y))
	  (setf (cdr i) val)
	  (return-from setf-coord-trigger i))))
    
    (push (cons (cons x y) val) (dungeon.triggers dungeon))))


(defun apply-possible-coord-trigger (dungeon x y)
  "This is a hack.. fix me later.."
  (declare (type fixnum x y))

  (when-bind (decor (cave-decor dungeon x y))
    (when-bind (events (decor.events decor))
      (dolist (i events)
	(when (typep i 'l-event)
	  (funcall (event.function i) decor dungeon x y)
	  (return-from apply-possible-coord-trigger t)))))

    
  (let ((trigger (get-coord-trigger dungeon x y)))
    (when (and trigger (consp trigger) (is-event? (cdr trigger)))
      (let ((the-event (cdr trigger)))
	(apply (event.function the-event) dungeon x y (event.state the-event)))
      )))

(defun get-relative-coords (x y)
  "Returns relative x and relative y, or NIL if they're not visible on map."
  (let* ((player *player*)
	 (pwy (player.view-y player)) 
	 (ky (- y pwy))
	 (pwx (player.view-x player))
	 (kx (- x pwx)))
    
    (declare (type fixnum pwx pwy kx ky))

    (values (if (and (<= 0 kx) (< kx (get-frame-width *map-frame*))) kx nil)
	    (if (and (<= 0 ky) (< ky (get-frame-height *map-frame*))) ky nil))))


(defun put-cursor-relative! (dungeon x y &optional (cursor :map-cursor))
  "Tries to put the cursor relative to the window."
  (declare (ignore dungeon))
  (declare (type fixnum x y))

  (let* ((player *player*)
	 (pwy (player.view-y player)) 
	 (ky (- y pwy))
	 (pwx (player.view-x player))
	 (kx (- x pwx)))
    
    (declare (type fixnum pwx pwy kx ky))
    
    (when (and (<= 0 kx)
               (<= 0 ky)
               (< ky (get-frame-height *map-frame*))
               (< kx (get-frame-width *map-frame*)))

      (set-cursor-to *map-frame* cursor kx ky))))


(defun remove-item-from-dungeon! (dungeon item)
;;  (lang-warn "Removing item ~s from dungeon" item)
  (setf (dungeon.objects dungeon) (delete item (dungeon.objects dungeon))))

(defun scatter-location (dungeon base-x base-y radius)
  "Tries to find a random location within a certain radius from
base-x,base-y and returns it as (values x y)"

  (unless (in-bounds-fully? dungeon base-x base-y)
    (error "base-x ~s, base-y ~s to scatter not within bounds." base-x base-y))
  (unless (positive-integer? radius)
    (error "Radius ~s scatter too small" radius))
  
  (loop
   (let ((nx (rand-spread base-x radius))
	 (ny (rand-spread base-y radius)))

     (when (and (in-bounds-fully? dungeon nx ny)
		(<= (distance base-x base-y nx ny) radius)
		t) ;; check line of sight
       (return-from scatter-location (values nx ny)))
     )))

;;; ---- room stuff ----

(defun add-room-to-dungeon! (dungeon room)
  (push room (dungeon.rooms dungeon)))


;; alters the dungeon object.
(defmethod activate-object :around ((obj level) &key)
   (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find ACTIVATE-OBJECT for type ~a" (type-of obj))
     (return-from activate-object nil))

   ;; we pass along the same arguments.. 
   (let* ((result (call-next-method))
	  (dungeon (level.dungeon result)))
     (cond ((and dungeon (typep dungeon 'dungeon))
	    (setf (dungeon.active dungeon) t)
	    result)
	   (t
	    (lang-warn "Activation of object ~a failed, return was ~a" obj result)
	    nil))
     ))

;; hack
(defmacro room-constructor (arguments &body body)
  (assert (= (length arguments) 2))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       (block room-constructor
		 ,@body))))
    `(function ,def)))
  
;; hack
(defmacro room-builder (arguments &body body)
  (assert (= (length arguments) 5))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       (block room-builder
		 ,@body))))
    `(function ,def)))


(defun define-room (id room-class name &key (inherits 'room-type) constructor builder (min-level 1) size-mod)
  "First argument should be an integer.. fix this later.."

  (when (eq id nil)
    (signal-condition 'illegal-room-data :id id :desc "id for room is NIL, it should be a string."))

  (when (symbolp id)
    (setf id (symbol-name id)))
  
  (unless (verify-id id)
    (signal-condition 'illegal-room-data :id id :desc "id for room didn't verify, please use a valid string."))

  ;; fix this, should define class here
  (cond ((eq room-class nil) (setf room-class 'room-type)) ;; hack
	((and (symbolp room-class) (find-class room-class))) ;; ok, it exists
	(t
	 (signal-condition 'illegal-room-data :id id :desc "room-class should be a symbol naming the room class.")))

  (unless (and (stringp name) (plusp (length name)))
    (signal-condition 'illegal-room-data :id id :desc "name of a room should be a string of length >= 1."))


  (cond ((eq inherits nil)
	 (setf inherits 'room-type))
	((symbolp inherits)
	 (unless (find-class inherits)
	   (signal-condition 'illegal-room-data :id id :desc "INHERITS argument is not naming a known class.")))
	((typep inherits 'class)) ;; ok
	(t
	 (signal-condition 'illegal-room-data :id id :desc "INHERITS argument is not naming a class.")))

  (cond ((non-negative-integer? min-level)) ;; ok
	((integerp min-level)
	 (signal-condition 'illegal-room-data :id id :desc "MIN-LEVEL arg to DEFINE-ROOM must be a positive integer."))
	(t
	 (signal-condition 'illegal-room-data :id id :desc "MIN-LEVEL arg to DEFINE-ROOM is not a positive integer.")))
  
  (cond ((eq size-mod nil)
	 (setf size-mod #(0 0 0 0)))
	((and (arrayp size-mod)
	      (= (length (array-dimensions size-mod)) 1)
	      (= 4 (length size-mod))
	      (every #'integerp size-mod))) ;; ok
	((arrayp size-mod)
	 (signal-condition 'illegal-room-data :id id
			   :desc "SIZE-MOD is not a SIMPLE-ARRAY of length 4 filled with integers."))
	(t
	 (signal-condition 'illegal-room-data :id id
			   :desc "SIZE-MOD to DEFINE-ROOM must be a SIMPLE-ARRAY of length 4 filled with integers.")))

  (cond ((functionp builder))
	(t
	 (signal-condition 'illegal-room-data :id id :desc "BUILDER arg to DEFINE-ROOM must be a function.")))
  
  (cond ((eq constructor nil)
	 (setf constructor #'(lambda (id)
			       (make-instance room-class :id id
					      :name name
					      :type room-class
					      :parent inherits
					      :constructor nil
					      :builder builder
					      :min-level min-level
					      :size-mod size-mod))))
	 ((functionp constructor)) ;; ok
	 (t
	  (signal-condition 'illegal-room-data :id id :desc "CONSTRUCTOR arg to DEFINE-ROOM is not NIL or function.")))

  
  (assert (functionp constructor))
  (assert (functionp builder))

    
  (let ((table (variant.room-types *variant*))
	(room-obj (make-instance 'room-type :id id
				 :name name
				 :type room-class
				 :parent nil
				 :constructor constructor
				 :builder builder
				 :min-level min-level
				 :size-mod size-mod)))

    (setf (gethash id table) room-obj)
    
    room-obj))


(defun get-room (id)
  "Returns the constructor to build the given room, or NIL."

  (assert (or (stringp id) (symbolp id)))
  
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.room-types *variant*)))
    (gethash key table)))

(defmethod find-appropriate-room (variant level player)
  (declare (ignore variant level player))
  (error "find-appropriate-room not implemented."))

(defun construct-room! (room-type dungeon player bx0 by0)
  "Constructs and returns an active-room."
  
  ;;  (declare (ignore player))
  ;;  (warn "Build room ~a ~a" by0 bx0)

  (assert (and (>= bx0 0) (>= by0 0) (< bx0 18) (< by0 6)))

  (let ((returned-room nil))
  
    (block room-construction
      
      (let* (;;(room-builder (get-room-builder num))
	     (room-info (room-type.size-mod room-type))
	     (room-map (dun-data.room-map *cur-dun*))
	     (by1 (+ by0 (svref room-info 0)))
	     (by2 (+ by0 (svref room-info 1)))
	     (bx1 (+ bx0 (svref room-info 2)))
	     (bx2 (+ bx0 (svref room-info 3))))
	
	(when (or (< by1 0)
		  (< bx1 0)
		  (>= by2 (dun-data.row-rooms *cur-dun*))
		  (>= bx2 (dun-data.col-rooms *cur-dun*)))
	  (warn "off the screen...")
	  (return-from room-construction nil))
	
	;; verify open space
	(loop for i from by1 to by2
	      do
	      (loop for j from bx1 to bx2
		    do
		    (when (aref room-map j i)
		      (return-from room-construction nil))))

    
	(let (;;(fun (cdr room-builder))
	      (y (int-/ (* (+ by1 by2 1) +block-height+) 2))
	      (x (int-/ (* (+ bx1 bx2 1) +block-width+) 2)))

	  ;; call the room-builder
	  (funcall (room-type.builder room-type)
		   room-type dungeon player x y)

	  (let ((aroom (make-instance 'active-room :type room-type
				      :loc-x x
				      :loc-y y)))
	    (add-room-to-dungeon! dungeon aroom)
	    (setq returned-room aroom))


	  (push (cons x y) (dun-data.room-centres *cur-dun*))

	  ;; reserve space in the room map
      
	  (loop for i from by1 to by2
		do
		(loop for j from bx1 to bx2
		      do
		      (setf (aref room-map j i) t)))

	  ;; skip crowd


	  returned-room)))))

;; hack, move me away later
(defun %room-has-light? (room dungeon &optional (chance 25))
  "Checks if a given room should have initial light."
  (declare (ignore room))
  (<= (dungeon.depth dungeon) (randint chance)))




;;; ---- some level-related stuff ----
 
;; a simple builder, register it in your variant as 'random-level
(defun make-random-level-obj ()
  (make-instance 'random-level :depth 0 :rating 0))

(defmethod level-ready? ((level random-level))
  (when (level.dungeon level)
    t))


(defmethod register-level! ((var-obj variant) (id string) &key object-filter monster-filter &allow-other-keys)
;;  (assert (not (eq nil var-obj)))
;;  (assert (symbolp id))
;;  #+langband-extra-checks
;;  (assert (ok-object? var-obj))

  (let ((mon-table (make-game-obj-table))
	(obj-table (make-game-obj-table)))
    
    (setf (gobj-table.obj-table mon-table) (make-hash-table :test #'equal)
	  (gobj-table.obj-table obj-table) (make-hash-table :test #'equal))

    (setf (gethash id (variant.monsters-by-level var-obj)) mon-table
	  (gethash id (variant.objects-by-level var-obj))  obj-table)

    ;; fix
    (when object-filter
      (if (not (functionp object-filter))
	  (lang-warn "Object-filter ~s for level ~s is not a function." object-filter id)
	  (pushnew (cons id object-filter)
		   (gethash :objects (variant.filters var-obj))
		   :key #'car)))
    
    (when monster-filter
      (if (not (functionp monster-filter))
	  (lang-warn "Monster-filter ~s for level ~s is not a function." monster-filter id)
	  (pushnew (cons id monster-filter)
		   (gethash :monsters (variant.filters var-obj))
		   :key #'car)))
    
    ))

;;; ---- read a custom-designed map as a dungeon ---

(defun read-map (variant fname)
  "This is probably only used by contraband atm.. good idea, maybe not
ready for full use in the engine yet."
  (let ((map-data '()))
    (with-open-file (s (pathname fname)
		       :direction :input)
      (loop for x = (read s nil 'eof)
	    until (eq x 'eof)
	    do
	    (push x map-data)))
    
    (let ((legal-syms (make-hash-table :test #'equal))
	  (height -1)
	  (width -1)
	  (dummy-cnt 400)
	  (dun-rows '()))
      
      (dolist (i map-data)
	
	(ecase (car i)
	  (define-map-symbol
	      (destructuring-bind (symbol id name &key text-sym gfx-sym (flags 0))
		  (cdr i)
		(let ((ft (make-instance 'floor-type :id id :name name
					 :flags (if (nonboolsym? flags)
						    (eval flags)
						    flags)
					 :numeric-id (incf dummy-cnt))))

		  (let ((val (eval text-sym)))
		    (cond ((non-negative-integer? val)
			 (setf (text-sym ft) val))
			  (t
			   (error "Unknown text-sym value ~s for map-sym ~s" val id))))
		  
		  (let ((val (eval gfx-sym)))
		    (cond ((non-negative-integer? val)
			   (setf (gfx-sym ft) val))
			  (t
			   (error "Unknown gfx-sym value ~s for map-sym ~s" val id))))
		  
		  (setf (gethash symbol legal-syms) ft))))
	  (map
	   (with-input-from-string (str (second i))
	     (loop for x = (read-line str nil 'eof)
		   until (eq x 'eof)
		   do
		   (incf height)
		   (setf width (length x))
		   (when (plusp (length x))
		     (push x dun-rows))
		   ))
	   )))

      (let ((real-table (variant.floor-types variant)))
	(loop for v being the hash-values of legal-syms
	      do
	      (setf (gethash (floor.numeric-id v) real-table) v)
	      (setf (gethash (floor.id v) real-table) v)))
	      
      (warn "H ~s W ~s" height width)
      
      (let ((dungeon (create-dungeon width height)))
	(loop for i from 0
	      for row in (nreverse dun-rows)
	      do
	      (loop for j from 0
		    for x across row
		    do
		    (let ((floor (gethash x legal-syms)))
		      ;;(when (eql x #\,)
			;;(warn "floor is ~s" floor))
		      (cond (floor
			     (setf (cave-floor dungeon j i) floor))
			    (t
			     (warn "Could not find ~s" x)))
		      )))

      
	dungeon))))

