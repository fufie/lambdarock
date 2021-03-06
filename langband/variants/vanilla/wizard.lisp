;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/wizard.lisp - wizard-commands
Copyright (c) 2002-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)


(defun van-obj-printer (file obj)
  (let ((var-obj *variant*))
    (print (lb-engine:get-loadable-form var-obj obj) file))
  (terpri file))


(defun print-key-table (table fname)
  "Prints a key-table to the given file."
  
  (with-open-file (s (pathname fname)
                     :direction :output 
                     :if-exists :supersede)
    (let ((collected nil))
      (maphash #'(lambda (k v)
		   (push (cons k v) collected))

	       table)
      ;; hackish
      (let ((key-ops (get-key-operations)))
	(dolist (i key-ops)
	  (dolist (j collected)
	    (when (eq (cdr i) (cdr j))
	      (setf (cdr j) (car i))))))
      
      (let ((sorted (sort (mapcar #'(lambda (k)
				      (format nil "key ~a -> ~a" (car k) (cdr k)))
				  collected)
			  #'string-lessp)))
	(dolist (i sorted)
	  (format s "~a~%" i))))))

(defun van-dump-monsters (out-file &key (monster-list nil) (var-obj *variant*) (action-fun #'van-obj-printer))

  (assert (functionp action-fun))
  
  (let ((mon-list (if monster-list
		       monster-list
		       (lb-engine::get-monster-list var-obj)))
	(*print-case* :downcase)
	(*print-right-margin* 120))
    
    (with-open-file (ffile (pathname out-file)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (pprint '(in-package :langband) ffile)
      (terpri ffile)

      (dolist (x mon-list)
	(funcall action-fun ffile x))
      
      (terpri ffile))))


(define-key-operation 'break-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(break)))

(define-key-operation 'deliver-damage
    #'(lambda (dungeon player)
        (declare (ignore dungeon))
        (modify-creature-state! player '<cut>      :add 20)
	(modify-creature-state! player '<stun>     :add 20)
	(modify-creature-state! player '<poisoned> :add 20)
	))

(define-key-operation 'summon
    #'(lambda (dungeon player)
	(let* ((summon (get-string-input "Monster to summon: "))
	       (mon (if summon (produce-active-monster *variant* summon))))
	  (when mon
	    (block mon-placement
	      (let ((px (location-x player))
		    (py (location-y player)))
		(flet ((put-mon (x y)
			 (when (cave-floor-bold? dungeon x y)
			   (place-single-monster! dungeon player mon x y nil)
			   (light-spot! dungeon x y)
			   (return-from mon-placement nil))))
		  (put-mon (1+ px) py)
		  (put-mon px (1+ py))
		  (put-mon (1+ px) (1+ py)))))
	    mon))))

(define-key-operation 'object-create
    #'(lambda (dungeon player)
	(let* ((summon (get-string-input "Object to create: "))
	       (mon (if summon (produce-active-object *variant* summon))))
	  (when mon
	    (block mon-placement
	      (let ((px (location-x player))
		    (py (location-y player)))
		(flet ((put-mon (x y)
			 (when (cave-floor-bold? dungeon x y)
			   (drop-near-location! *variant* dungeon mon x y)
			   (light-spot! dungeon x y)
			   (return-from mon-placement nil))))
		  (put-mon (1+ px) py)
		  (put-mon px (1+ py))
		  (put-mon (1+ px) (1+ py)))))
	    mon))))


(define-key-operation 'set-gold
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let* ((str-amount (get-string-input "Gold-amount: "))
	       (amount (ignore-errors (parse-integer str-amount))))
	  (when (and (integerp amount) (plusp amount))
	    (setf (player.gold player) amount)
	    (ask-for-redraw! player '[gold]))
	  )))

(define-key-operation 'go-to-depth
    #'(lambda (dungeon player)

	(let* ((which-depth (get-string-input "Depth: "))
	       (depth (ignore-errors (parse-integer which-depth))))
	  (when (and (integerp depth) (plusp depth))
	    (setf (player.depth player) depth
		  (dungeon.depth dungeon) depth)
		  
	    (when (> depth (player.max-depth player))
	      (setf (player.max-depth player) depth))

	    (setf (player.leaving? player) :teleport)
	    t))
	))

    

(define-key-operation 'print-odd-info
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((var-obj *variant*)
	      (fname (concatenate 'string lb-engine::*dumps-directory* "odd.info"))
	      (*print-case* :downcase))
	  (with-open-file (s (pathname fname)
			     :direction :output 
			     :if-exists :supersede
			     :if-does-not-exist :create)
	    (let ((effects (variant.effects var-obj)))
	      (dolist (i (reverse effects))
		(format s "~&Effect ~d: ~a - ~a" (effect.number i) (effect.name i) (effect.symbol i))))

	    (format s "~2%")
	    
	    (let ((elements (variant.elements var-obj)))
	      (dolist (i (reverse elements))
		(format s "~&Element ~d: ~a - ~a" (element.number i) (element.name i) (element.symbol i))))

	    (format s "~2%")
	    
	    (loop for i from 5 to 200 by 10
		  do
		  (format s "~&~a% chance to hit armour ~a (~a)~%"
			  (get-tohit-chance var-obj 80 i)
			  i
			  (get-armour-desc var-obj i)))
	    
	    )
	  (print-message! "Odd info was dumped to dumps/odd.info")
	  )))

(define-key-operation 'print-keys
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(print-key-table (gethash :global *angband-keys*)
			 "angband.keys")))

(define-key-operation 'dump-monsters
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((var-obj *variant*))
	  (van-dump-monsters (concatenate 'string lb-engine::*dumps-directory* "mon-by-id.list")
			     :monster-list (lb-engine::get-monster-list var-obj
									:predicate #'string<
									:sort-key #'get-id))
	  (van-dump-monsters (concatenate 'string lb-engine::*dumps-directory* "mon-by-id-short.list")
			     :monster-list (lb-engine::get-monster-list var-obj
									:predicate #'string<
									:sort-key #'get-id)
			     :action-fun #'(lambda (file obj)
					     (format file "~&~30a ~30a~%" (get-id obj)
						     (monster.power-lvl obj))))

	  (van-dump-monsters (concatenate 'string lb-engine::*dumps-directory* "mon-by-depth.list")
			     :monster-list (lb-engine::get-monster-list var-obj
									:predicate #'<
									:sort-key #'monster.power-lvl)
			     :action-fun #'(lambda (file obj)
					     (format file "~&~30a ~30a~%" (get-id obj)
						     (monster.power-lvl obj))))
	  )))

(define-key-operation 'dump-objects
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(lb-engine::dump-objects (concatenate 'string lb-engine::*dumps-directory* "obj.list"))
	))

(define-key-operation 'show-objects
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((objs (lb-engine::get-object-list))
	      (win (aref *windows* *map-frame*)))
	  (clear-window win)
	  (loop for i from 0 
		for obj in objs
		for row = (mod i (window.height win))
		do
		(progn
		  (output-string! win 0 row +term-white+ (format nil "~d" (object.numeric-id obj)))
		  (setf (window-coord win +foreground+ 4 row) (gfx-sym obj))
		  (setf (window-coord win +foreground+ 5 row) (text-sym obj))
		  (output-string! win 6 row +term-white+ (if (> (length (get-id obj)) 15)
							     (subseq (get-id obj) 0 14)
							     (get-id obj)))
		  ;;(warn "did object ~s" obj)
		  (loop for i from 0 below (window.width win)
			do
			(paint-coord win i row))
		  (when (= row (1- (window.height win)))
		    (read-one-character)
		    (clear-window win))
		  ))
	  (refresh-window win)
	  (pause-last-line!)
	  )))

(define-key-operation 'show-monsters
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((objs (lb-engine::get-monster-list *variant*))
	      (win (aref *windows* *map-frame*)))
	  (clear-window win)
	  (loop for i from 0 
		for obj in objs
		for row = (mod i (window.height win))
		do
		(progn
		  (output-string! win 0 row +term-white+ (format nil "~d" (monster.numeric-id obj)))
		  (setf (window-coord win +foreground+ 4 row) (gfx-sym obj))
		  (setf (window-coord win +foreground+ 5 row) (text-sym obj))
		  (output-string! win 6 row +term-white+ (if (> (length (get-id obj)) 15)
							     (subseq (get-id obj) 0 14)
							     (get-id obj)))
		  ;;(warn "did object ~s" obj)
		  (loop for i from 0 below (window.width win)
			do
			(paint-coord win i row))
		  (when (= row (1- (window.height win)))
		    (read-one-character)
		    (clear-window win))
		  ))
	  (refresh-window win)
	  (pause-last-line!)
	  )))

(define-key-operation 'show-format
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-dialogue ()
	  (let ((win (aref *windows* +dialogue-frame+))
		(colour +term-l-green+))
	    (clear-window win)

	  (win/format win 1 1 colour "~d" 50)
	  (win/format win 1 2 colour "~d" -50)
	  (win/format win 1 3 colour "~d" 5000)
	  (win/format win 1 4 colour "~d" -5000)
	  (win/format win 1 5 colour "~v" 10 5000)
	  (win/format win 1 6 colour "~v" 10 -5000)
	  ;;(win/format win 1 7 colour "~v" 15 5000)
	  ;;(win/format win 1 8 colour "~v" 15 -5000)

	  (win/format win 1 7 colour "~a" 'hei)
	  (win/format win 1 8 colour "~a" :hei)
	  
	  
	  (win/format win 1 9 colour "~v" 2 5000)
	  (win/format win 1 10 colour "~v" 2 -5000)

	  (win/format win 1 11 colour "~a" "hei")
	  (win/format win 1 12 colour "~a~a" "hei" "hei")
	  (win/format win 1 13 colour "~~")
	  (win/format win 1 14 colour "~a~~~a" "hei" "hei")

	  (win/format win 1 15 colour "~v,~v" 3 1 3 1)
	  (win/format win 1 16 colour "~v,~v" 3 12 3 12)
	  (win/format win 1 17 colour "~v,~v" 3 123 3 123)
	  
	  (refresh-window win)
	  (read-one-character)
	  ;;(clear-window win)
	  ))))


(define-key-operation 'dump-features
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(lb-engine::dump-floors (concatenate 'string lb-engine::*dumps-directory* "floors.list"))
	))

(define-key-operation 'inspect-coord
    #'(lambda (dungeon player)
	(let* ((cur-x (location-x player))
	       (cur-y (location-y player))
	       (coord-obj (cave-coord dungeon cur-x cur-y)))
	  (warn "Describing [~a,~a]" cur-x cur-y)
	  (describe coord-obj)
	  #||
	  (multiple-value-bind (the-attr the-char)
	      (map-info dungeon cur-x cur-y)
	    (warn "Mapped to (~s . ~s)" the-attr the-char))
	  ||#
	  )))

(define-key-operation 'in-game-test
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	;; temporary place
	#+xp-testing
	(do-a-test :in)
	#||
	  ;; for those times when things crash
	  (when-bind (func (get-late-bind-function 'lb-test '%loc-save-test))
	      (funcall func lb::*variant* :variant)
	      (funcall func lb::*level* :level)
	      (funcall func lb::*player* :player)))
	  ||#

	))
#||
(define-key-operation 'print-map
    #'(lambda (dungeon player)
	(declare (ignore player))
	(print-map-to-file dungeon "./map.ascii")
	(print-message! "Map printed to map.ascii.")
	))


(define-key-operation 'print-map-as-ppm
    #'(lambda (dungeon player)
	(declare (ignore player))
	(print-map-as-ppm dungeon "./map.ppm")
	(print-message! "Map printed to map.ppm.")
	))
||#
(define-key-operation 'gain-level
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let ((cur-level (player.power-lvl player)))

	  (cond ((<= (variant.max-charlevel *variant*) cur-level)
		 (modify-xp! player 50000)) ;; dummy val
		(t
		 (let* ((next-limit (aref (player.xp-table player) cur-level))
			(lacks (- next-limit (player.current-xp player))))
		   (modify-xp! player lacks))))
	  )))

(define-key-operation 'heal-player
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(setf (current-hp player) (maximum-hp player))
	(ask-for-redraw! player '[hp])
	))
(define-key-operation 'load-vanilla
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	;; enable some time else, not now!
	;;(compat-read-savefile& "vanilla.save")
	))

(define-key-operation 'send-spell
    #'(lambda (dungeon player)
	(declare (ignore dungeon))

	(let ((px (location-x player))
	      (py (location-y player))
	      (fire-effect (get-spell-effect '<fire>)))
	  
	  ;; let us do a fire-bolt
	  (let ((flag (logior +project-kill+ +project-stop+ +project-through+)))
	    (when-bind (dir (get-aim-direction))
	      (do-projection player (+ (aref *ddx* dir) px) (+ (aref *ddy* dir) py) flag
			     :effect fire-effect :damage 4)))
	  
	  ;; let us do a fire-beam
	  (let ((flag (logior +project-kill+ +project-beam+ +project-through+)))
	    (when-bind (dir (get-aim-direction))
	      (do-projection player (+ (aref *ddx* dir) px) (+ (aref *ddy* dir) py) flag
			     :effect fire-effect :damage 4)))
	  
	  ;; let us do a fire-ball
	  (let ((flag (logior +project-kill+ +project-stop+ +project-grid+ +project-item+)))
	    (when-bind (dir (get-aim-direction))
	      (do-projection player (+ px (* 99 (aref *ddx* dir))) (+ py (* 99 (aref *ddy* dir))) flag
			     :effect fire-effect :radius 4 :damage 4)))

	)

	))


(define-key-operation 'show-animtest
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	#||
	(let* ((win (aref *windows* *map-frame*))
	       (x 8)
	       (y 8))
	  ;;(push (lb-engine::get-walk-event win x y (+ x 5) y) *visevents*)
	  ;;(push (lb-engine::get-walk-event win x y (- x 5) y) *visevents*)
	  (push (lb-engine::get-walk-event win x y x (- y 4)) *visevents*)
	  (push (lb-engine::get-walk-event win x y x (+ y 4)) *visevents*)
	  (push (lb-engine::get-walk-event win x y (+ x 4) (- y 3)) *visevents*)
	  (push (lb-engine::get-walk-event win x y (- x 5) (- y 2)) *visevents*)
	  )
	||#
	))


(define-key-operation 'jump-to-test-level
    #'(lambda (dungeon player)
	(declare (ignorable dungeon player))
	(let ((wanted-level 45)
	      (depth 40))
	  (declare (ignorable depth wanted-level))
	;; get decent 
	(loop while (< (player.power-lvl player) wanted-level)
	      do
	      (modify-xp! player (+ 100 (player.maximum-xp player))))
	
	(heal-creature! player 7000)
	#||
	(when (and (integerp depth) (plusp depth))
	  (setf (player.depth player) depth
		(dungeon.depth dungeon) depth)
	  
	  (when (> depth (player.max-depth player))
	    (setf (player.max-depth player) depth))
	  
	  (setf (player.leaving? player) :teleport)
	  t)
	||#

	)))


(define-key-operation 'wizard-menu
    #'(lambda (dungeon player)

	(prog1
	    (block wizard-input 
	      (let ((loc-table (gethash :wizard *current-key-table*)))
		(loop
		 ;;	(clear-window *cur-win*)
		 ;;	(display-creature *variant* player)
		 ;;(print-message! nil)
		 (with-frame (+query-frame+)
		   (put-coloured-line! +term-white+ "Wizard command: " 0 0)
		   
		   (let* ((ch (read-one-character))
			  (fun (check-keypress loc-table ch)))
		     (cond ((and fun (functionp fun))
			    (return-from wizard-input (funcall fun dungeon player)))
			   ((eql ch +escape+)
			    (return-from wizard-input t))
			   (t
			    ;; nil
			    )))
		   ))))
	  (clear-window +query-frame+))
	
	))

;;(define-keypress *ang-keys* :wizard #\Y 'projecteur)

