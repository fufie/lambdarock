;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: ai.lisp - the game's "intelligence"
Copyright (c) 2002-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defclass ai-strategy ()
  ((id :initarg :id :initform nil :accessor strategy.id)
   (key :initarg :key :initform nil :accessor strategy.key)
   (weights :initform '() :accessor strategy.weights)
   ))

(defun define-strategy (id key constructor-info)
  (let ((var-obj *variant*))
    (unless var-obj
      (error "No variant object when defining a strategy: ~a" id))
    (unless constructor-info
      (error "No constructor-info for strategy: ~a" id))

    (cond ((consp constructor-info)
	   (setf (gethash key (variant.strategies var-obj))
		 #'(lambda () (apply 'make-instance (first constructor-info)
				     :id id :key key (cdr constructor-info)))))
	  
	  ((functionp constructor-info)
	   (setf (gethash key (variant.strategies var-obj)) constructor-info))
	  (t
	   (warn "Fell through define-strategy for ~a" id)))
    
    key))

(defun get-strategy-constructor (var-obj key)
  (multiple-value-bind (value present-p)
      (gethash key (variant.strategies var-obj))
    (unless present-p
      (warn "Unable to find strategy for key: ~a" key))
    value))
 

(defclass primitive-melee-attacker (ai-strategy)
  ((id :initform "primitive-melee-attacker")))

(defclass peaceful-mover (ai-strategy)
  ((id :initform "peaceful-mover")
   (destinations :initform '() :accessor strategy.destinations
		 :documentation "destinations is a list of (x y fun) lists, where
x is the x coord, y is the y coord and fun is an otional trigger function that's called
with (player dungeon monster strategy).  When a coordinate has been reached the list for
the coordinate is removed.")))

(defclass tactic-factors ()
  ((offensive :accessor factor.offensive :initform 0)
   (defensive :accessor factor.defensive :initform 0)
   (cost      :accessor factor.cost      :initform 0)
   ))


(defun get-move-direction (src-x src-y dest-x dest-y)
  "Returns an array of five elements with directions in preferred
order to get from monster to player."
  ;; the graph of moves.
  ;; 88AAECC
  ;; 8     C
  ;; 9     B
  ;; 1  M  5
  ;; 1     5
  ;; 0     4
  ;; 0022644

  ;; ignore invisibility, etc
  (let* ((px dest-x)
	 (py dest-y)
	 (mx src-x)
	 (my src-y)
	 (dx (- mx px))
	 (dy (- my py))
	 (lx (abs dx))
	 (ly (abs dy))
	 (move-value 0)
	 )

    (when (< dy 0) (incf move-value 8))  ;; above is +8
    (when (> dx 0) (incf move-value 4))  ;; right is +4

    (cond ((> ly (* lx 2))
	   (incf move-value 2))  ;; vertical centre
	  ((> lx (* ly 2))
	   (incf move-value 1))) ;; horizontal centre

    (cond ((= move-value 0)
	   (if (> ly lx) #(9 8 6 7 3) #(9 6 8 3 7)))
	  ((or (= move-value 1) (= move-value 9))  
	   (if (< dy 0)  #(6 3 9 2 8) #(6 9 3 8 2)))
	  ((or (= move-value 2) (= move-value 6))  
	   (if (< dx 0)  #(8 9 7 6 4) #(8 7 9 4 6)))
	  ((= move-value 4)
	   (if (> ly lx) #(7 8 4 9 1) #(7 4 8 1 9)))
	  ((or (= move-value 5) (= move-value 13))  
	   (if (< dy 0)  #(4 1 7 2 8) #(4 7 1 8 2)))
	  ((= move-value 8)
	   (if (> ly lx) #(3 2 6 1 9) #(3 6 2 9 1)))
	  ((or (= move-value 10) (= move-value 14))  
	   (if (< dx 0)  #(2 3 1 6 4) #(2 1 3 4 6)))
	  (t
	   (if (> ly lx) #(1 2 4 3 7) #(1 4 2 7 3))))
    ))

(defun update-sleep-status (dungeon player mon)
  "If the monster is sleeping this code checks effects and if
the monster should wake up."
  (declare (ignorable dungeon))
  (let ((calc-attrs (player.calc-attrs player)))
    (when-bind (aggravate-radius (get-attribute-value '<aggravates>
						      calc-attrs))
      (when (and (positive-integer? aggravate-radius)
		 (< (distance (location-x player) (location-y player)
			      (location-x mon) (location-y mon))
		    aggravate-radius))
	;; if aggravated, wake up!
	(modify-creature-state! mon '<sleeping> :new-value 0)
	(format-message! "~@(~A~) wakes up, annoyed!"
			 (get-creature-desc mon #x00))
	(return-from update-sleep-status t))))

  
  ;; we do a hack here
  (cond ((< (random 100) 20)
	 ;;(warn "Wake up ~s" mon)
	 (modify-creature-state! mon '<sleeping> :new-value 0) ;; awakened
	 (format-message! "~@(~A~) wakes up."
			  (get-creature-desc mon #x00)))
	
	(t ;; still sleeping
	 ;;(warn "~s is sleeping, bastard" mon)
	 
	 (return-from update-sleep-status t)))

  t)

(defmethod is-breeder? ((creature active-monster))
  (has-ability? creature '<breeder>)
  ;;(has-ability? creature '<random-mover>) ;; when testing, more frequent
  ) 

(defmethod breeders-in-dungeon ((dungeon dungeon))
  (let ((count 0))
    (with-dungeon-monsters (dungeon mon)
      (declare (ignore dungeon))
      (when (is-breeder? mon) (incf count)))
    count))

(defmethod process-single-monster! ((variant variant) dungeon player mon)
  "Tries to process a single monster and make it do nasty stuff."
  
  (declare (ignorable player))
  
  (let (#|(temp-attrs (amon.temp-attrs mon))|#)

    (block process-monster
      
      ;;; first check sleep
      (when (is-sleeping? variant mon)
	(update-sleep-status dungeon player mon))

      (when (is-sleeping? variant mon)
	;; if we're still sleeping, return
	(return-from process-monster t))
      
      
      ;;; check stun
      
      ;;; check confusion
      
      ;;; check fear

      ;;; check breeding
      (when (and (is-breeder? mon)
		 (possible-breed! variant dungeon player mon)) ;; side-effect
	;; possible msg
	(format-message! "~@(~a~) breeds explosively." (get-creature-desc mon #x00))
	(return-from process-monster t))
	
    ;;; check spell

    ;;; check confuse

    ;;(when-bind (strat (amon.strategies mon))
    ;;  (warn "Strategies are ~s" strat))
      (loop for i in (amon.strategies mon)
	    do
	    (let ((attempt (execute-strategy i mon dungeon :force nil)))
	      (when attempt
		(return-from process-monster t))))

      (loop for i in (amon.strategies mon)
	    do
	    (let ((attempt (execute-strategy i mon dungeon :force t)))
	      (when attempt
		(return-from process-monster t))))
      
      ;; then do nothing
      t)))

(defconstant +monster-breed-slowdown+ 8)
(defconstant +monster-max-breeders+ 40) ;; 100

(defun possible-breed! (variant dungeon player mon)
  "Returns nil when no breeding, returns T if breeding happened.
check density of monsters around it and has random chance of
breeding depending on density."

  (when (> (breeders-in-dungeon dungeon) +monster-max-breeders+)
    ;;(warn "MAX breeders, don't breed.")
    (return-from possible-breed! nil))
  
  (let ((loc-x (location-x mon))
	(loc-y (location-y mon))
	(count 0)) ;; count monsters
    
    (with-scatter-area ((dung x y) dungeon loc-x loc-y)
      (when (cave-monsters dung x y)
	(incf count)))

    ;; count includes itself, not player.
    (decf count)

    ;; (when (>= count 4) (warn "~s FAILED to breed.." (get-creature-name mon)))
    
    (when (or (= count 0) ;; just that monster
	      ;; lower chance of breeding if more monsters around,
	      ;;  won't mate when many are watching
	      (and (< count 4)
		   (= 0 (random (* count +monster-breed-slowdown+)))))

        (dotimes (i 20) ;; attempt 20 times until you give up
	  (multiple-value-bind (x y)
	      (scatter-location dungeon loc-x loc-y 1)
	    (when (cave-empty-bold? dungeon x y)
	      (let* ((kind (amon.kind mon))
		     (new-mon (produce-active-monster variant kind))
		     (sleeping nil))
		;;(warn "~s BREED to ~s,~s from ~s,~s!" (get-creature-name mon) x y loc-x loc-y)
		(place-single-monster! dungeon player new-mon x y sleeping)
		;;(warn "Monster at ~s,~s (from ~s,~s) is now ~s" x y
		;;      loc-x loc-y (cave-monsters dungeon x y))
		(update-monster! variant new-mon t)
		;;(light-spot! dungeon x y) ;; needed?
		;;(flush-window *map-frame*) ;; bad!
		(return-from possible-breed! t)
		))
	    )))
    
    nil))


  
(defun balance-bid (strategy creature bid)
  "Balances a bid-number from 0 to 10."
  
  (declare (ignorable strategy creature))
  
  (let ((bid-number (factor.offensive bid)))
    (setf (factor.offensive bid)
	  (cond ((< bid-number 1) 0)
		((< bid-number 10) 1)
		((< bid-number 25) 2)
		((< bid-number 45) 3)
		((< bid-number 65) 4)
		((< bid-number 100) 5)
		((< bid-number 150) 6)
		((< bid-number 210) 7)
		((< bid-number 300) 8)
		((< bid-number 450) 9)
		(t 10))))
  ;; balance up defensive based on dmg
  
  bid)

(defstruct (tactic-choice (:copier nil)
			  (:predicate nil))
  tactic
  bid)


(defvar *tactic-chooser* nil)
(defconstant +tactic-chooser-length+ 20)

(defun reset-tactic-bid! (obj)
  (setf (factor.offensive obj) 0
	(factor.defensive obj) 0
	(factor.cost obj) 0))
	
(defun reset-tactic-choice! (obj)
  (setf (tactic-choice-tactic obj) nil)
  (reset-tactic-bid! (tactic-choice-bid obj)))

(defun legal-tactic-choice? (obj)
  (and obj (tactic-choice-tactic obj)))

(defun %ensure-tactic-chooser ()
    (unless *tactic-chooser*
      (setf *tactic-chooser* (make-array +tactic-chooser-length+ :initial-element nil))
      (loop for i from 0 below +tactic-chooser-length+
	    do
	    (setf (svref *tactic-chooser* i)
		  (make-tactic-choice :tactic nil
				      :bid (make-instance 'tactic-factors))))))
  

(defun is-staggering? (creature)
  ;; confused monsters stagger about
  (let ((confusion-attr (gethash '<confusion> (amon.temp-attrs creature))))
    (cond ((and confusion-attr
		(plusp (attr.value confusion-attr)))
	   t)

	  ;; some monsters even move randomly
	  ((when-bind (random-mover (has-ability? creature '<random-mover>))
	     (let ((how-often (second random-mover)))
	       (when (< (random 100) (* 100 how-often))
		 t))))
	  (t nil))))

(defun %pick-special-ability (strategy mon)
  (let ((mon-type (amon.kind mon)))
    ;; check use of special-abilities
    (when-bind (spabs (monster.sp-abilities mon-type))
      (loop for i from 0 below +tactic-chooser-length+
	 do (reset-tactic-choice! (svref *tactic-chooser* i)))
	
      (loop for i from 0
	 for j in (cdr spabs)
	 do
	 (setf (tactic-choice-tactic (svref *tactic-chooser* i)) j))
	
      (block check-spabs
	;; try is the odds for using a spab
	(let ((try 0) ;;(random 100))
	      (choice nil)
	      (choice-table *tactic-chooser*))
	  ;; if try is below our frequency we will use a spab
	  (when (< try (first spabs))
	    ;;(warn "Use random spab from ~s" (cdr spabs))

	      
	    ;;#+debug-ai
	    ;;(loop for x across choice-table
	    ;;    when x
	    ;;  do (warn "Tactic for ~s is ~s" (monster.name mon) (tactic-choice-tactic x)))

	    ;; get bids
	    (loop for x across choice-table
	       when (legal-tactic-choice? x)
	       do (get-tactical-bid strategy mon (tactic-choice-tactic x)
				    (tactic-choice-bid x)))

	      
	    (loop for x across choice-table
	       when (legal-tactic-choice? x)
	       do
	       (progn
		 (balance-bid strategy mon (tactic-choice-bid x))
		 #+debug-ai
		 (warn "Bid ~s for ~s" x (monster.name mon))))

	    ;; primitive attacker gets most offensive always
	      
	    (loop for x across choice-table
	       when (legal-tactic-choice? x)
	       do
	       (cond ((not choice)
		      (setf choice x))
		     (t
		      (when (and (> (factor.offensive (tactic-choice-bid x))
				    (factor.offensive (tactic-choice-bid choice)))
				 (setf choice x))))))
	    ;; when random
	    ;;(setf choice (rand-elm (cdr spabs)))

	    ;; check if bid is high enough
	    (when (or (plusp (factor.offensive (tactic-choice-bid choice)))
		      (plusp (factor.defensive (tactic-choice-bid choice))))

	      (return-from %pick-special-ability choice))))))
    nil))
 

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
	      (cmb-monster-attack! dungeon player mon nx ny)
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
