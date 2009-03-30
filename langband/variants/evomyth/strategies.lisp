;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/strategies.lisp - ai-related stuff
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defclass guard (ai-strategy)
  ((id :initform "guard")
   (cur-dest     :initform '() :accessor strategy.cur-dest)
   (top-left     :initform '() :accessor strategy.top-left)
   (top-right    :initform '() :accessor strategy.top-right)
   (bottom-left  :initform '() :accessor strategy.bottom-left)
   (bottom-right :initform '() :accessor strategy.bottom-right)
   ))

(defclass avoid-carnivore (ai-strategy)
  ((id :initform "avoid-carnivore"))
   

(defmethod execute-strategy ((strategy guard) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))

  (let ((mx (location-x mon))
	(my (location-y mon))
	(temp-attrs (amon.temp-attrs mon))
	(staggering nil)
	)

    ;; confused monsters stagger about
    (let ((confusion-attr (gethash '<confusion> temp-attrs)))
      (cond ((and confusion-attr
		  (plusp (attr.value confusion-attr)))
	     (setf staggering t))
	    ;; some monsters even move randomly
	    ((when-bind (random-mover (has-ability? mon '<random-mover>))
	       (let ((how-often (second random-mover)))
		 (when (< (random 100) (* 100 how-often))
		   (setf staggering t)))))))

    (let ((dest (strategy.cur-dest strategy)))

      (when dest
	;;(warn "try to go ~s" dest)
	(let ((dest-x (first dest))
	      (dest-y (second dest))
	      (moves nil))
	  
	  (when (and (= dest-x mx) (= dest-y my))
	    (setf (strategy.cur-dest strategy) nil)
	    (return-from execute-strategy nil))
	  
	  (setf moves (get-move-direction mx my dest-x dest-y))
	  
	  (loop named move-attempts
		for i from 0 to 4
		do
		(let* ((dir (if staggering (aref *ddd* (random 8)) (aref moves i)))
		       (nx (+ mx (aref *ddx* dir)))
		       (ny (+ my (aref *ddy* dir)))
		       )
		  ;;(warn "At (~s,~s) checking (~s,~s) ~s ~s"
		  ;;	mx my nx ny (cave-floor-bold? dungeon nx ny) (cave-empty-bold? dungeon nx ny))
		  (when (and (cave-empty-bold? dungeon nx ny)
			     (not (and (= nx (location-x *player*))
				       (= ny (location-y *player*)))))
		    ;;(warn "Going (~s,~s)" nx ny)
		    (swap-monsters! dungeon *player* mx my nx ny)
		    (return-from execute-strategy t))

		  ))
	  ))

      (unless dest
	;; we need to find a new dest
	;; we go for corners in this first version

	(ecase (random 4)
	  (0 (setf (strategy.cur-dest strategy) (strategy.top-left strategy)))
	  (1 (setf (strategy.cur-dest strategy) (strategy.bottom-left strategy)))
	  (2 (setf (strategy.cur-dest strategy) (strategy.top-right strategy)))
	  (3 (setf (strategy.cur-dest strategy) (strategy.bottom-right strategy)))
	  ))

	;; next turn we have a dest!
	

      t)))

(defmethod execute-strategy ((strategy avoid-carnivore) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))

  (let ((mx (location-x mon))
	(my (location-y mon))
	(temp-attrs (amon.temp-attrs mon))
	(staggering nil)
	)

    (unless *tactic-chooser*
      (setf *tactic-chooser* (make-array +tactic-chooser-length+ :initial-element nil))
      (loop for i from 0 below +tactic-chooser-length+
	    do
	    (setf (svref *tactic-chooser* i)
		  (make-tactic-choice :tactic nil
				      :bid (make-instance 'tactic-factors)))))

    
    ;; confused monsters stagger about
    (let ((confusion-attr (gethash '<confusion> temp-attrs)))
      (cond ((and confusion-attr
		  (plusp (attr.value confusion-attr)))
	     (setf staggering t))
	    ;; some monsters even move randomly
	    ((when-bind (random-mover (has-ability? mon '<random-mover>))
	       (let ((how-often (second random-mover)))
		 (when (< (random 100) (* 100 how-often))
		   (setf staggering t)))))))

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


(defun make-guard-strategy (tl bl tr br)
  "Makes a guard-ai for the given coords."
  (let ((strategy (make-instance 'guard)))
    (setf (strategy.top-left strategy) tl
	  (strategy.bottom-left strategy) bl
	  (strategy.top-right strategy) tr
	  (strategy.bottom-right strategy) br)
    strategy))

(defmethod print-object ((inst guard) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
           (strategy.id inst)
	   (strategy.cur-dest inst)
	   ))

  inst)
