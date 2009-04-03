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

(defclass avoidance-strategy (ai-strategy)
  ((id :initform "avoidance")
   (avoid-diet :initform nil :initarg :avoid-diet :accessor strategy.avoid-diet)
   (avoid-type :initform nil :initarg :avoid-type :accessor strategy.avoid-type)))

(defclass fight-strategy (ai-strategy)
  ((id :initform "fight")
   (when-to-fight :initform nil :initarg :when-to-fight :accessor strategy.when-to-fight)))

;; move to engine later
(defun try-moving-creature (dungeon src-x src-y dest-x dest-y &optional (reversed nil))
  (let ((moves (get-move-direction src-x src-y dest-x dest-y)))
    (loop named move-attempts
	  for i from 0 to 4
	  do
       (let* ((dir (if reversed (opposite-direction (aref moves i)) (aref moves i)))
	      (nx (+ src-x (aref *ddx* dir)))
	      (ny (+ src-y (aref *ddy* dir))))
		
	 (when (and (cave-empty-bold? dungeon nx ny)
		    (not (and (= nx (location-x *player*))
			      (= ny (location-y *player*)))))
	   ;;(warn "Dir ~s/~s  -> Going (~s,~s) -> (~s,~s)" (aref moves i) dir src-x src-y nx ny)
	   (swap-monsters! dungeon *player* src-x src-y nx ny)
	   (return-from try-moving-creature t))))
    nil))



(defmethod execute-strategy ((strategy avoidance-strategy) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))
  (let ((avoid-type (strategy.avoid-type strategy))
	(diet-type (strategy.avoid-diet strategy))
	(mx (location-x mon))
	(my (location-y mon))
	(px (location-x *player*))
	(py (location-y *player*)))

    ;;(warn "Execute ~a ~a ~a" (strategy.id strategy) avoid-type '<player>)
    (cond ((eql avoid-type '<player>)
	   ;; reverse argument is set to true
	   (when-bind (status (try-moving-creature dungeon mx my px py t))
	     (return-from execute-strategy t))))

    nil))

(defmethod execute-strategy ((strategy fight-strategy) (mon active-monster) dungeon &key action force)
  (declare (ignorable action force))
  ;;(warn "Execute avoid: ~a" strategy)
  nil)


(defmethod print-object ((inst guard) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
           (strategy.id inst)
	   (strategy.cur-dest inst)
	   ))

  inst)

(defmethod print-object ((inst avoidance-strategy) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S ~S ~S]" (lbsys/class-name inst)
           (strategy.id inst)
	   (strategy.key inst)
	   (strategy.avoid-diet inst)
	   (strategy.avoid-type inst)
	   ))

  inst)

(defmethod print-object ((inst fight-strategy) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S ~S]" (lbsys/class-name inst)
           (strategy.id inst)
	   (strategy.key inst)
	   (strategy.when-to-fight inst)
	   ))

  inst)


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
