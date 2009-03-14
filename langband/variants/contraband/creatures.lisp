;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/creatures.lisp - code dealing with non-player creatures
Copyright (c) 2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)


(defun is-atrocitan? (creature)
  "Returns T if the creature is from Atrocitas."
  (etypecase creature
    (player (eq (race.symbol (player.race creature)) '<atrocitan>))
    (active-monster (find '<atrocitan> (monster.type (amon.kind creature))))))

(defun is-copian? (creature)
  "Returns T if the creature is from copia."
  (etypecase creature
    (player (eq (race.symbol (player.race creature)) '<copian>))
    (active-monster (find '<copian> (monster.type (amon.kind creature))))))

(defun get-nationality (creature)
  "Returns a symbol for nationality."
  (cond ((is-copian? creature) '<copian>)
	((is-atrocitan? creature) '<atrocitan>)
	(t nil)))

(defun in-faction? (creature faction)
  "Returns T if the creature is part of the given faction."
  (cond ((eq faction '<atrocitan>) (is-atrocitan? creature))
	((eq faction '<copian>) (is-copian? creature))
	((typep creature 'active-monster) (find faction (monster.type (amon.kind creature))))
	;; add for player here
	(t
	 nil)))

(defmethod generate-random-name ((variant contraband) creature race)
  "Returns a random name for a given creature of a given race."
  (declare (ignore creature race))
  "NulNulNix")


(defmethod produce-monster-kind ((variant contraband) id name &key the-kind)
  (declare (ignore the-kind))
  
  (assert (stringp id))
  (assert (stringp name))

  (let ((retval (make-instance 'con/monster-kind :id id :name name)))

    ;; add stuff here?
    (assert (listp (monster.type retval)))
    
    retval))

(defmethod produce-active-monster ((variant contraband) mon-type)

  (assert (not (eq mon-type nil)))

  
  (let ((the-kind (cond ((symbolp mon-type)
			 (get-monster-kind variant (symbol-name mon-type)))
			((stringp mon-type)
			 (get-monster-kind variant mon-type))
			((typep mon-type 'monster-kind)
			 mon-type)
			(t
			 (error "Mon-type argument to produce-active-monster is not {symbol,string,mkind}, but is ~s"
				mon-type)))))
    
    (unless (typep the-kind 'monster-kind)
      (warn "Unable to find the monster-kind ~s" mon-type)
      (return-from produce-active-monster nil))

    (unless (is-creatable? variant the-kind)
      (warn "Tried to produce dead unique ~a, failed" (get-id the-kind)) 
      (return-from produce-active-monster nil))

    (let ((amon-type 'active-monster))

      (when (find '<npc> (monster.type the-kind))
	(setf amon-type 'npc))
    
      (make-instance amon-type :kind the-kind)
      )))

  
  
(defmethod initialise-monster-kind! ((var-obj contraband) (m-obj con/monster-kind) keyword-args)

  (call-next-method)

  ;; add stuff here
  (when-bind (pic (getf keyword-args :picture))
    (setf (monster.picture m-obj) pic))

  m-obj)


(defmethod initialise-monsters& ((var-obj contraband) &key old-file (file "monsters"))
  "old-file is ignored in contraband."
  (declare (ignore old-file))
  
  (cond (file
	 (let ((*load-verbose* nil))
	   (load-variant-data& var-obj file)))
	(t
	 (error "No file specified for monster-init.")))
    
  ;; initialise all tables
  (let ((object-tables (variant.monsters-by-level var-obj)))
    (maphash #'(lambda (key obj)
		 (con/update-gobj-table! var-obj key obj
					 #'create-alloc-table-monsters))
	     object-tables)))

(defclass guard (ai-strategy)
  ((id :initform "guard")
   (cur-dest     :initform '() :accessor strategy.cur-dest)
   (top-left     :initform '() :accessor strategy.top-left)
   (top-right    :initform '() :accessor strategy.top-right)
   (bottom-left  :initform '() :accessor strategy.bottom-left)
   (bottom-right :initform '() :accessor strategy.bottom-right)
   ))


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

