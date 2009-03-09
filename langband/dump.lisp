;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: dump.lisp - code related to dumping various information-structures, ...
Copyright (c) 2002-2004 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defmethod get-loadable-form ((variant variant) (object object-kind) &key (full-dump nil))
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-value val)))))))
    (setf the-form (list 'define-object-kind 
			 (get-id object)
			 (object.name object)))
    (possibly-add :numeric-id (object.numeric-id object))
;;    (possibly-add :desc (object.desc object))
    (possibly-add :gfx-sym (gfx-sym object))
    (possibly-add :text-sym (text-sym object))
    (possibly-add :locations (alloc-locations object) nil)
    (possibly-add :weight (object.weight object))
    (possibly-add :cost (object.cost object))
    (possibly-add :flags (object.flags object))
    (possibly-add :identified (object.tried object))
    (possibly-add :sort-value (object.sort-value object) 0)
    (possibly-add :easy-know (object.easy-know object))
    (possibly-add :the-kind (object.the-kind object))
    (possibly-add :vulnerabilities (get-vulnerabilities object) 0)
    (possibly-add :immunities (get-immunities object) 0)
    (possibly-add :sustains (get-stat-sustains object) '())
    (possibly-add :light-radius (get-light-radius object) 0)
    (possibly-add :speed-modifier (get-light-radius object) 0)
    (possibly-add :stat-modifiers (get-stat-modifiers object))
    (possibly-add :abilities (object.abilities object))
    (possibly-add :ignores (get-ignores object) 0)
    (possibly-add :resists (get-resists object) 0)

    (possibly-add :armour-rating (get-armour-rating object) 0)
    (possibly-add :armour-modifier (get-armour-modifier object) 0)

    (possibly-add :damage-dice (get-damage-dice object) 0)
    (possibly-add :number-of-damage-dice (get-number-of-damage-dice object) 0)
    (possibly-add :tohit-modifier (get-tohit-modifier object) 0)
    (possibly-add :damage-modifier (get-damage-modifier object) 0)
    
    (when full-dump
      (possibly-add :flavour (object.flavour object)))

    the-form)))


(defmethod get-loadable-form ((variant variant) (object monster-kind) &key)
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
	     (unless (equal val def-val)
	       (setf the-form (nconc the-form (list initarg (loadable-value val)))))))
      
      (setf the-form (list 'define-monster-kind 
			   (get-id object)
			   (monster.name object)))
      
      (possibly-add :desc (monster.desc object))
      (possibly-add :gfx-sym (gfx-sym object))
      (possibly-add :text-sym (text-sym object))
      (possibly-add :alignment (monster.alignment object))
      (possibly-add :locations (alloc-locations object) '())
      (possibly-add :type (monster.type object))
      (possibly-add :hitpoints (monster.hitpoints object))
      (possibly-add :armour (monster.armour object))
      (possibly-add :speed (monster.speed object) 0)
      (possibly-add :xp (monster.xp object) 0)
      
      (possibly-add :abilities (monster.abilities object))
      (possibly-add :special-abilities (monster.sp-abilities object))
      
      (possibly-add :resists (get-resists object) 0)
      (possibly-add :immunities (get-immunities object) 0)
      (possibly-add :vulnerabilities (get-vulnerabilities object) 0)
      
      (possibly-add :alertness (monster.alertness object))
      (possibly-add :vision (monster.vision object))
      (possibly-add :attacks (convert-obj (monster.attacks object) :attk-list))
      (possibly-add :treasures (monster.treasures object))
      (possibly-add :gender (monster.gender object))

      ;; in group, uncertain of this one
;;      (possibly-add :in-group (monster.in-group object))

      the-form)))

(defmethod print-object ((inst character-class) stream)
  (print-unreadable-object
      (inst stream :identity t)
    (format stream "~:(~S~) [~A ~A]" (lbsys/class-name inst)
	    (get-id inst)
	    (class.name inst)))
  inst)


(defmethod print-object ((inst character-race) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (lbsys/class-name inst)
	   (get-id inst)
	   (race.name inst)))
  inst)

(defmethod print-object ((inst floor-type) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
	   (floor.name inst) (floor.id inst)))
  inst)

(defmethod print-object ((inst house) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A ~A]" (lbsys/class-name inst)
	   (slot-value inst 'name)
	   (slot-value inst 'id)
	   (slot-value inst 'owner)
	   ))
  inst)

(defmethod print-object ((inst owner) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A]" (lbsys/class-name inst)
	   (slot-value inst 'name)))
  inst)

(defmethod print-object ((inst object-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
	   (object.name inst) (object.numeric-id inst)))
  inst)

(defmethod print-object ((inst active-object) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~a ~S (~a,~a)]" (lbsys/class-name inst)
	   (aobj.number inst) (aobj.kind inst) (location-x inst) (location-y inst))
  inst))

(defmethod print-object ((inst flavour) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
	   (flavour.name inst)))
  inst)

(defmethod print-object ((inst monster-kind) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
	   (get-power-lvl inst)
	   (get-id inst)
	   ))
  inst)

(defmethod print-object ((inst active-monster) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~S, (~s,~s)]" (lbsys/class-name inst)
	   (get-id (amon.kind inst)) (location-x inst) (location-y inst))
  inst))

(defmethod print-object ((inst trap-type) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst) 
	   (trap.id inst)))
  inst)

(defmethod print-object ((inst active-trap) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
	   (if (decor.type inst) (trap.id (decor.type inst)) "NO TYPE")))
  inst)

(defmethod print-object ((inst door-type) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
	   (door.id inst)))
  inst)

(defmethod print-object ((inst active-door) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
	   (if (decor.type inst) (door.id (decor.type inst)) "NO TYPE")))
  inst)


(defmethod print-object ((inst treasure-drop) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a ~a ~a]" (lbsys/class-name inst)
	   (drop.chance inst) (drop.quality inst) (drop.amount inst) (drop.type inst))
   inst))

(defmethod print-object ((inst creature-attribute) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
	   (attr.key inst) (attr.value inst))
   inst))

(defmethod print-object ((inst temp-creature-attribute) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a - ~a]" (lbsys/class-name inst)
	   (attr.key inst) (attr.value inst) (attr.duration inst))
   inst))

(defmethod print-object ((inst character-stat) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
	   (stat.symbol inst) (stat.number inst))
   inst))

(defmethod print-object ((inst element) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
	   (element.symbol inst) (element.number inst))
   inst))

(defmethod print-object ((inst effect) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
	   (effect.symbol inst) (effect.number inst))
   inst))

(defmethod print-object ((inst gender) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
	   (get-id inst) (gender.symbol inst))
   inst))

(defmethod print-object ((inst attack) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a ~a]" (lbsys/class-name inst)
	   (attack.kind inst) (attack.dmg-type inst) (attack.damage inst))
   inst))

(defmethod print-object ((inst attack-type) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
	   (attack-type.key inst) (attack-type.power inst))
   inst))


(defmethod print-object ((inst items-in-container) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~A ~A]" (lbsys/class-name inst)
	   (items.cur-size inst)
	   (items.max-size inst)))
  inst)

(defmethod print-object ((inst window) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a ~a]" (lbsys/class-name inst)
	   (window.id inst) (window.num-id inst) (window.visible? inst))
   inst))

(defmethod print-object ((inst visual-projectile) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (projectile.id inst)))

  inst)

(defmethod print-object ((inst visual-state) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (visual-state.key inst)))

  inst)

(defmethod print-object ((inst ai-strategy) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (strategy.id inst)))

  inst)

(defmethod print-object ((inst peaceful-mover) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S]" (lbsys/class-name inst)
           (strategy.id inst)
	   (first (strategy.destinations inst))
	   ))
  inst)

(defmethod print-object ((inst tactic-factors) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S ~S ~S]" (lbsys/class-name inst)
           (factor.offensive inst)
           (factor.defensive inst)
           (factor.cost inst)
	   ))

  inst)


(defun dump-objects (out-file &optional object-list)
  (let ((obj-list (if object-list
		      object-list
		      (get-object-list)))
	(var-obj *variant*)
	(*print-case* :downcase)
	(*print-right-margin* 120))
    
    (with-open-file (ffile (pathname out-file)
			   :direction :output
			   :if-exists :supersede
			   :if-does-not-exist :create)
      (pprint '(in-package :langband)
	      ffile)
      (terpri ffile)
      (dolist (x obj-list)
	(print (get-loadable-form var-obj x) ffile)
	(terpri ffile))
      (terpri ffile))))


;; turn into loadable forms
(defun dump-floors (out-file &optional floor-list)
  (let* ((features (if floor-list
		       floor-list
		       (loop for x being the hash-values of (variant.floor-types *variant*)
			     collecting x)))
	 (sorted-features (sort (copy-list features) #'< :key #'floor.id)))

    (let ((*print-case* :downcase))
      (with-open-file (ffile (pathname out-file)
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	(loop for x in sorted-features
	      do
	      (pprint `(define-floor-type ,(floor.id x)
			,(floor.name x)
			:gfx-sym ,(gfx-sym x)
			:text-sym ,(text-sym x)
			:mimic ,(floor.mimic x))
		      ffile))))))


(defun dump-alloc-table (table fname)
  "Dumps an alloc-table to the given file."
  (with-open-file (s (pathname fname)
		     :direction :output 
		     :if-exists :supersede)
    (loop for i across table
	  do
	  (format s "~&~a: ~a~%" (alloc.depth i) (alloc.obj i)))))
