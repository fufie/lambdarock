;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/ego.lisp - ego-item code
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

;;; Ego code

(defun get-ego-item (variant id)
  (etypecase id
    (string (gethash id (variant.ego-items variant)))))

(defun (setf get-ego-item) (value variant id)
  (etypecase id
    (string (setf (gethash id (variant.ego-items variant)) value))))

(defmethod get-power-lvl ((obj ego-item))
  (ego.power-lvl obj))


(defun define-ego-item (id name &key (numeric-id :unspec) (power-lvl :unspec)
			(xtra :unspec) (max-to-ac :unspec) (max-to-hit :unspec)
			(max-to-dmg :unspec) (pval :unspec) (locations :unspec)
			(cost :unspec) (weight :unspec) (obj-types :unspec)
			(flags :unspec))
  "Attempts to define an ego-item."


  (check-type id string)
  (check-type name string)
  
  (assert (verify-id id))
  (assert (> (length name) 0))
  
  (let ((variant *variant*)
	(ego-item (make-instance 'ego-item :name name :id id)))

    (cond ((integerp numeric-id)
	   (setf (ego.numeric-id ego-item) numeric-id))
	  ((eq numeric-id :unspec))
	  (t
	   (warn "Unknown value for ego-numeric-id: ~s" numeric-id)))

    
    (cond ((integerp power-lvl)
	   (setf (ego.power-lvl ego-item) power-lvl))
	  ((eq power-lvl :unspec))
	  (t
	   (warn "Unknown value for ego-power-lvl: ~s" power-lvl)))
    
    (cond ((integerp xtra)
	   ;; check this later
	   ;;(warn "Ego ~s has xtra-parameter: ~s" id xtra)
	   (setf (ego.xtra ego-item) xtra))
	  ((eq xtra :unspec))
	  (t
	    (warn "Unknown value for ego-xtra: ~s" xtra)))


    (cond ((integerp max-to-hit)
	   (setf (ego.max-to-hit ego-item) max-to-hit))
	  ((eq max-to-hit :unspec))
	  (t
	   (warn "Unknown value for ego-max-to-hit: ~s" max-to-hit)))

    (cond ((integerp max-to-dmg)
	   (setf (ego.max-to-dmg ego-item) max-to-dmg))
	  ((eq max-to-dmg :unspec))
	  (t
	   (warn "Unknown value for ego-max-to-dmg: ~s" max-to-dmg)))
    
    (cond ((integerp max-to-ac)
	   (setf (ego.max-to-ac ego-item) max-to-ac))
	  ((eq max-to-ac :unspec))
	  (t
	   (warn "Unknown value for ego-max-to-ac: ~s" max-to-ac)))

    (cond ((integerp pval)
	   (setf (ego.pval ego-item) pval))
	  ((eq pval :unspec))
	  (t
	   (warn "Unknown value for ego-pval: ~s" pval)))

    (cond ((consp locations)
	   (setf (ego.locations ego-item) locations))
	  ((eq locations :unspec))
	  (t
	   (warn "Odd ego-locations argument ~s" locations))) 
        
    (cond ((integerp cost)
	   (setf (ego.cost ego-item) cost))
	  ((eq cost :unspec))
	  (t
	   (warn "Unknown value for ego-cost: ~s" cost)))

    (cond ((integerp weight)
	   (setf (ego.weight ego-item) weight))
	  ((eq weight :unspec))
	  (t
	   (warn "Unknown value for ego-weight: ~s" weight)))

    (cond ((consp obj-types)
	   (setf (ego.obj-types ego-item) obj-types))
	  ((eq obj-types :unspec))
	  (t
	   (warn "Unknown obj-types value ~s for ego-item." obj-types))) 
    
    (cond ((consp flags)
	   (let ((ok-flags '()))
	     (dolist (i flags)
	       (cond ((consp i)
		      (cond ((eq (car i) '<slay>)
			     (let ((slay (second i)))
			       (if (is-legal-slay? variant slay)
				   (push slay (get-slays ego-item))
				   (signal-condition 'illegal-object-data :id id
						     :desc (format nil "Wanted slay is unknown/illegal: ~s"
								   slay)))))

			    ((eq (car i) '<execute>)
			     (let ((slay (second i)))
			       (if (is-legal-slay? variant slay)
				   (push slay (get-executes ego-item))
				   (signal-condition 'illegal-object-data :id id
						     :desc (format nil "Wanted execute is unknown/illegal: ~s"
								   slay)))))
			    
			    ((eq (car i) '<brand>)
			     (let ((brand (second i)))
			       (if (is-legal-element? variant brand)
				   (push brand (get-brands ego-item))
				   (signal-condition 'illegal-object-data :id id
						     :desc (format nil "Wanted brand is unknown/illegal: ~s"
								   brand)))))
			    
			    ((eq (car i) '<sustain>)
			     (let ((sust-stat (second i)))
			       (if (is-legal-stat? variant sust-stat)
				   (bit-flag-add! (get-stat-sustains ego-item)
						  (stat.bit-flag (get-stat-obj variant sust-stat)))
				   (signal-condition 'illegal-object-data :id id
						     :desc (format nil "Not legal element in stat-sustains list: ~s"
								   sust-stat)))))
			     
			    ((eq (car i) '<resist>)
			      (let ((resisted-elm (second i)))
			       (if (is-legal-element? variant resisted-elm)
				   (bit-flag-add! (get-resists ego-item)
						  (get-element-flag variant resisted-elm))
				   (signal-condition 'illegal-object-data :id id
						     :desc (format nil "Not legal element in object-resists list: ~s"
								   resisted-elm)))))

			    ((eq (car i) '<ignore>)
			     (let ((ignored-elm (second i)))
			       (if (is-legal-element? variant ignored-elm)
				   (bit-flag-add! (get-ignores ego-item)
						  (get-element-flag variant ignored-elm))
				   (signal-condition 'illegal-object-data :id id
						     :desc (format nil "Not legal element in object-ignores list: ~s"
								   ignored-elm)))))

			    ((eq (first i) '<curse>)
			     (bit-flag-add! (get-sanctity ego-item) +sanctity-cursed+))
			    ((eq (first i) '<heavy-curse>)
			     (bit-flag-add! (get-sanctity ego-item) +sanctity-heavily-cursed+))
			    ((eq (first i) '<permanent-curse>)
			     (bit-flag-add! (get-sanctity ego-item) +sanctity-perma-cursed+))
			    ((eq (first i) '<blessed>)
			     (bit-flag-add! (get-sanctity ego-item) +sanctity-blessed+))
			    ((eq (first i) '<holy>)
			     (bit-flag-add! (get-sanctity ego-item) +sanctity-holy+))
			    ((eq (first i) '<divine>)
			     (bit-flag-add! (get-sanctity ego-item) +sanctity-divine+))

			    (t
			     (warn "~s fell through in ego-init" i)
			     (push i ok-flags)
			     )))
		     (t
		      (push i ok-flags))))
;;	     (when ok-flags
;;	       (warn "Flags ~s" ok-flags))
	   (setf (ego.flags ego-item) (nreverse ok-flags))))
	  ((eq flags :unspec))
	  ((eq flags '()))
	  (t
	   (warn "Unknown value for ego-flags: ~s" flags)))

    (setf (get-ego-item variant id) ego-item)

    (apply-filters-on-obj :ego-items variant ego-item)
    
    ego-item))


(defmethod get-loadable-form (variant (object ego-item) &key (full-dump nil))

;;(defun %dump-form (variant object)
  (declare (ignore full-dump variant))
  
  (let ((the-form '()))
    (flet ((possibly-add (initarg val &optional (def-val nil))
             (unless (equal val def-val)
               (setf the-form (nconc the-form (list initarg (loadable-value val)))))))

    (setf the-form (list 'define-ego-item
                         (ego.id object)
                         (ego.name object)))

;;    (possibly-add :id (ego.id object) "dummy-id")
    (possibly-add :numeric-id (ego.numeric-id object) -1)
    (possibly-add :power-lvl (ego.power-lvl object) 0)
    (possibly-add :xtra (ego.xtra object) 0)
    
    (possibly-add :max-to-hit (ego.max-to-hit object) 0)
    (possibly-add :max-to-dmg (ego.max-to-dmg object) 0)
    (possibly-add :max-to-ac (ego.max-to-ac object) 0)
    (possibly-add :pval (ego.pval object) 0)

    (possibly-add :locations (ego.locations object) nil)
    ;;(possibly-add :depth (ego.depth object) 0)
    ;;(possibly-add :rarity (ego.rarity object) 0)
    (possibly-add :cost (ego.cost object) 0)
    (possibly-add :weight (ego.weight object) 0)

    (possibly-add :obj-types (ego.obj-types object) '())
    (possibly-add :flags (ego.flags object) '())
    
    ;; ADD more shit
    
    the-form)))


(defmethod print-object ((inst ego-item) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (lbsys/class-name inst)
           (ego.name inst) ))
  inst)

#||

(defun dump-egos (&optional (fname "ego.dump"))
  (let* ((variant *variant*)
	 (vals (loop for x being the hash-values of (variant.ego-items variant)
		     collecting x))
	 (sorted-vals (sort vals #'< :key #'ego.numeric-id))
	 (*print-case* :downcase)
	 (*print-right-margin* 140))

    (with-open-file (s (pathname fname)
		       :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
      (loop for x in sorted-vals
	    do
	    (progn
	      (pprint (get-loadable-form variant x) s)
	      (terpri s)))
	    
      )))

(defun parse-ego-items& (variant file)
  (let (;;(fname (variant-data-fname variant file))
	(fname file)
	(*egos* (make-hash-table :test #'equal)))
    
    (warn "Reading ego from ~s" fname)

    (compat-read-ego-file& fname)
    (dump-egos "ego-items.txt")
    
    t))

(in-package :cl-user)

(defun k ()
  (let ((*package* (find-package :lb)))
    (load "modules/compat/ego.lisp")
    (lb::parse-ego-items& nil "ego_item.txt")
    ))

||#
