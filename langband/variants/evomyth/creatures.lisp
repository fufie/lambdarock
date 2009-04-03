;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/creatures.lisp - code dealing with non-player creatures
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defmethod generate-random-name ((variant evomyth) creature race)
  "Returns a random name for a given creature of a given race."
  (declare (ignore creature race))
  "NulNulNix")


(defmethod produce-monster-kind ((variant evomyth) id name &key the-kind)
  (declare (ignore the-kind))
  
  (assert (stringp id))
  (assert (stringp name))

  (let ((retval (make-instance 'evo/monster-kind :id id :name name)))

    ;; add stuff here?
    (assert (listp (monster.type retval)))
    
    retval))

(defmethod produce-active-monster ((variant evomyth) mon-type)

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

      (let ((a-monster (make-instance amon-type :kind the-kind)))
	;; ensure we always have proper strategies
	(setf (amon.strategies a-monster) (loop for str in (monster.strategies the-kind)
					     collect (funcall str)))

	a-monster))))


  
  
(defmethod initialise-monster-kind! ((var-obj evomyth) (m-obj evo/monster-kind) keyword-args)

  (call-next-method)

  ;; add stuff here
  (let ((id (get-id m-obj)))

    (let ((depth (getf keyword-args :power-lvl))
	  (rarity (getf keyword-args :rarity)))

      (cond ((and depth rarity)
	     (unless (integerp depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Non-integer power-lvl argument for monster-kind"))
	     (unless (integerp rarity)
	       (signal-condition 'illegal-monster-data :id id :desc "Non-integer rarity argument for monster-kind"))
	     (unless (non-negative-integer? depth)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative power-lvl argument for monster-kind"))
	     (unless (non-negative-integer? rarity)
	       (signal-condition 'illegal-monster-data :id id :desc "Negative rarity argument for monster-kind"))
	     (push (cons depth rarity) (alloc-locations m-obj)))
	    ((and (eq depth nil) (eq depth rarity)))
	    (t
	     (signal-condition 'illegal-monster-data :id id
			       :desc "Unknown power-lvl + rarity argument for monster-kind"))))
  
    (when-bind (pic (getf keyword-args :picture))
      (setf (monster.picture m-obj) pic))

    (when-bind (strats (getf keyword-args :strategies))
      (let ((constrs '()))
        (dolist (s strats)
          (cond ((consp s)
                 (case (first s)
                   ((<avoid> <fight>)
                      (let ((args (rest s)))
                        (push #'(lambda () (funcall (get-strategy-constructor var-obj (first s)) args)) constrs)))
                   (otherwise
                      (warn "Unhandled strategy ~s" s))))
                (t
                 (warn "Unhandled strategy ~s" s))))
                            
        ;;(avoid-match '(<avoid-player> <avoid-omnivore> <avoid-carnivore> <avoid-herbivore>)))
        
        ;; let us go through known groups first
        ;; avoids
        ;;(let ((avoids (intersection strats avoid-match)))
                                        ;:(setf strats (set-difference strats avoid-match))
        ;;(warn "Avoids: ~a" avoids)
        ;;(setf constrs (get-strategy-constructor var-obj avoids) constrs))
        

        ;;        (setf constrs (append constrs (loop for s in strats
        ;;                                            for constr = (get-strategy-constructor var-obj s)
        ;;                                            when constr
        ;;                                              collect constr)))

        (setf (monster.strategies m-obj) constrs)))
    
    m-obj))


(defmethod initialise-monsters& ((var-obj evomyth) &key old-file (file "monsters"))
  "old-file is ignored in evomyth."
  (declare (ignore old-file))
  
  (cond (file
	 (let ((*load-verbose* nil))
	   (load-variant-data& var-obj file)))
	(t
	 (error "No file specified for monster-init.")))
    
  ;; initialise all tables
  (let ((object-tables (variant.monsters-by-level var-obj)))
    (maphash #'(lambda (key obj)
		 (evo/update-gobj-table! var-obj key obj
					 #'create-alloc-table-monsters))
	     object-tables)))

