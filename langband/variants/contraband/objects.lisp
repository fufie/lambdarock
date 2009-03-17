;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/objects.lisp - code dealing with physical objects
Copyright (c) 2003 - Stig Erik Sandoe

|#

(in-package :org.langband.contraband)

(defmethod initialise-objects& ((var-obj contraband) &key old-file (file "objects"))

  (declare (ignore old-file))
  (cond
    (file
     (let ((*load-verbose* nil))
       (load-variant-data& var-obj file)))
    (t
     (error "No file specified for floor-init.")))


  ;; initialise all tables
;;    (warn "Mapping ~a" object-tables)
  ;; let us find some behaviour

  (let ((object-tables (variant.objects-by-level var-obj)))
    
    (maphash #'(lambda (key obj)
		 (con/update-gobj-table! var-obj key obj
					 #'create-alloc-table-objects))
	     object-tables))


  
;;  #+langband-debug
;;  (%output-kinds-to-file "dumps/obj.lisp")
  )

(defmethod write-obj-description ((variant contraband) (obj active-object) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))
  (let* (;;(o-type (aobj.kind obj))
	 (name (object.name obj))
	 ;;(flavour (if store nil (object.flavour o-type)))
	 (known-type (or store (is-object-known? obj)))
	 (number (aobj.number obj))
	 ;;(plural-string nil)
	 )


    (let ((str (plural-name number name nil known-type nil :numeric-prefix numeric-prefix)))
      (write-string str stream))

    ))

(defmethod initialise-object-kind! ((var-obj contraband) (new-obj object-kind/armour) keyword-args)

  (call-next-method)
  
  (when-bind (skill (getf keyword-args :skill))
    (assert (find skill '(<shield> <light> <heavy>)))
    (setf (object.armour-skill new-obj) skill))

  (when-bind (bulk (getf keyword-args :bulk))
    (assert (and (integerp bulk) (>= bulk 0)))
    (setf (object.armour-bulk new-obj) bulk))

  new-obj)

(defmethod use-object! ((var contraband) dungeon player the-object &key (which-use :use))
;;  (declare (ignore var))
  (check-type the-object active-object)
  
  (let* ((okind (aobj.kind the-object))
	 (effects (object.effects okind))
	 (the-effect (find which-use effects :key #'effect-entry-type))
	 (retval :not-used))

    ;;(warn "Found use-effect ~s for ~s" the-effect the-object)

    (unless the-effect 
      (warn "Didn't find ~s effect for ~s" which-use (get-id okind))
      (return-from use-object! retval))
	    
    
    (when the-effect
      (assert (and (effect-entry-p the-effect)
		   (functionp (effect-entry-fun the-effect))))
;;      (unless (compiled-function-p (effect-entry-fun the-effect))
;;	(warn "not compiled"))
      
      (setf retval (funcall (effect-entry-fun the-effect) dungeon player the-object))

      (cond ((eq retval :used)
	     (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	    ((eq retval :still-useful)
	     (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	    ;; do nothing
	    ((eq retval :not-used) nil)
	    ((eq retval nil)
	     (warn "Object-effect ~s for object ~s returned nil, fix?"
		   (effect-entry-type the-effect) the-object))
	    (t
	     (error "Unknown return-value from effect: ~s" retval))))
      
    
    retval))
