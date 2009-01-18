;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: object.lisp - code for object-kinds
Copyright (c) 2000-2004 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code for object-kinds which is basic and should be widely
ADD_DESC: available in the game.

||#

(in-package :org.langband.engine)


(defmacro define-object-type (name &key is key kind-slots aobj-slots)
  "Creates necessary objects and registers them."
  (let* ((ok-name (concat-pnames 'object-kind/ name))
	 (act-name (concat-pnames 'active-object/ name))
	 (ok-par-name (if is (concat-pnames 'object-kind/ is) 'object-kind))
	 (act-par-name (if is (concat-pnames 'active-object/ is) 'active-object))
	 (reg-call (when key `(setf (gethash ',key lb-engine::*obj-type-mappings*) (cons ',ok-name ',act-name)))))
    
    (let ((retval `(progn
		    (defclass ,ok-name (,ok-par-name) ,kind-slots)
		    (defclass ,act-name (,act-par-name) ,aobj-slots)
		    ,reg-call)))
      
;;      (warn "Ret: ~s" retval)
      
      retval)))

(defun satisfies-obj-type? (type obj)
  "Checks if the OBJ satisifies the object-type TYPE."
  ;; hackish
  (let ((mapping (gethash type *obj-type-mappings*)))
    (unless mapping
      (warn "Obj-type ~s fell through" type))
    (etypecase obj
      (object-kind   (typep obj (car mapping)))
      (active-object (typep obj (cdr mapping))))))
       

(defmethod learn-about-object! (player object what)
  (error "Fell through learn with ~s ~s ~s" player object what))

(defmethod learn-about-object! (player (object active-object) (what (eql :aware)))
  (learn-about-object! player (aobj.kind object) what))

(defmethod learn-about-object! (player (object object-kind) (what (eql :aware)))
  (declare (ignore player))
  (setf (object.aware object) t))

(defmethod learn-about-object! (player (object active-object) (what (eql :tried)))
  (learn-about-object! player (aobj.kind object) what))

(defmethod learn-about-object! (player (object object-kind) (what (eql :tried)))
  (declare (ignore player))
  (setf (object.tried object) t))

(defmethod learn-about-object! (player (object active-object) (what (eql :known)))
  (declare (ignore player))
  (let ((flag (aobj.identify object)))
    (bit-flag-remove! flag +ident-sense+)
    (bit-flag-remove! flag +ident-empty+)
    (bit-flag-add! flag +ident-known+)
    (setf (aobj.identify object) flag)
    flag))

(defmethod learn-about-object! (player (object object-kind) (what (eql :known)))
  (declare (ignore player))
  (error "Learn :known must be called on an active-object.."))

(defmethod is-object-known? ((object active-object))
  (or (bit-flag-set? (aobj.identify object) +ident-known+)
      (and (object.easy-know (aobj.kind object))
	   (object.aware (aobj.kind object)))))

(defmethod get-text-colour ((obj active-object))
  (get-text-colour (aobj.kind obj)))

(defmethod get-text-colour ((kind object-kind))
  (object.text-colour kind))


(defmethod get-okind-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-otype-table var-obj level))
	 (table (gobj-table.obj-table o-table)))
    table))

(defmethod get-okind-alloc-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-otype-table var-obj level))
	 (table (gobj-table.alloc-table o-table)))
    table))

(defmethod get-object-kind ((variant variant) obj)
  "Returns the object-kind for the given obj id."
  (etypecase obj
    (string (gethash obj (variant.objects variant)))
    (symbol (gethash (symbol-name obj) (variant.objects variant)))
    (integer (block foo
	       (loop for x being the hash-values of (variant.objects variant)
		     do
		     (when (eql obj (object.numeric-id x))
		       (return-from foo x))
		     ))
	     )))

(defmethod object.name ((obj active-object))
  (object.name (aobj.kind obj)))

(defmethod get-id ((obj active-object))
  (get-id (aobj.kind obj)))

(defmethod get-power-lvl ((obj active-object))
  (get-power-lvl (aobj.kind obj)))

(defmethod get-power-lvl ((obj object-kind))
  (object.power-lvl obj))


;; sometimes we've assigned a special gfx-sym for that object, check for it
(defmethod gfx-sym ((obj active-object))
  (let ((val (slot-value obj 'gfx-sym)))
    (cond ((positive-integer? val)
	   val)
	  (t
	   (let* ((kind (aobj.kind obj))
		  (flavour (object.flavour kind)))
	     (if flavour
		 (gfx-sym flavour)
		 (gfx-sym kind))))
	  )))

;; sometimes we've assigned a special text-sym for that object, check for it
(defmethod text-sym ((obj active-object))
  (let ((val (slot-value obj 'text-sym)))
    (cond ((positive-integer? val)
	   val)
	  (t
	   (let* ((kind (aobj.kind obj))
		  (flavour (object.flavour kind)))
	     (if flavour
		 (text-sym flavour)
		 (text-sym kind))))
	  )))

(defmethod object.weight ((obj active-object))
  (* (aobj.number obj) (object.weight (aobj.kind obj))))


(defun create-aobj-from-id (id &key (amount 1) (variant *variant*))
  "Creates an active object from object-kind identified by id.
Amount specifies how many objects the active-object is, e.g for arrows.
Uses *VARIANT*."
  (let* ((kind (get-object-kind variant id))) ;; fix later
    (unless kind
      (return-from create-aobj-from-id nil))
    (create-aobj-from-kind kind :amount amount :variant variant)))


(defun create-aobj-from-kind-num (num &key (amount 1) (variant *variant*))
  "This is a hackish function which is backward compatible
with k-info.txt numbers. NUM is the numeric id."
  (create-aobj-from-id num :amount amount :variant variant))

(defun create-aobj-from-kind (kind &key (amount 1) (variant *variant*))
  "Creates an aobj from a given kind.  Uses *VARIANT*."
  (let ((obj (produce-active-object variant kind)))
    ;; assume dice
    (setf (aobj.number obj)
	  (cond ((and (stringp amount) (position #\d amount))
		 (parse-and-roll-dice amount))
		((stringp amount)
		 (parse-integer amount))
		((and (numberp amount) (plusp amount))
		 amount)
		(t
		 (warn "Invalid amount ~s for object ~s, assuming 1 instead."
		       amount (object.name kind))
		 1)))

    (when-bind (create-effect (get-object-effect variant obj :create))
      
      (assert (and (effect-entry-p create-effect)
                   (functionp (effect-entry-fun create-effect))))
      ;;  ignore retval so far
      (funcall (effect-entry-fun create-effect) obj))

;;    (trigger-event obj :on-create (list nil nil))
    (activate-object obj)
    obj))

(defmethod trigger-event ((obj active-object) event arg-list)
  "trigger any kind events first (add this obj as last argument).. then active object"
  (trigger-event (aobj.kind obj) event (append arg-list (list obj)))
  (apply-event event (aobj.events obj) arg-list))

(defmethod trigger-event ((obj object-kind) event arg-list)
  "trigger events registered for the kind."
  (apply-event event (object.events obj) arg-list))
  

(defun write-pluralised-string (stream plural-string number &key (flavour nil) (ident nil) (actual-name nil)
				(numeric-prefix t))
  (declare (type u16b number)
	   ;;(type simple-base-string plural-string)
	   )

  (assert (or (eq nil flavour) (typep flavour 'flavour)))
  
  (let ((plural (> number 1))
	(counter 0))
    (declare (type u16b counter))

    ;; fix this to jump right whatever happens!
    (let ((article? (eql (schar plural-string counter) #\&)))
      (when article?
	(incf counter 2)) ;; skip ampersand plus space
      (cond ((eq numeric-prefix nil)
	     nil) ;; nothing
	    ((<= number 0)
	     (write-string "no more " stream))
	    ((> number 1)
	     (format stream "~d " number))
	    
	    ;; did we ask for a/an ?
	    (article?
	     ;; should take into account flavour! should also check length
	     (let ((next-char (schar plural-string counter)))
	       (when (and (eql next-char #\#) flavour) ;; use flavour if we have a flavour sign
		 (setf next-char (schar (flavour.name flavour) 0)))
	       ;;(format t "~&Checking ~s for vowel in '~a'~%" next-char plural-string)
	       (cond ((find next-char '(#\a #\e #\i #\o #\u #\y))
		      (write-string "an " stream))
		     (t
		      (write-string "a " stream)))
	       ))
	    
	    (t nil)))
	   
    
    (loop for i of-type u16b from counter below (length plural-string)
	  for x = (schar plural-string i)
	  do
	  (case x
	    (#\~ (when plural
		   ;; hackish
		   (when (find (schar plural-string (1- i)) '(#\h #\s))
		     (write-char #\e stream))
		   (write-char #\s stream)))
	    #||
	    (#\& (if numeric-prefix
		     (if plural
			 (write-string (format nil "~a" number) stream)
			 (if (find (schar plural-string (+ i 2)) '(#\a #\e #\i #\o #\u #\y))
			     (write-string "an" stream)
			     (write-char #\a stream)))
		     ;; wah!
		     ))
	    ||#	    
	    (#\# (when flavour
		   (write-string (flavour.name flavour) stream)
		   (write-char #\Space stream)
		   ))
	    
	    (#\@ (when ident
		   (write-string " of " stream)
		   (write-string actual-name stream)))
	    
	    (otherwise
	     (write-char x stream))))
    ))

;;(trace write-pluralised-string)

(defun plural-name (number name flavour ident actual-name &key numeric-prefix)
  "Returns a name with plurality fixed as in normal Angband.  FIX ME"
  (with-output-to-string (s)
    (write-pluralised-string s name number :flavour flavour :ident ident :actual-name actual-name
			     :numeric-prefix numeric-prefix)))

(defun parse-dmg-desc (desc)
  "Parses a dmg description and returns the values."
  
  (let ((num-dice 0)
	(dice-type 0)
	(tohit-bonus 0)
	(dmg-bonus 0)
	(state :numdice)
	(next-pos 0)
	(warn-missing-comma nil)
	(warn-missing-rpar nil))

    ;;(warn "desc is ~s" desc)
    (loop named the-loop
	  for i from 0
	  for x across desc
	  do
	  (ecase state
	    (:numdice
	     (cond ((or (eql x #\d) (eql x #\D))
		    ;;(warn "D at ~s" i)
		    (if (plusp i)
			(setf num-dice (read-from-string desc nil nil :start 0 :end i))
			(setf num-dice 1))
		    (setf state :dicetype
			  next-pos (1+ i)))))
	    (:dicetype
	     (cond ((and (eql x #\Space)
			 (plusp (- i next-pos)))
		    (setf dice-type (read-from-string desc nil nil :start next-pos :end i))
		    (setf state :tohit
			  next-pos (1+ i)))
		   ((eql x #\()
		    (warn "No space between damage die and bonuses in damage-desc ~s" desc))))
	    
	    (:tohit
	     (cond ((eql x #\()
		    (setf warn-missing-rpar t
			  warn-missing-comma t)
		    (setf next-pos (1+ i)))
		   ((eql x #\,)
		    (setf tohit-bonus (read-from-string desc nil nil :start next-pos :end i))
		    (setf warn-missing-comma nil)
		    (setf state :todmg
			  next-pos (1+ i)))))
	    (:todmg
	     (when (eql x #\))
	       (setf warn-missing-rpar nil)
	       (setf dmg-bonus (read-from-string desc nil nil :start next-pos :end i))
	       (setf next-pos (1+ i)
		     state :quit)
	       (return-from the-loop nil)))
	     
	     ))
    (cond ((eq state :numdice) ;; never even found the 'd'
	   (setf num-dice (read-from-string desc nil nil)
		 dice-type 1))
	  ((eq state :dicetype) ;; didn't find space
	   (setf dice-type (read-from-string desc nil nil :start next-pos)))
	  )

    (cond (warn-missing-comma
	   (warn "Damage description ~s misses comma in tohit/dmg section" desc))
	  (warn-missing-rpar
	   (warn "Damage description ~s misses right paranthesis" desc)))

    (values num-dice dice-type tohit-bonus dmg-bonus)))


(defmethod is-magical? ((obj active-object))
  nil)

(defmethod is-artifact? ((obj active-object))
  nil)

(defmethod is-cursed? ((obj active-object))
  (or (bit-flag-set? (get-sanctity obj) +sanctity-cursed+)
      (bit-flag-set? (get-sanctity obj) +sanctity-heavily-cursed+)
      (bit-flag-set? (get-sanctity obj) +sanctity-perma-cursed+)))
      
(defmethod is-broken? ((obj active-object))
  ;;(bit-flag-set? (aobj.identify obj) +ident-broken+)
  (is-cursed? obj))

(defmethod is-worthless? ((obj active-object))
  (or (not (plusp (object.cost (aobj.kind obj))))
      (is-cursed? obj)))
   

(defmethod uncurse-object! ((obj active-object) power)

  (cond ((eq power :light)
	 (bit-flag-remove! (get-sanctity obj) +sanctity-cursed+))
	((eq power :heavy)
	 (bit-flag-remove! (get-sanctity obj) #.(logior +sanctity-cursed+ +sanctity-heavily-cursed+)))
	(t
	 (warn "Unknown :power argument ~s to uncurse-object!" power)))
  
  (when (is-cursed? obj)
    (return-from uncurse-object! nil))
  
  ;; remove any inscriptions
  (let ((inscr (aobj.inscr obj)))
    (when (or (eq inscr nil)
	      (= (length inscr) 0)
	      (equal inscr "cursed"))
      (setf (aobj.inscr obj) "uncursed")))
  ;; check inscription here
  
  (bit-flag-add! (aobj.identify obj) +ident-sense+)
  
  t)

(defmethod curse-object! ((obj active-object) power)

  (cond ((eq power :light)
	 (bit-flag-add! (get-sanctity obj) +sanctity-cursed+)
	 t)
	((eq power :heavy)
	 (bit-flag-add! (get-sanctity obj) #.(logior +sanctity-cursed+ +sanctity-heavily-cursed+))
	 t)
	(t
	 (warn "Unknown :power argument ~s to uncurse-object!" power)
	 nil)))
    

(defun get-object-list (&key (var-obj *variant*) (level *level*))
  "returns a fresh list.  Remove me!"
  (let ((table (get-okind-table var-obj level)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 #'<
		 :key #'object.numeric-id)))


(defmethod copy-active-object ((variant variant) (obj active-object) &optional (target nil))
  "Copies the given OBJ and returns a new object that is equal."
  
  (let ((new-obj (if target target (produce-active-object variant obj))))
;;    (warn "Old ~s and new ~s" (class-of obj) (class-of new-obj))
    ;; needs improvement
    (dolist (i '(kind inscription number loc-x loc-y identify marked
		 speed-modifier armour-rating armour-modifier
		 damage-dice number-of-damage-dice tohit-modifier
		 damage-modifier))
      ;; doesn't handle shared-structures well
      (setf (slot-value new-obj i) (slot-value obj i)))

    ;; skip contains
    ;; skip events
    new-obj))

(defun is-object-effect? (arg)
  (functionp arg))

;; hack
(defmacro object-effect (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defmacro magic-add (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defmethod get-object-effect ((var variant) (the-object active-object) effect)
  (find effect (object.effects (aobj.kind the-object)) :key #'effect-entry-type))

(defmethod get-object-effect ((var variant) (the-object object-kind) effect)
  (find effect (object.effects the-object) :key #'effect-entry-type))

(defmethod initialise-object-kind! ((var-obj variant) (new-obj object-kind) keyword-args)
  
  ;; hackish, gradually move variant-specific stuff to variant. 

  (let ((id (get-id new-obj)))

    (when-bind (numeric-id (getf keyword-args :numeric-id))
      (cond ((integerp numeric-id)
	     (unless (>= numeric-id 0)
	       (signal-condition 'illegal-object-data :id id :desc "numeric-id negative"))
	     
	     (setf (object.numeric-id new-obj) numeric-id))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on numeric-id data"))))

    (when-bind (flags (getf keyword-args :flags))
      (cond ((consp flags)
	     (when (find '<easy-know> flags)
	       (setf (object.easy-know new-obj) t)
	       (setf flags (remove '<easy-know> flags)))
	     (setf (object.flags new-obj) flags))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on flags data"))))

    
    (when-bind (sort-value (getf keyword-args :sort-value))
      (cond ((integerp sort-value)
	     (unless (>= sort-value 0)
	       (signal-condition 'illegal-object-data :id id :desc "sort-value negative for object-kind."))
	     (setf (object.sort-value new-obj) sort-value))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on sort-value data for obj-kind"))))
    
    (when-bind (power-lvl (getf keyword-args :power-lvl))
      (cond ((integerp power-lvl)
	     (unless (non-negative-integer? power-lvl)
	       (signal-condition 'illegal-object-data :id id :desc "power-lvl negative for object-kind"))
	     (setf (object.power-lvl new-obj) power-lvl))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on power-lvl data for obj-kind"))))


    (when-bind (cost (getf keyword-args :cost))
      (cond ((integerp cost)
	     (unless (non-negative-integer? cost)
	       (signal-condition 'illegal-object-data :id id :desc "cost negative for object-kind"))
	     (setf (object.cost new-obj) cost))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on cost data"))))

    (when-bind (weight (getf keyword-args :weight))
      (cond ((integerp weight)
	     (unless (non-negative-integer? weight)
	       (signal-condition 'illegal-object-data :id id :desc "weight negative"))
	     (setf (object.weight new-obj) weight))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on weight data"))))

    (when-bind (locations (getf keyword-args :locations))
      (cond ((consp locations)
	     (setf (alloc-locations new-obj) locations))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on locations data"))))

    (when-bind (text-colour (getf keyword-args :text-colour))
      (cond ((integerp text-colour)
	     (unless (>= text-colour 0)
	       (signal-condition 'illegal-object-data :id id :desc "text-colour negative"))
	     (unless (<= text-colour +term-l-umber+)
	       (signal-condition 'illegal-object-data :id id :desc "text-colour too high"))
	     (setf (object.text-colour new-obj) text-colour))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Unknown format on text-colour data"))))

    (when-bind (vulnerabilities (getf keyword-args :vulnerabilities))
      (cond ((non-negative-integer? vulnerabilities)
	     (bit-flag-add! (get-vulnerabilities new-obj) vulnerabilities))
	    ((consp vulnerabilities)
	     (dolist (i vulnerabilities)
	       (if (is-legal-element? var-obj i)
		   (bit-flag-add! (get-vulnerabilities new-obj) (get-element-flag var-obj i))
		   (signal-condition 'illegal-object-data :id id
				     :desc (format nil "Not legal element in vulnerabilities list: ~s" i)))
	       ))
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for vulnerabilities list: ~s" vulnerabilities)))))
    
    (when-bind (immunities (getf keyword-args :immunities))
      (cond ((non-negative-integer? immunities)
	     (bit-flag-add! (get-immunities new-obj) immunities))
	    ((consp immunities)
	     (dolist (i immunities)
	       (if (is-legal-element? var-obj i)
		   (bit-flag-add! (get-immunities new-obj) (get-element-flag var-obj i))
		   (signal-condition 'illegal-object-data :id id
				     :desc (format nil "Not legal element in immunities list: ~s" i)))
	       ))
	    
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for immunities: ~s" immunities)))))

    (when-bind (resists (getf keyword-args :resists))
      (cond ((non-negative-integer? resists)
	     (bit-flag-add! (get-resists new-obj) resists))
	    ((consp resists)
	     (dolist (i resists)
	       (if (is-legal-element? var-obj i)
		   (bit-flag-add! (get-resists new-obj) (get-element-flag var-obj i))
		   (signal-condition 'illegal-object-data :id id
				     :desc (format nil "Not legal element in resists list: ~s" i)))
	       ))
	    
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for resists list: ~s" resists)))))
      
    
    (when-bind (ignores (getf keyword-args :ignores))
      (cond ((non-negative-integer? ignores)
	     (bit-flag-add! (get-ignores new-obj) ignores))
	    ((consp ignores)
	     (dolist (i ignores)
	       (if (is-legal-element? var-obj i)
		   (bit-flag-add! (get-ignores new-obj) (get-element-flag var-obj i))
		   (signal-condition 'illegal-object-data :id id
				     :desc (format nil "Not legal element in ignores list: ~s" i)))
	       ))
	    
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for ignores: ~s" ignores)))))

    
    (when-bind (sustains (getf keyword-args :stat-sustains))
      (cond ((consp sustains)
	     (dolist (i sustains)
	       (if (is-legal-stat? var-obj i)
		   (bit-flag-add! (get-stat-sustains new-obj) (stat.bit-flag (get-stat-obj var-obj i)))
		   (signal-condition 'illegal-object-data :id id
				     :desc (format nil "Not legal element in stat-sustains list: ~s" i)))
	       ))
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for stat-sustains list: ~s" sustains)))))

    (when-bind (stat-modifiers (getf keyword-args :stat-modifiers))
      (cond ((consp stat-modifiers)
	     (setf (get-stat-modifiers new-obj) (build-stat-table-from-symlist var-obj stat-modifiers)))
	    ((is-stat-array? var-obj stat-modifiers)
	     (setf (get-stat-modifiers new-obj) stat-modifiers))
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for stat-modifiers list: ~s"
					     stat-modifiers)))))
    
    (when-bind (abilities (getf keyword-args :abilities))
      (cond ((consp abilities)
	     (setf (object.abilities new-obj) abilities))
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for abilities list: ~s" abilities)))))

    
    (when-bind (light-radius (getf keyword-args :light-radius))
      (cond ((non-negative-integer? light-radius)
	     (setf (get-light-radius new-obj) light-radius))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of light-radius unknown."))))

    (when-bind (speed-modifier (getf keyword-args :speed-modifier))
      (cond ((integerp speed-modifier)
	     (setf (object.speed-modifier new-obj) speed-modifier))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of speed-radius unknown."))))
    
    (when-bind (armour-rating (getf keyword-args :armour-rating))
      (cond ((non-negative-integer? armour-rating)
	     (setf (get-armour-rating new-obj) armour-rating))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of :armour-rating unknown."))))

    (when-bind (armour-modifier (getf keyword-args :armour-modifier))
      (cond ((integerp armour-modifier)
	     (setf (get-armour-modifier new-obj) armour-modifier))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of :armour-modifier unknown."))))

    (when-bind (damage (getf keyword-args :damage))
      (cond ((positive-integer? damage)
	     (setf (get-damage-dice new-obj) 1
		   (get-number-of-damage-dice new-obj) damage))
	    
	    ((stringp damage)
	     (multiple-value-bind (num type tohit dmg)
		 (parse-dmg-desc damage)
	       (assert (positive-integer? num))
	       (assert (positive-integer? type))
	       (assert (integerp tohit))
	       (assert (integerp dmg))
		       
	       (setf (get-damage-dice new-obj) type
		     (get-number-of-damage-dice new-obj) num
		     (get-tohit-modifier new-obj) tohit
		     (get-damage-modifier new-obj) dmg)))
	    (t
	     (signal-condition 'illegal-object-data :id id :desc "Format of damage unknown."))))


    
    ;; when do we get :events ??
    (when-bind (events (getf keyword-args :events))
      (warn "Found :events in obj-kind ~s" id)
      (setf (object.events new-obj) (get-legal-events events)))

    
    (flet ((possible-add-effect (effect varkey &optional (energy +energy-normal-action+))
	     (when-bind (var (getf keyword-args varkey))
	       (cond ((is-object-effect? var)
		      ;;(warn "Compiling ~s for ~s" effect id)
		      (let ((entry (make-effect-entry :type effect
						    ;;;; somehow allegro trips up when passed a compiled function
						      ;;:fun #+allegro var
						      ;;#-allegro (compile nil var)
						      :fun var
						      :energy-use energy)))
			(pushnew entry (object.effects new-obj) :key #'effect-entry-type)))
		     (t
		      (signal-condition 'illegal-object-data :id id
					:desc (format nil "Unknown effect-value ~s for ~s" var effect)
					))))))
      
      (declare (dynamic-extent #'possible-add-effect))
      
      (possible-add-effect :quaff :on-quaff)
      (possible-add-effect :read :on-read)
      (possible-add-effect :eat :on-eat)
      (possible-add-effect :create :on-create)
      (possible-add-effect :add-magic :on-add-magic)
      (possible-add-effect :wear :on-wear)
      (possible-add-effect :drop :on-drop)
      (possible-add-effect :takeoff :on-takeoff)
      (possible-add-effect :destroy :on-destroy)
      (possible-add-effect :zap :on-zap)
      (possible-add-effect :hit :on-hit)
      (possible-add-effect :miss :on-miss)
      (possible-add-effect :calculate :on-calculate)
      )

    (when-bind (gfx-sym (getf keyword-args :gfx-sym))
      (cond ((non-negative-integer? gfx-sym)
	     (setf (gfx-sym new-obj) gfx-sym))
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for gfx-sym: ~s" gfx-sym))
	     )))
    
    (when-bind (text-sym (getf keyword-args :text-sym))
      (cond ((non-negative-integer? text-sym)
	     (setf (text-sym new-obj) text-sym))
	    (t
	     (signal-condition 'illegal-object-data :id id
			       :desc (format nil "Unknown value for text-sym: ~s" text-sym))
	     )))
        
    new-obj))

#-langband-release
(defmethod initialise-object-kind! :after ((var-obj variant) (new-obj object-kind) keyword-args)
  "Used to verify data after obj-kind has been init'ed."

  (declare (ignore keyword-args))
  (let ((id (get-id new-obj)))
    (unless (listp (alloc-locations new-obj))
      (signal-condition 'illegal-object-data :id id :desc "Non-list locations data for object-kind."))
    (unless (integerp (object.power-lvl new-obj))
      (signal-condition 'illegal-object-data :id id :desc "Non-integer power-lvl for object-kind."))
    (unless (>= (object.power-lvl new-obj) 0)
      (signal-condition 'illegal-object-data :id id :desc "Negative power-lvl for object-kind."))


    (dolist (i (alloc-locations new-obj))
      (unless (consp i)
	(signal-condition 'illegal-object-data :id id :desc "location-data for object-kind not a cons"))
      (unless (and (integerp (car i)) (>= (car i) 0))
	(signal-condition 'illegal-object-data :id id :desc "car-part of location-data not legal"))
      (unless (and (integerp (cdr i)) (>= (cdr i) 0))
	(signal-condition 'illegal-object-data :id id :desc "cdr-part of location-data not legal")))

    ))

  


;; must be fixed!!
(defun define-object-kind (id name &rest keyword-args
			   &key the-kind &allow-other-keys) ;; list should be checked thoroughly!
  "creates and establishes an object corresponding to parameters.  It uses
the *VARIANT* object so it has to be properly initialised."

  (handler-case
      (let* ((var-obj *variant*)
	     (new-obj (produce-object-kind var-obj id name :the-kind the-kind))
	     )

	(when (symbolp id)
	  (warn "Deprecated id for object ~s" id))

	(initialise-object-kind! var-obj new-obj keyword-args)
    
	;; hackish addition to big object-table
	(let ((main-obj-table (variant.objects var-obj))
	      (obj-id (get-id new-obj)))
	  (multiple-value-bind (val found-p)
	      (gethash obj-id main-obj-table)
	    (declare (ignore val))
	    (when found-p
	      (warn "Replacing object with id ~s" obj-id))
	    (setf (gethash obj-id main-obj-table) new-obj)))
    
	;; apply object-filters on the new object.    
	(apply-filters-on-obj :objects var-obj new-obj)
    
	new-obj)
    
    (illegal-object-data (co)
      (warn "Failed to initialise object-kind [~a]: ~a"
	    (illegal-data.id co) (illegal-data.desc co))
      nil)

    ))

    
(defmethod produce-active-object ((variant variant) (obj active-object))
  (produce-active-object variant (aobj.kind obj)))

(defmethod produce-active-object ((variant variant) (okind object-kind))
  "Returns an active-object based on the given okind."

  
  (let* ((wanted-kind (object.the-kind okind))
	 (mapping (when wanted-kind
		    (gethash wanted-kind *obj-type-mappings*)))
	 )

    (let ((new-obj (cond ((and mapping (consp mapping))
			  (make-instance (cdr mapping) :kind okind))
			 (t 
			  (make-instance 'active-object :kind okind)))))
      ;; copy stuff over
      ;;(bit-flag-add! (get-vulnerabilities new-obj) (get-vulnerabilities okind))
      (setf (aobj.speed-modifier new-obj) (object.speed-modifier okind)
	    (get-armour-rating new-obj) (get-armour-rating okind)
	    (get-armour-modifier new-obj) (get-armour-modifier okind)
	    (get-tohit-modifier new-obj) (get-tohit-modifier okind)
	    (get-damage-modifier new-obj) (get-damage-modifier okind)
	    (get-damage-dice new-obj) (get-damage-dice okind)
	    (get-number-of-damage-dice new-obj) (get-number-of-damage-dice okind)
	    )

      (when (find '<curse> (object.flags okind))
	(curse-object! new-obj :light))

      new-obj)))


;; this is a hack
(defmethod produce-active-object ((variant variant) (id string))
  (let ((okind (get-object-kind variant id)))
    (cond ((typep okind 'object-kind)
	   (produce-active-object variant okind))
	  (t
	   (warn "Unable to find object with id ~s" id)
	   nil))))


(defmethod produce-object-kind ((variant variant) id name &key the-kind)
  "Produces a suitable object of type object-kind"

  (assert (or (stringp id) (symbolp id)))

  (let ((key (if (symbolp id) (symbol-name id) id))
	(has-mapping (gethash the-kind *obj-type-mappings*)))
    
    (assert (verify-id key))

    (cond ((consp has-mapping)
	   (make-instance (car has-mapping) :id key :name name
			  :the-kind the-kind))
	  (t
	   (make-instance 'object-kind :id key :name name))
	  )))

(defmethod shared-initialize :after ((instance object-kind) slot-names &rest initargs)
  
  (declare (ignore initargs slot-names))
  
  (when (listp (get-vulnerabilities instance))
    (let ((value 0)
	  (variant *variant*))
      (dolist (i (get-vulnerabilities instance))
	(if (is-legal-element? variant i)
	    (bit-flag-add! value (get-element-flag variant i))
	    (signal-condition 'illegal-object-data :id (get-id instance)
			      :desc (format nil "Not legal element in initform vulnerabilities list: ~s" i))))

      (setf (get-vulnerabilities instance) value)))

  (when (listp (get-ignores instance))
    (let ((value 0)
	  (variant *variant*))
      (dolist (i (get-ignores instance))
	(if (is-legal-element? variant i)
	    (bit-flag-add! value (get-element-flag variant i))
	    (signal-condition 'illegal-object-data :id (get-id instance)
			      :desc (format nil "Not legal element in initform ignores list: ~s" i))))

      (setf (get-ignores instance) value)))
  
  (when (listp (get-resists instance))
    (let ((value 0)
	  (variant *variant*))
      (dolist (i (get-resists instance))
	(if (is-legal-element? variant i)
	    (bit-flag-add! value (get-element-flag variant i))
	    (signal-condition 'illegal-object-data :id (get-id instance)
			      :desc (format nil "Not legal element in initform resists list: ~s" i))))

      (setf (get-resists instance) value)))

  
  instance)
  


(defun define-flavour-type (symbol &key generator-fn gfx-sym text-sym)
  "Defines a flavour-type"
  (let* ((var-obj *variant*)
	 (ft-obj (make-instance 'flavour-type
				:symbol symbol
				:gfx-sym (if (non-negative-integer? gfx-sym)
					     gfx-sym 0)
				:text-sym (if (non-negative-integer? text-sym)
					       text-sym 0)
				:generator-fn generator-fn))
	 (table (variant.flavour-types var-obj)))
    (setf (gethash symbol table) ft-obj)
    ft-obj))

(defun legal-flavour-obj? (flav)
  "Is FLAV a legal flavour object?"
  (typep flav 'flavour))

  
(defun establish-flavour& (flavour-type name &key gfx-sym text-sym)
    
  (let ((flav (make-instance 'flavour :name name))
	(table (flavour-type.table flavour-type))
	(val 0))

    (setf val gfx-sym)
    (cond ((non-negative-integer? val)
	   (setf (gfx-sym flav) val))
	  (t
	   ;;(warn "No gfx-sym set for flavour with name ~s" name)
	   ))

    (setf val text-sym) 
    (cond ((non-negative-integer? val)
	   (setf (text-sym flav) val))
	  (t
	   ;;(warn "No gfx-sym set for flavour with name ~s" name)
	   ))

    (push flav (flavour-type.unused-flavours flavour-type))
    (setf (gethash name table) flav)
    
    flav))

(defun find-flavour-type (variant-obj type)
  "Tries to find given flavour-type in given variant-obj."
  (gethash type (variant.flavour-types variant-obj)))

;;(trace find-flavour-type)

(defun define-basic-flavour (type name &key gfx-sym text-sym)
  "Defines a basic flavour.."
  
  (let ((ft-obj (find-flavour-type *variant* type)))
    (unless ft-obj
      (warn "Unable to find flavour-type ~s" type)
      (return-from define-basic-flavour nil))

    (establish-flavour& ft-obj name
			:gfx-sym gfx-sym
			:text-sym text-sym
			)))
	

#||
(defun use-flavour-table (flavour-to-use used-by &key (variant *variant*))
  "a handy way to re-use a flavour-table for another kind
of objects.  all entries are copied, not shared."
  
  (let* ((var-obj variant)
	 (used-by-type (find-flavour-type var-obj used-by))
	 (type-to-use (find-flavour-type var-obj flavour-to-use))
	 (old-table (flavour-type.table type-to-use))
	 (new-table (flavour-type.table used-by-type)))
	 
    
    (maphash #'(lambda (key val)
		 (setf (gethash key new-table) val))
	     old-table)
    
    ;;(warn "~s will use ~s" used-by-type type-to-use)
    
    used-by-type))
||#

(defmethod flavour-object! ((variant variant) (obj object-kind))
  ;; do nothing
  (warn "Not added flavouring for object-kind ~a" obj)
  nil)

(defun flavour-simple-object-kind! (var-obj kind)
  "This is a helper function that flavours an object-kind where
the flavouring follows a simple pattern."
  (let* ((f-type (gethash (object.the-kind kind) (variant.flavour-types var-obj))))
    (when f-type
      (cond ((flavour-type.generator-fn f-type)
	     (setf (object.flavour kind) (funcall (flavour-type.generator-fn f-type)
						  var-obj kind)))
	    
	    ((consp (flavour-type.unused-flavours f-type))
	     (setf (object.flavour kind) (pop (flavour-type.unused-flavours f-type))))
	    
	    ((null (flavour-type.unused-flavours f-type))
	     (warn "No more unused ~s flavours for ~s" (object.the-kind kind)
		   (get-id kind)))
	    (t
	     (describe f-type)
	     (error "Unable to flavour object kind ~a with ~s" kind (flavour-type.symbol f-type))
	     )))
    
    (assert (legal-flavour-obj? (object.flavour kind)))
    
    kind))


(defmethod get-visual-projectile (obj)
  (declare (ignore obj))
  nil)

(defmethod distribute-flavours! ((var-obj variant))
  ;; do nothing
  nil)

(defun carries-object? (creature obj)
  "Checks if a creature carries an object with a certain id."
  ;; handle '(object "id") case
  (when (and (consp obj) (eq (car obj) 'object))
    (setf obj (second obj)))
  
  (check-type obj string)
  
  (let ((objs (items.objs (aobj.contains (get-creature-inventory creature)))))
    (loop for i from 0
          for x across objs
          do
          (when (typep x 'active-object)
            (when (equal obj (get-id (aobj.kind x)))
              (return-from carries-object? i)))))
  
  nil)

#||
(defun remove-from-inventory! (creature key)
  "Removes an object from a creature's inventory."
  (check-type creature player)
  (let ((pos (has-object? creature key)))
    (when pos
      (item-table-remove! (aobj.contains (get-creature-inventory creature)) pos))))
||#

(defmethod get-stat-sustains ((object active-object))
  (get-stat-sustains (aobj.kind object)))

(defmethod get-light-radius ((object active-object))
  (get-light-radius (aobj.kind object)))

(defmethod get-immunities ((object active-object))
  (get-immunities (aobj.kind object)))

(defmethod get-resists ((object active-object))
  (get-resists (aobj.kind object)))

(defmethod get-ignores ((object active-object))
  (get-ignores (aobj.kind object)))

;;; some predicates
(defmethod resists-element? ((object active-object) element)
  (bit-flag-set? (get-element-flag *variant* element)
		 (get-resists object)))

(defmethod immune-to-element? ((object active-object) element)
  (bit-flag-set? (get-element-flag *variant* element)
		 (logior (get-immunities object)
			 (get-ignores object))))


(defmethod vulnerable-to-element? ((object active-object) element)
  (bit-flag-set? (get-element-flag *variant* element)
		 (get-vulnerabilities object)))


(defmethod resists-element? ((object object-kind) element)
  (bit-flag-set? (get-element-flag *variant* element) (get-resists object)))

(defmethod immune-to-element? ((object object-kind) element)
  (bit-flag-set? (get-element-flag *variant* element)
		 (logior (get-ignores object)
			 (get-immunities object))))

(defmethod vulnerable-to-element? ((object object-kind) element)
  (bit-flag-set? (get-element-flag *variant* element)
		 (get-vulnerabilities object)))

(defmethod create-aobj-from-spec ((variant variant) spec)
  "Creates an object from a spec by guessing wildly."
  (let ((retobj nil))
    (cond ((and (consp spec) (eq (car spec) 'obj))
	   (destructuring-bind (dummy-id &key (type nil) (id nil) (numeric-id nil) (amount 1) (no-magic nil)
					 &allow-other-keys)
	       spec
	     (declare (ignore dummy-id))
	     (when type
	       (warn "Equipment with type-spec ~s, please update to id" spec))
	     (cond ((and id (stringp id))
		    (setf retobj (create-aobj-from-id id :variant variant :amount amount)))
		   ((and numeric-id (numberp numeric-id))
		    (setf retobj (create-aobj-from-kind-num numeric-id :variant variant :amount amount)))
		   (t
		    (warn "Unable to handle obj-spec ~s" spec)))
	     (unless no-magic
	       (apply-magic! variant retobj 1 :allow-artifact nil))
	     ))
		   
	  (t
	   (warn "Don't know how to handle obj-creation from ~s" spec)))
    
    retobj))
