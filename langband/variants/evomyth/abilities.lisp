;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#||

DESC: variants/evomyth/abilities.lisp 
Copyright (c) 2009 - Stig Erik Sandoe

||#

(in-package :org.langband.evomyth)

(defclass evo/ability-group ()
  ((id :initarg :id :initform "" :accessor ability-group.id)
   (name :initarg :name :initform "" :accessor ability-group.name)
   (key :initarg :key :initform nil :accessor ability-group.key)
   (hidden :initarg :hidden :initform nil :accessor ability-group.hidden)
   (abilities :initform (make-hash-table :test #'equal) :accessor ability-group.abilities)))

(defclass evo/ability ()
  ((id :initarg :id :initform "" :accessor ability.id)
   (name :initarg :name :initform "" :accessor ability.name)
   (key :initarg :key :initform nil :accessor ability.key)
   (group-key :initarg :group-key :initform nil :accessor ability.group-key)
   (description :initarg :description :initform "" :accessor ability.description)
   (power-lvl :initarg :power-lvl :initform 0 :accessor ability.power-lvl)
   (levels :initarg :levels :initform nil :accessor ability.levels)
   (excludes :initarg :excludes :initform nil :accessor ability.excludes)
   (hidden :initarg :hidden :initform nil :accessor ability.hidden)))

(defun register-ability-group& (variant group)
  (let ((table (variant.ability-groups variant)))
    (unless table
      (error "Unable to find ability-group in variant object: ~s" variant))
    (setf (gethash (ability-group.id group) table) group
	  (gethash (ability-group.key group) table) group)
    group))

(defun register-ability& (group ability)
  (let ((table (ability-group.abilities group)))
    (unless table
      (error "Unable to find abilities in group object: ~s" group))
    (setf (gethash (ability.id ability) table) ability
	  (gethash (ability.key ability) table) ability)
    ability))

(defun get-ability (variant key)
  "Returns an ability object if it exists, or nil"
  (let ((table (variant.ability-groups variant)))
    ;; might do values twice ue to double registration
    (loop for value being the hash-values of table
	  do (multiple-value-bind (val exists-p)
		 (gethash key (ability-group.abilities value))
	       (when exists-p
		 (return-from get-ability val))))
    (warn "Unable to find ability ~s" key)
    nil))


(defun define-ability-group(id name key &key hidden)
  "Defines an ability group."
  (let ((obj (make-instance 'evo/ability-group :id id :name name :key key)))
    (when hidden
      (setf (ability-group.hidden obj) t))

    ;; register the group
    (register-ability-group& *variant* obj)
    obj))

(defun define-racial-ability (id name &key group description key power-lvl levels excludes obsoletes hidden)
  "Defines a racial abilitiy"

  (let ((obj (make-instance 'evo/ability :id id :name name)))

    (unless (and (stringp id) (plusp (length id)))
      (warn "Not a valid id for ability: ~s/~s" id name))

    (unless (and (stringp name) (plusp (length name)))
      (warn "Not a valid name for ability: ~s/~s" id name))    

    (cond ((and group (symbolp group) (plusp (length (symbol-name group))))
	   (setf (ability.group-key obj) group))
	  (group
	   (warn "Illegal group specification ~s for ability ~s" group id)))

    (when (stringp description)
      (setf (ability.description obj) description))

    (cond ((and key (symbolp key) (plusp (length (symbol-name key))))
	   (setf (ability.key obj) key))
	  (key
	   (warn "Illegal key specification ~s for ability ~s" key id)))

    (cond ((integerp power-lvl)
	   (setf (ability.power-lvl obj) power-lvl))
	  (power-lvl
	   (warn "Power lvl ~s for ability ~s is invalid." power-lvl id)))

    (when levels
      (warn "ABILITY LEVELS not implemented"))

    (when excludes
      (warn "~s excludes ~s" id excludes))

    (when obsoletes
      (warn "~s obsoletes ~s" id obsoletes))

    (when hidden
      (setf (ability.hidden obj) t))
    
    (let ((my-group (gethash group (variant.ability-groups *variant*))))
      (unless my-group
	(warn "Unable to find parent group ~s for ability ~s" group id)
	(return-from define-racial-ability nil))

      (register-ability& my-group obj))

    obj))
