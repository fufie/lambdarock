;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#||

DESC: variants/evomyth/magic.lisp - spell-effects
Copyright (c) 2003 - Eugene Zaikonnikov, Stig Erik Sand�

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.evomythy)

(defclass evo/magic-effect ()
  (id name technique domain))

(defclass evo/spell ()
  ((id     :accessor spell.id)
   (name   :accessor spell.name)
   (desc   :accessor spell.desc)
   (effect :accessor spell.effect)
   ))

(defclass evo/technique ()
  ((key  :accessor tech.key)
   (name :accessor tech.name)
   (desc :accessor tech.desc)
   ))

(defclass evo/domain ()
  ((key  :accessor domain.key)
   (name :accessor domain.name)
   (desc :accessor domain.desc)
   ))

(defclass evo/rune-material ()
  ((key   :accessor rune-material.key)
   (bonus :accessor rune-material.bonus)
   ))

(defclass evo/effect-zone ()
  ((key  :accessor effect-zone.key)
   (name :accessor effect-zone.name)
   (desc :accessor effect-zone.desc)
   ))
  

;; move these later
(defvar *techniques* '())
(defvar *domains* '())
(defvar *rune-materials* '())
(defvar *effect-zones* '())
(defvar *spells* '())
(defvar *domain-effects* '())

(defun get-technique (key)
  (find key *techniques* :key #'tech.key))

(defun get-domain (key)
  (find key *domains* :key #'domain.key))

(defun get-rune-material (key)
  (find key *rune-materials* :key #'rune-material.key))

(defun get-effect-zone (key)
  (find key *effect-zones* :key #'effect-zone.key))

(defun get-spell (key)
  (find key *spells* :key #'spell.id :test #'equal))



(defun (setf get-technique) (value key)
  (assert (eq key (tech.key value)))
  (push value *techniques*))

(defun (setf get-domain) (value key)
  (assert (eq key (domain.key value)))
  (push value *domains*))

(defun (setf get-rune-material) (value key)
  (assert (eq key (rune-material.key value)))
  (push value *rune-materials*))

(defun (setf get-effect-zone) (value key)
  (assert (eq key (effect-zone.key value)))
  (push value *effect-zones*))

(defun (setf get-spell) (value key)
  (assert (equal key (spell.id value)))
  (push value *spells*))


(defun define-technique (key desc)
  (let ((tech (make-instance 'evo/technique)))

    (setf (tech.key tech) key
	  (tech.desc tech) desc)

    (setf (tech.name tech) (string-downcase (symbol-name key)))
    
    (setf (get-technique key) tech)
    tech))

(defun define-domain (key desc)
  (let ((dom (make-instance 'evo/domain)))

    (setf (domain.key dom) key
	  (domain.desc dom) desc)

    (setf (domain.name dom) (string-downcase (symbol-name key)))

    (setf (get-domain key) dom)
    dom))
  

(defun define-rune-material (key)
  (let ((dom (make-instance 'evo/rune-material)))

    (setf (rune-material.key dom) key
	  (rune-material.bonus dom) 1)

    (setf (get-rune-material key) dom)
    dom))

(defun define-effect-zone (key desc)
  (let ((dom (make-instance 'evo/effect-zone)))

    (setf (effect-zone.key dom) key
	  (effect-zone.desc dom) desc)

    (setf (effect-zone.name dom) (string-downcase (symbol-name key)))

    (setf (get-effect-zone key) dom)
    dom))
 

(defun define-spell (name combinations desc)
  (let ((spell (make-instance 'evo/spell)))

    (check-type name string)
    (check-type desc string)
    
    (setf (spell.name spell) name
	  (spell.id spell) name ;; not good
	  (spell.desc spell) desc)
    
    (check-type combinations cons)

    (assert (= (length combinations) 2))

    (assert (get-technique (first combinations)))
    (assert (get-domain (second combinations)))
    
    
    (setf (get-spell name) spell)

    spell))

(defun define-domain-effect (effect domain)
  (declare (ignorable effect domain))
  nil)
