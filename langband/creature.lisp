;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: creature.lisp - code for creatures (monsters, players) where shared
Copyright (c) 2003 - Stig Erik Sandoe

||#

(in-package :org.langband.engine)

(defmethod text-sym ((creature creature)) 
  (let ((val (slot-value creature 'text-sym)))
    (cond ((typep val 'animation)
	   (aref (anim.path val) (anim.current val)))
	  
	  ((non-negative-integer? val)
	   val)

	  (t
	   (error "Cannot handle text-sym ~s for ~s" val creature)))))

(defmethod gfx-sym ((creature creature)) 
  (let ((val (slot-value creature 'gfx-sym)))
    (cond ((typep val 'animation)
	   (aref (anim.path val) (anim.current val)))
	  
	  ((non-negative-integer? val)
	   val)

	  (t
	   (error "Cannot handle gfx-sym ~s for ~s" val creature)))))

#||
(defmethod text-sym ((creature active-monster)) 
  (text-sym (amon.kind creature)))

(defmethod gfx-sym ((creature active-monster)) 
  (gfx-sym (amon.kind creature)))
||#
