;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: creature.lisp - code for creatures (monsters, players) where shared
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

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
