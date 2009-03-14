;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/objects.lisp - various objects
Copyright (c) 2000-2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defconstant +common-backpack-size+ 23)

(define-object-kind "backpack" "backpack"
  :numeric-id 750
  :text-sym (text-paint-value +term-white+ #\&)
  :power-lvl 3
  :weight nil
  :cost 1200
  :on-create #'(lambda (item)
		 (let ((container (make-container +common-backpack-size+)))
		   (setf (aobj.contains item) container)
		   t))
  :the-kind '<container>)
