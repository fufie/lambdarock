;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.quest -*-

#|

DESC: modules/quest/print.lisp - code to print quest info
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.quest)

(defun print-quests (var-obj player settings)
  (declare (ignore player settings))
  (clear-window *cur-win*)
  (let ((title-row 2)
	(title-col 15)
	(title-attr +term-blue+))
    
    (put-coloured-str! title-attr "Quests" title-col title-row)
    (put-coloured-str! title-attr "======" title-col (1+ title-row))

    (let ((row (+ title-row 3))
	  (dummy-col 9))
      (loop for x being the hash-values of (variant.quests var-obj)
	    do
	    (when (and (eq (quest:quest.state x) :active)
		       (not (quest:quest.parent x)))
	      (put-coloured-str! title-attr (quest:quest.title x) title-col (incf row))
	      (multiple-value-setq (dummy-col row)
		(print-text! title-col (incf row) +term-green+ (quest:quest.desc x) :end-col 45))

	      (when (stringp (quest:quest.step x))
		(when-bind (q (quest:find-quest var-obj (quest:quest.step x)))
		  (when (eq (quest:quest.state q) :active)
		    (put-coloured-str! title-attr (quest:quest.title q) (+ 2 title-col) (incf row))
		    (multiple-value-setq (dummy-col row)
		      (print-text! (+ 2 title-col) (incf row) +term-green+ (quest:quest.desc q) :end-col 45))

		    )))

	      (incf row)
	      )))
    
    (pause-last-line!)
    ))

(defmethod print-object ((inst quest:quest) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~a~) [~a ~a]" (lbsys/class-name inst)
           (quest:quest.id inst) (quest:quest.state inst))
   inst))
