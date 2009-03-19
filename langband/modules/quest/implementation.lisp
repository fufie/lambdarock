;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.quest -*-

#|

DESC: modules/quest/quests.lisp - code to handle quests
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.quest)

(defun %make-init-method (quest-name init)
  (let ((var-arg (first (second init)))
	(quest-arg (second (second init)))
	(giver (third (second init)))
	(taker (fourth (second init))))

    (when (eq *variant-class* nil)
      (error "You need to bind *variant-class* before using DEFQUEST."))

    `(defmethod init-quest ((,var-arg ,*variant-class*) (,quest-arg ,quest-name) ,giver ,taker)
      ,@(cddr init))))

(defun register-quest& (id name)
  ;;(warn "Registering quest ~s ~s" id name)
  (setf (gethash id (variant.quests *variant*)) name))

(defun find-quest (variant id)
  (gethash id (variant.quests variant)))

(defmethod quest-available? ((variant variant) quest quest-giver quest-taker)
  (declare (ignorable quest quest-giver quest-taker))
  nil)

(defmethod quest-status ((variant variant) quest taker)
  (declare (ignorable taker))
  (quest.state quest))

(defmethod init-quest ((variant variant) (quest quest) quest-giver quest-taker)

  ;;(warn "Init ~s" quest)
  (setf (quest.state quest) :active
	(quest.step quest) nil
	(quest.giver quest) quest-giver
	(quest.taker quest) quest-taker)
  
  (when-bind (steps (quest.steps quest))
    (let ((nextq (find-quest variant (first steps))))

      (cond ((typep nextq 'quest)
	     (setf (quest.parent nextq) quest)
	     (init-quest variant nextq quest-giver quest-taker)
	     (setf (quest.step quest) (first steps)))
	    (t
	     (signal-condition 'quest-problem :id (quest.id quest)
			       :desc (format nil "Can't find quest: ~a" (first steps)))))
      ))
  
  quest)


(defmethod advance-quest ((variant variant) (quest string) taker &key to from giver)
  (declare (ignorable taker))

  (when-bind (qobj (find-quest variant quest))
    (advance-quest variant qobj taker :to to :from from :giver giver))
  
  quest)

(defmethod advance-quest ((variant variant) (quest quest) quest-taker &key from to giver)

  (declare (ignorable from to))
  (warn "Advancing ~s" quest)
  (warn "step ~s ~s ~s ~s" (quest.step quest) (quest.steps quest) (quest.state quest) (quest.id quest))

  (let ((cur-step (quest.step quest)))

    (when cur-step
      ;; clean up old subquest
      (let ((curq (find-quest variant cur-step)))
	(cond ((typep curq 'quest)
	       (finish-quest variant curq quest-taker))
	      (t
	       (signal-condition 'quest-problem :id (quest.id quest)
				 :desc "Unable to find current subquest during advance"))))

      (let ((next (next-subquest quest cur-step)))
	(warn "Next from ~s is ~s" cur-step next)
	(when next
	  (let ((nextq (find-quest variant next)))
	    (cond ((typep nextq 'quest)
		   (setf (quest.parent nextq) quest)
		   (init-quest variant nextq (if giver giver (quest.giver quest))
			       quest-taker)
		   (setf (quest.step quest) next))
		  (t
		   (signal-condition 'quest-problem :id (quest.id quest)
				     :desc (format nil "Can't find quest: ~a" next))))))
      
	(unless next
	  (setf cur-step nil))))

    (unless cur-step
      ;; time to finish this quest
      (finish-quest variant quest quest-taker))

    quest))


(defmethod finish-quest ((variant variant) (quest quest) quest-taker)
  (warn "Finish ~s" quest)
  (setf (quest.state quest) :finished)
  
  quest)

(defun doing-quest? (creature id)
  "Returns T if the creature is doing named quest."
  (when-bind (quest (find-quest *variant* id))
    (let ((state (quest-status *variant* quest creature)))
      ;;(warn "~s status ~s" id state)
      (eq state :active))))

(defun done-quest? (creature id)
  "Returns T if the creature has done the quest."
  (when-bind (quest (find-quest *variant* id))
    (let ((state (quest-status *variant* quest creature)))
      ;;(warn "~s status ~s" id state)
      (eq state :finished))))

(defun next-subquest (quest cur-step)
  "Returns id for next subquest, or NIL."
  
  (loop for x on (quest.steps quest)
	do
	(when (equal cur-step (first x))
	  (return-from next-subquest (second x))))
  nil)

#||
(defun add-quest-event (quest condition event)
  
  (when (consp condition)
    (when (eq (car condition) 'on-move-to-coord)
      (let ((x (second condition))
	    (y (third condition)))
	(setf (gethash (cons x y) *coord-quest-events*) (make-coord-quest-event :quest quest :x x :y y :trigger event)))))
  
  (warn "Adding event for ~s" condition)
  nil)
||#