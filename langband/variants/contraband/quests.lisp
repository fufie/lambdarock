;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/quests.lisp - code to handle quests
Copyright (c) 2003 - Stig Erik Sandoe

|#

(in-package :org.langband.contraband)

(defstruct con/coord-event
  x
  y
  quest
  trigger)

(defun %make-init-method (quest-name init)
  (let ((var-arg (first (second init)))
	(quest-arg (second (second init)))
	(giver (third (second init)))
	(taker (fourth (second init))))

    `(defmethod init-quest ((,var-arg contraband) (,quest-arg ,quest-name) ,giver ,taker)
      ,@(cddr init))))

;;(trace %make-init-method)

(defmacro defquest (classname superclass &key id title desc steps init)
  (declare (ignorable superclass steps))

  (let ((init-fun (when init (%make-init-method classname init))))
  ;; fix superclass later
  `(eval-when (:execute :load-toplevel :compile-toplevel)
    (defclass ,classname (quest)
      ((id :initform ,id)
       (title :initform ,title)
       (desc :initform ,desc)
       (steps :initform ,steps)
       ))

    ,init-fun
    
    (register-quest& ,id ',classname))))

(defun register-quest& (id name)
  ;;(warn "Registering quest ~s ~s" id name)
  (setf (gethash id (variant.quests *variant*)) name))

(defun find-quest (variant id)
  (gethash id (variant.quests variant)))


;; hack hack hack
(defvar *coord-events* (make-hash-table :test #'equal))

(defun add-quest-event (quest condition event)

  (when (consp condition)
    (when (eq (car condition) 'on-move-to-coord)
      (let ((x (second condition))
	    (y (third condition)))
	(setf (gethash (cons x y) *coord-events*) (make-con/coord-event :quest quest :x x :y y :trigger event)))))
	
  
  (warn "Adding event for ~s" condition)
  nil)

(defun has-object? (creature obj)
  "Checks if the creature has the given object."
  ;; handle '(object "id") case
  (when (and (consp obj) (eq (car obj) 'object))
    (setf obj (second obj)))
  
  (check-type creature player)
  (check-type obj string)
  (let ((objs (items.objs (aobj.contains (get-creature-inventory creature)))))
    (loop for i from 0
	  for x across objs
	  do
	  (when (typep x 'active-object)
	    (when (equal obj (get-id (aobj.kind x)))
	      (return-from has-object? i)))))
  
  nil)

(defun has-gold>= (creature amount)
  "Checks if the creature has gold/florentins more than or equal to amount."
  (check-type creature player)
  (>= (player.gold creature) amount))
  

(defmacro quest-event (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
               (declare (ignorable ,@arguments))
               ,@body)))
    `(function ,def)))

(defmethod on-move-to-coord ((variant contraband) (player player) x y)

  ;; bad consing
  (when-bind (ev (gethash (cons x y) *coord-events*))
    ;;(warn "found coord event ~s" ev)
    (when (functionp (con/coord-event-trigger ev))
      (funcall (con/coord-event-trigger ev) variant (con/coord-event-quest ev) player)))

  (let* ((win (aref *windows* +charinfo-frame+))
	 (row (- (window.height win) 2)))
    (output-string! win 0 row +term-l-blue+ "        ")
    (output-string! win 0 row +term-l-blue+ (format nil "~3d,~3d" (location-x player) (location-y player) )))
    
  
  player)

(defmethod quest-available? ((variant contraband) quest quest-giver quest-taker)
  (declare (ignorable quest quest-giver quest-taker))
  nil)

(defmethod quest-status ((variant contraband) quest taker)
  (declare (ignorable taker))
  (quest.state quest))

#||
(defmethod advance-quest ((variant contraband) quest taker)
  (declare (ignorable taker))
  quest)
||#

(defmethod advance-quest ((variant contraband) (quest string) taker &key to from giver)
  (declare (ignorable taker))

  (when-bind (qobj (find-quest variant quest))
    (advance-quest variant qobj taker :to to :from from :giver giver))
  
  quest)

(defmethod finish-quest ((variant contraband) (quest quest) quest-taker)
  ;;(warn "Finish ~s" quest)
  (setf (quest.state quest) :finished)
  
  quest)


(defmethod init-quest ((variant contraband) (quest quest) quest-giver quest-taker)

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

(defmethod advance-quest ((variant contraband) (quest quest) quest-taker &key from to giver)

  (declare (ignorable from to))
  ;;(warn "Advancing ~s" quest)
  ;;(warn "step ~s ~s ~s ~s" (quest.step quest) (quest.steps quest) (quest.state quest) (quest.id quest))

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
	(setf cur-step nil)))

    (unless cur-step
      ;; time to finish this quest
      (finish-quest variant quest quest-taker))

    quest)))


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


(defun add-to-inventory (creature object &key identified)
  (check-type creature player)
  (let* ((backpack (get-creature-inventory creature))
	 (inventory (aobj.contains backpack)))
    (when identified
      (learn-about-object! creature object :aware)
      (learn-about-object! creature object :known))
    ;; should do error-checking
    (item-table-add! inventory object)
    (update-inventory-row *variant* creature)
    object))

(defun remove-from-inventory (creature key)
  (check-type creature player)
  (let ((pos (has-object? creature key)))
    (when pos
      (item-table-remove! (aobj.contains (get-creature-inventory creature)) pos))))

(defun get-new-object (id)
  (check-type id string)
  (create-aobj-from-id id))

;;(trace init-quest)
;;(trace finish-quest)
;;(trace advance-quest)
;;(trace next-subquest)
