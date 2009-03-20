;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: event.lisp - event-related functionality
Copyright (c) 2002009 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

;;; == Event related code

(defun is-event? (obj)
  "Checks if OBJ is of type L-EVENT."
  (typep obj 'l-event))

(defun register-event& (id event &key (variant *variant*))
  "Registers an event-id and connects it to a function."
  (unless (equal id (event.id event))
    (warn "registration id ~s of event ~s aren't equal" id event)) 
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.event-types variant)))
    (setf (gethash key table) event)))

(defun find-event-for-key (id &key (variant *variant*))
  "Tries to find an event for the given id."
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.event-types variant)))
    (gethash key table)))

(defun make-event (id type function &key (state nil) (return-action :remove-event))
  "Returns an event-object that can be used."
  (check-type return-action return-actions)
  (check-type type event-types)
  (make-instance 'l-event :id id :type type :function function :state state
		 :return return-action))

(defun make-coord-event (id function extra)
  (make-event id :step-on-coord function
	      :state (if (listp extra)
			 extra
			 (list extra))))
 

(defun define-normal-event (dummy-arg id type function &key (variant *variant*))
  "establishes an event basically."
  (declare (ignore dummy-arg))
  
  (let ((the-event (make-event id type function)))
    (register-event& id the-event :variant variant)
    the-event))

(defmethod trigger-event (obj event arg-list)
  (declare (ignore obj event arg-list))
  (values))

(defun apply-event (event-type event-list arg-list)
  "Iterates through event-list and funcalls any events
with given arg-list if any events match."
  (dolist (i event-list)
    (when (eq event-type (event.type i))
      (apply (event.function i) (event.state i) arg-list)
      )))

(defun get-legal-events (event-list)
  "Goes through the list and ensures that all events are
legal, and if they're not they will be replaced with a legal-event
or removed.  Conses up a new list."
  (let ((new-list nil))
    (dolist (i event-list)
      (cond ((typep i 'l-event)
	     (push i new-list))
	    ((or (symbolp i) (stringp i))
	     (let ((find-attempt (find-event-for-key i)))
	       (if (and find-attempt (typep find-attempt 'l-event))
		   (push find-attempt new-list)
		   (warn "Unable to find an event for key ~s" i))))

	    (t
	     (warn "Do not know how to handle possible event ~s" i))))
    (nreverse new-list)))

;;; End event-code
