;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.quest -*-

#|

DESC: modules/quest/base.lisp - code to handle quests
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.quest)

(defvar *variant-class* nil)


(defclass quest ()
  ((id    :accessor quest.id    :initform nil :initarg :id
	  :documentation "A string id.")
   (title :accessor quest.title
	  :initform nil
	  :initarg :title
	  :documentation "A title to use when presenting the quest.")
   (desc  :accessor quest.desc  :initform nil
	  :documentation "A description of the quest to put on a quest page.")
   (state :accessor quest.state :initform :not-started
	  :documentation "what is the current state of the quest?")
   (step  :accessor quest.step  :initform :init
	  :documentation "specifies at what step we are at.. :init and :finish being special values.")
   (steps :accessor quest.steps :initform nil
	  :documentation "steps within a quest, typically pointers to subquests.")
   (giver :accessor quest.giver :initform nil
	  :documentation "Who gave this quest.")
   (taker :accessor quest.taker :initform nil
	  :documentation "Who is doing this quest")
   (parent :accessor quest.parent :initform nil
	   :documentation "If it is a subquest, PARENT should point to the parent quest.")
   ))

(define-condition quest-problem (error)
  ((id   :initarg :id   :reader problem.id)
   (desc :initarg :desc :reader problem.desc)))

(defstruct coord-event
  x
  y
  quest
  trigger)

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

(defmacro quest-event (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
               (declare (ignorable ,@arguments))
               ,@body)))
    `(function ,def)))


(defgeneric quest-available? (variant quest quest-giver quest-taker)
  (:documentation "Is a quest avaialble?"))

(defgeneric quest-status (variant quest taker)
  (:documentation "What is the status of a certain quest"))

(defgeneric init-quest (variant quest quest-giver quest-taker)
  (:documentation "Initialise a new quest."))

(defgeneric advance-quest (variant quest taker &key to from giver)
  (:documentation "Move the quest to the next step"))

(defgeneric finish-quest (variant quest quest-taker)
  (:documentation "Finish up a certain quest."))

