;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/attitude.lisp - code dealing with npc attitude
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defconstant +attitude-jihad+   -100)
(defconstant +attitude-hated+    -75)
(defconstant +attitude-hostile+  -50)
(defconstant +attitude-negative+ -25)
(defconstant +attitude-neutral+    0)
(defconstant +attitude-positive+  25)
(defconstant +attitude-friendly+  50)
(defconstant +attitude-close+     75)
(defconstant +attitude-worship+  100)

(defconstant +attchange-dishonoured+ -50)
(defconstant +attchange-offended+    -30)
(defconstant +attchange-shocked+     -20)
(defconstant +attchange-annoyed+     -10)
(defconstant +attchange-pleased+     +10)
(defconstant +attchange-happier+     +20)
(defconstant +attchange-impressed+   +30)
(defconstant +attchange-overwhelmed+ +50)




(defun attitude-modifier (npc player)
  (declare (ignorable npc player))
  ;; should check faction-relations, nationality, presence, reputation
  0)

(defun attitude (npc)
  "Returns attitude of npc, or NIL."
  (check-type npc npc)
  (+ (attitude-modifier npc *player*) (npc.attitude npc)))

(defun (setf attitude) (value npc)
  (check-type npc npc)
  (check-type value integer)
  ;; might need to adjust vs attitude-adjustment
  (setf (npc.attitude npc) value))

(defun adjust-attitude! (npc amount)
  (incf (attitude npc) amount))

(defun attitude<= (npc val)
  (check-type npc npc)
  (< (attitude npc) val))

(defun attitude>= (npc val)
  (check-type npc npc)
  (> (attitude npc) val))
