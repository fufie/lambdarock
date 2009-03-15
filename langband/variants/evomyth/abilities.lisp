;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#||

DESC: variants/evomyth/abilities.lisp 
Copyright (c) 2009 - Stig Erik Sandoe

||#

(in-package :org.langband.evomyth)

(defclass evo/ability ()
  (id name key type description power-lvl levels hidden))


(defun define-racial-ability (id name &key type description key power-lvl levels hidden)
  "Defines a racial abilitiy"

  nil)
