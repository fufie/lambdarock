;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/strategies.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

;;; Things to avoid
(define-strategy "avoid" '<avoid>
  'avoidance-strategy)

;;; Things to fight
(define-strategy "fight" '<fight>
  'fight-strategy)
