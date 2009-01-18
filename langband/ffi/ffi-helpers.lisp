;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: ffi/ffi-helpers.lisp - helpers when loading ffi-defs
Copyright (c) 2001-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

(defvar *langband-loaded-libs* '())
  
(defun lb-quickly-quit-game& ()
  "Tries to quit game.."
  #+cmu
  (cl-user::quit)
  #+allegro
  (excl::exit)
  #+sbcl
  (sb-ext:quit)
  #+openmcl
  (cl-user::quit)
  #-(or cmu allegro sbcl openmcl)
  (warn "Can't quit yet.. fix me..")
  (values))

#||
antifuchs:

For those of you with code that needs to work on both old and new
SBCLs, here's the backwards-compatible code that UFFI uses to support
them:

instead of (load-1-foreign filename), use:

              (handler-case (sb-alien::load-1-foreign filename)
                (sb-int:unsupported-operator (c)
                  (if (fboundp (intern "LOAD-SHARED-OBJECT" :sb-alien))
                      (funcall (intern "LOAD-SHARED-OBJECT" :sb-alien) filename)
                      (error c))))
||#
  
(defun load-shared-lib (&key
			(lib "./zterm/lbui.so")
			(key :unknown))
  "Loads the necessary shared-lib."
  #-lispworks
  (declare (ignore key))
  
  (let ((is-there (probe-file lib)))
    (unless is-there
      (warn "Unable to locate dynamic library ~a, please run 'make'."
	    lib)
      (lb-quickly-quit-game&)))
  
  
  #+allegro
  (load lib)
  #+cmu
  (alien:load-foreign lib)
  #+clisp
  nil
  #+lispworks
  (fli:register-module key :real-name lib :connection-style :manual)
  
  ;; was killed summer 2004
  ;; #+sbcl
  ;; (sb-alien:load-foreign lib)
  
  ;; new version
  #+sbcl
  (sb-alien:load-shared-object lib)
  
  #+openmcl
  (ccl:open-shared-library lib)
  #-(or cmu allegro clisp lispworks sbcl openmcl)
  (warn "Did not load shared-library.."))

