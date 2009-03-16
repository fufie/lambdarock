;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: game.lisp - simple load of the game
Copyright (c) 2000-2004, 2009 - Stig Erik Sandoe

|#

(in-package :cl-user)

;; we want to know where we are
(defun %get-default-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (ext:default-directory)
  #+cmu (ext:default-directory)
;;  #+sbcl (sb-ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #+sbcl (truename ".")
  #-(or allegro sbcl clisp cmu cormanlisp lispworks lucid) (truename "."))


(defvar *current-dir* (namestring (%get-default-directory)))

(defvar *variant-to-load* :vanilla)

;; add features we need
(eval-when (:execute :load-toplevel :compile-toplevel)

;;  (pushnew :xp-testing *features*)
  (pushnew :langband-development *features*)

  ;; should be on in a released source
  ;;(pushnew :langband-release *features*)

;;  (pushnew :maintainer-mode *features*)
  #+(or cmu allegro sbcl lispworks clisp openmcl ecl)
  (pushnew :use-asdf *features*)

  )

#-use-asdf
(error "Your lisp-system is not supported, make it use ASDF and edit game.lisp.")

(defvar *normal-opt* '(optimize
		       #+cmu (ext:inhibit-warnings 3)
		       (speed 3)
		       (compilation-speed 0)
		       (safety 1)
		       (debug 1)
		       #+lispworks (fixnum-safety 3)
		       ))

(defvar *dev-opt* '(optimize
		    #+cmu (ext:inhibit-warnings 2)
		    #+sbcl (sb-ext:inhibit-warnings 1)
		    (speed 0)
		    (compilation-speed 3)
		    (safety 3)
		    (debug 3)
		    #+lispworks (fixnum-safety 3)
		    ))

#+langband-release
(proclaim *normal-opt*)
#-langband-release
(proclaim *dev-opt*)

(defvar *asdf-file*
  #+(or ecl cormanlisp) "tools/asdf.lisp"
  #-(or ecl cormanlisp) "tools/asdf")

#+(or ecl sbcl)
(require 'asdf)

#+use-asdf
(unless (find-package :asdf)
  #+lispworks
  (compile-file *asdf-file* :verbose nil)
  (load *asdf-file* :verbose nil)
  )


;; hack!
#+(and cmu use-asdf)
(setf *default-pathname-defaults* (%get-default-directory))
#+(and clisp use-asdf)
(setf *default-pathname-defaults* (%get-default-directory))

 
(defun compile-in-environment (func)
  (let (
	#+(or cmu lispworks sbcl) (*compile-print* nil)
	  #+lispworks (*compile-verbose* nil)
	  #+(or cmu lispworks) (*load-verbose* nil)
	  (*load-print* nil)
	  ;; #+cmu (*error-output* o-str)
	  #+cmu (extensions:*gc-verbose* nil)
	  ;; #+sbcl (sb-ext:*gc-verbose* nil)
	  )
    (funcall func)))

(defun progress-msg (msg)
  (format t "~&~a~%" msg))

#+use-asdf
(defun load-game ()
  "Tries to load the game asdf-style."
  (let ((asdf:*central-registry* (list "../clbuild/source"
				       *default-pathname-defaults* "variants/vanilla/"
				       "variants/contraband/"))
	;;#+lispworks ;; possibly others too
	(asdf::*compile-file-failure-behaviour* :ignore)
	(asdf::*compile-file-warnings-behaviour* :ignore)
	(var *variant-to-load*)
	)

    #+(or ecl)
    (progn
      (load "trivial-features/trivial-features.asd")
      (asdf:oos 'asdf:load-op :trivial-features)
      (load "alexandria/alexandria.asd")
      (asdf:oos 'asdf:load-op :alexandria)
      (load "babel/babel.asd")
      (asdf:oos 'asdf:load-op :babel)
      (load "cffi/cffi.asd")
      (asdf:oos 'asdf:load-op :cffi))
    
    (load "langband-engine.asd")

    (when (or (eq var :evomyth) (eq var :all))
      (load "modules/dialogue/dialogue.asd")
      (load "modules/quest/quest.asd")
      (load "variants/evomyth/evomyth.asd")
      (asdf:oos 'asdf:load-op :evomyth))
    
    (when (or (eq var :contraband) (eq var :all))
      (load "modules/dialogue/dialogue.asd")
      (load "modules/quest/quest.asd")
      (load "variants/contraband/contraband.asd")
      (asdf:oos 'asdf:load-op :contraband))
    
    (when (or (eq var :vanilla) (eq var :all))
      (load "variants/vanilla/langband-vanilla.asd")
      #-ecl
      (asdf:oos 'asdf:load-op :langband-vanilla)
      #+ecl
      (asdf:make-build :langband-vanilla :type :program :epilogue-code '(ext:quit))
      #+sbcl
      (asdf:oos 'asdf:compile-op :langband-vanilla-data))

    (progress-msg "Variants loaded...")
    t))


(compile-in-environment #'load-game)

(in-package :org.langband.engine)
