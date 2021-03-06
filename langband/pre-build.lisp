;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: pre-build.lisp - settings that must be set before build
Copyright (c) 2001-2004 - Stig Erik Sandoe

|#

(in-package :cl-user)

;; should be on in a released source
;;(pushnew :langband-release cl:*features*)

;; this is a hack to get out a working release now
(pushnew :image-support cl:*features*)

;;  #+(or allegro cmu sbcl lispworks)
;;  (pushnew :use-callback-from-c cl:*features*)

;;  #+(or cmu clisp sbcl)
(pushnew :handle-char-as-num cl:*features*) ;; always

;; this one should be turned on when maintainer is debugging
;;  (pushnew :langband-extra-checks cl:*features*)

#+(or cmu sbcl lispworks)
(pushnew :compiler-that-inlines cl:*features*)

(pushnew :debug-ai cl:*features*)

#+clisp
(progn
  (format t "~&Removing some clisp-warnings.. we hope~%")
  ;;(push (pathname "@lisppath@/") *load-paths*)        
  (setq 
   clos::*gf-warn-on-removing-all-methods* nil
   clos::*warn-if-gf-already-called* nil
   clos::*gf-warn-on-replacing-method* nil
   system::*source-file-types* '(".lisp" ".lsp")))

#+cormanlisp
(setq cl::*support-eql-specializers* t)
  
#+ecl
(setq sys:*gc-verbose* nil)

  
#+cmu
(progn
  (setq ext:*gc-verbose* nil
	ext:*byte-compile-default* nil
	cl:*compile-verbose* nil
	ext:*compile-progress* nil
	cl:*compile-print* nil)
  ;; to avoid exit-problems with cmucl on debian
  ;;#+direct-syscall
  ;;(pushnew :disable-sound cl:*features*)
  #+pcl
  (pushnew 'compile pcl::*defclass-times*))

#+sbcl
(progn
  (setq ;;sb-ext:*gc-verbose* nil
	;;sb-ext:*byte-compile-default* nil
	*compile-print* nil
	)
  ;; to avoid exit-problems with sbcl
  ;;(pushnew :disable-sound cl:*features*)
  )

#+win32
(pushnew :disable-sound cl:*features*) ;; will change, but safest now

#+allegro
(progn
  (setf *load-local-names-info* t
	;;(sys:gsgc-switch :print) t
	))

#||
;; this crap is obsolete, make ~/.langband/settings.lisp later
(defun %load-settings-file (fname)
  "Loads settings info."
  (with-open-file (s (pathname fname)
		     :direction :input)
    (loop for x = (read s nil 'eof)
	  until (eq x 'eof)
	  do
	  (unless (consp x)
	    (warn "Unknown setting directive ~s in ~s" x fname))
	  (when (consp x)
	    (case (car x)
	      (sound-use
	       #-clisp
	       (when (eq (second x) 'yes)
		 (pushnew :using-sound *features*)))
	      (environments
	       (dolist (i (cdr x))
		 (when (or (eq i 'x11) (eq i 'sdl) (eq i 'win))
		   (pushnew :image-support *features*))))
	      (otherwise
	       (warn "Unknown setting directive ~s in ~s" x fname))))
	  )))


(ignore-errors
  (%load-settings-file "config/settings.cfg")) ;; fix
||#
