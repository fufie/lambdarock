;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: init.lisp - initialisation code
Copyright (c) 2000-2004 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify ;
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or ;
(at your option) any later version.

----

ADD_DESC: Code which makes sure all tables and settings are as they should 
ADD_DESC: at the start.

||#

(in-package :org.langband.engine)

;; hackish
(defun %assign-debian-dirs ()
  (setf *engine-source-dir* "/usr/share/common-lisp/source/langband-engine/")
  #+unix
  (setf *engine-config-dir* "/var/games/langband-engine/")
  #+unix
  (setf *engine-data-dir* "/usr/share/games/langband-data/")
  )

(defun %assign-win-dirs ()
  (let ((dir (concatenate 'string (lbsys/ensure-dir-name (namestring (lbsys/get-current-directory)))
			  "config/")))
    (setf *engine-config-dir* dir)))

(defun load-user-preference-file& (&key (filename "prefs.lisp"))
  "Tries to load the user's preference file in ~/.angband/langband/"
  (let ((wanted-file (merge-pathnames (pathname filename) (home-langband-path)))
	(*package* (find-package :org.langband.engine))
	(*load-verbose* nil))
    (when (probe-file wanted-file)
      (load wanted-file))))


(defun load-user-variant-prefs& (variant &key (filename "prefs.lisp"))
  "Tries to load the user's preference file for given variant in ~/.angband/langband/<variant>/"
  (let ((wanted-file (merge-pathnames (pathname filename) (variant-home-path variant)))
	(*package* (find-package :org.langband.engine))
	(*load-verbose* nil))
    (when (probe-file wanted-file)
      (load wanted-file))))



(defun game-init& (&key
		   (ui "sdl")
		   (window-width :default)
		   (window-height :default)
		   (full-screen :default))
  "This function should be called from the outside to
start the whole show.  It will deal with low-level and
call appropriately high-level init in correct order."

  (setf cl:*random-state* (cl:make-random-state t))
  (setf *visevents* '())
  (vinfo-init&) ;; init line-of-sight arrays

  ;;(lb-ds:init-pq-pool 60) ;; random number
  
  ;; ensure that we always have a valid input-event
  (unless (and *input-event* (input-event-p *input-event*))
    (setf *input-event* (make-input-event :keypress (make-keyboard-event)
					  :mouseclick (make-mouse-event))))

  ;; check gcu thoroughly
  (when (string-equal ui "gcu")
    (setf ;;size :wait
     *screen-height* :wait
     *screen-width* :wait))
  
  (let ((alternative-errors (open #+win32 #p"lb-warn.txt"
				  #-win32 #p"warnings-langband.txt"
				  :direction :output
				  :if-exists :append
				  :if-does-not-exist :create))
	(wanted-width -1)
	(wanted-height -1))
    
    (unwind-protect
	 (let* ((hide-warnings (or #+win32 t
				   #+langband-release t
				   (string-equal ui "gcu")))
		
		(cl:*error-output* (if hide-warnings
				       alternative-errors
				       cl:*error-output*))
		
		(cl:*trace-output* (if hide-warnings
				       alternative-errors
				       cl:*trace-output*))
		(cl:*standard-output* (if hide-warnings
					  alternative-errors
					  cl:*standard-output*))
		
		)
	   

	   ;; fix paths
	   #-langband-development
	   (%assign-debian-dirs)
	   #+win32
	   (%assign-win-dirs)
      
	   ;; time to register our lisp
	   #+(or cmu allegro clisp lispworks sbcl cormanlisp ecl)
	   (org.langband.ffi:c-set-lisp-system! #+cmu 0 #+allegro 1 #+clisp 2 #+lispworks 3
						#+sbcl 4 #+cormanlisp 5 #+openmcl 6 #+ecl 7)
      
	   #-(or cmu allegro clisp lispworks sbcl cormanlisp openmcl ecl)
	   (error "lisp-system ~s unknown for C-side." (lisp-implementation-type))
	   (org.langband.ffi:c-init-frame-system& +max-frames+ +predefined-frames+)

	   ;; let us read what the user prefers
	   (load-user-preference-file&)

	   (when (eq window-width :default)
	     (setf window-width (default-setting "window-width")))
      
	   (when (eq window-height :default)
	     (setf window-height (default-setting "window-height")))
      
	   (when (eq full-screen :default)
	     (setf full-screen (default-setting "full-screen")))

	   (let ((minimum-width +sdl-minimum-window-width+)
		 (minimum-height +sdl-minimum-window-height+)
		 (maximum-width +sdl-maximum-window-width+)
		 (maximum-height +sdl-maximum-window-height+))
      
	     (cond ((and (positive-integer? window-width)
			 (>= window-width minimum-width)
			 (<= window-width maximum-width))
		    (setf wanted-width window-width))
		   ((integerp window-width)
		    (warn "Window width ~s out of bounds, using minimum: ~s." window-width minimum-width)
		    (setf wanted-width minimum-width))
		   (t
		    (warn "Weird window-width, exiting: ~s" window-width)
		    (return-from game-init& nil)))
      
	     (cond ((and (positive-integer? window-height)
			 (>= window-height minimum-height)
			 (<= window-height maximum-height))
		    (setf wanted-height window-height))
		   ((integerp window-height)
		    (warn "Window height ~s out of bounds, using minimum: ~s." window-height minimum-height)
		    (setf wanted-height minimum-height))
		   (t
		    (warn "Weird window-height, exiting: ~s" window-height)
		    (return-from game-init& nil)))
     
	     )
		  
	   (handler-case
	       (let* ( ;; hacks
		      (*screen-width* wanted-width)
		      (*screen-height* wanted-height)

		      #||
		      (:wait :wait)
		      (:800x600 800)
		      (:1024x768 1024)
		      (:1280x1024 1280)))
		      (*screen-height* (ecase size
		      (:wait :wait)
		      (:800x600 600)
		      (:1024x768 768)
		      (:1280x1024 1024)))
		      ||#
		      (theme (find-ui-theme ui)))
	   (unless (typep theme 'ui-theme)
	     (warn "No usable theme found, using defaults."))
	   (install-ui-theme& theme)
	   (setf *current-ui-theme* theme))
	
	     (illegal-ui-theme-data (co)
	       (warn "UI-Theme problems for ~a: ~a" (illegal-data.id co)
		     (illegal-data.desc co))
	       (return-from game-init& nil)))

    #+use-callback-from-c
    (arrange-callbacks)
      
    #+(and cmu use-callback-from-c)
    (pushnew 'arrange-callbacks ext:*after-gc-hooks*)
      
    #+(and sbcl use-callback-from-c)
    (pushnew 'arrange-callbacks sb-ext:*after-gc-hooks*)

    (handler-case
	(let ((flag 0)
	      (retval -1))
	  (when *graphics-supported*
	    (bit-flag-add! flag #x01)) ;; graphics
	  ;; add sound?
	  #-disable-sound
	  (bit-flag-add! flag #x02)

	  (when (eq full-screen t)
	    (bit-flag-add! flag #x10))

	  (setf retval (org.langband.ffi:c-init-c-side& (string ui)
							*engine-source-dir*
							*engine-config-dir*
							*engine-data-dir*
							wanted-width
							wanted-height
							flag)) ;; no debug, possible gfx
	  (cond ((= retval -42)
		 #-use-callback-from-c
		 (play-game&)
		 #+use-callback-from-c
		 (progn
		   (warn "Problems init'ing UI, please check term-window for error-messages")
		   (return-from game-init& nil))
		 )
		((/= retval 0) ;; only success is accepted
		 (warn "Problems init'ing UI, please check term-window for error-messages")
		 (return-from game-init& nil))
		;;(warn "return..")
		))
	    
      (langband-quit ()
	(format t "~&Thanks for helping to test Langband.~2%")
	(org.langband.ffi:c-cleanup-c-side&)
	))
  
    t)
      ;; cleanup
  (close alternative-errors)
  )))

(defun %adjust-screen-size (width height)
  (declare (ignore width height))
  ;; call the update-term-sizes in global.lisp
  ;; but only when we're absolutely sure all terms are properly created!!
  ;;(update-term-sizes!)
  
  ;;(warn "ADJUST DISABLED!!")

  t)
    
#+use-callback-from-c
(defun arrange-callbacks ()
  "Assures that the C-side has necessary callbacks to the Lisp-side."

  ;; not sure if this allegro code is 110% correct
  #+allegro
  (let ((play-ptr  (ff:register-foreign-callable `c-callable-play nil t))
	(size-ptr  (ff:register-foreign-callable `c-callable-resize nil t))
	(mouse-ptr (ff:register-foreign-callable `c-callable-mouseclick nil t))
	)
  (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
  (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
  (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
  )
  
  #+cmu
  (let ((play-ptr  (kernel:get-lisp-obj-address #'play-game&))
	(size-ptr  (kernel:get-lisp-obj-address #'%adjust-screen-size))
	(mouse-ptr (kernel:get-lisp-obj-address #'%mouse-clicked))
	)
  (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
  (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
  (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
  )

  #+sbcl
  (let ((play-ptr  (sb-kernel:get-lisp-obj-address #'play-game&))
	(size-ptr  (sb-kernel:get-lisp-obj-address #'%adjust-screen-size))
	(mouse-ptr (sb-kernel:get-lisp-obj-address #'%mouse-clicked))
	)
    ;;    (warn "setting callbacks ~d ~d" play-ptr size-ptr)
  (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
  (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
  (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
  )

  #+lispworks
  (let ((play-ptr  (fli:make-pointer :symbol-name "LB_PlayGame"))
	(size-ptr  (fli:make-pointer :symbol-name "LB_AdjustSize"))
	(mouse-ptr (fli:make-pointer :symbol-name "LB_MouseClicked"))
	)
    
  (org.langband.ffi:c-set-lisp-callback! "play-game" play-ptr)
  (org.langband.ffi:c-set-lisp-callback! "adjust-size" size-ptr)
  (org.langband.ffi:c-set-lisp-callback! "mouse-clicked" mouse-ptr)
  )
  
  #-(or sbcl cmu allegro lispworks)
  (error "No callback arranged for implementation..")
  
  )

;;; hackish thing to start the game ever so long.
(defun a (&key (ui "sdl")
(gfx nil)
(window-width :default)
(window-height :default)
(full-screen :default))
  
;; to make sure dumps look pretty
(let ((*package* (find-package :org.langband.engine))
      #+(or cmu) (extensions:*gc-verbose* nil)
      #+(or cmu sbcl) (*compile-print* nil)
      )

(unless (or (string-equal "sdl" (string ui))
	    (string-equal "x11" (string ui)))
  (setf gfx nil)) ;; hack      
    
(when gfx
  (setf *graphics-supported* t))
;; still get problems as ffi locks up thread system, but a bit better.
#||
#+lispworks
(mp:process-run-function "langband" '() #'game-init& ui)
#-lispworks
||#
(game-init& :ui ui
	    :window-width window-width
	    :window-height window-height
	    :full-screen full-screen)
;;    (format t "~&Thanks for helping to test Langband.~2%")
))

(defun b (&optional (ui "gcu"))
  ;;(warn "Curses/GCU not supported in this version.")
  (a :ui ui :gfx nil))

(defun c (&key (ui "sdl") (full-screen :default))
  (a :ui ui :gfx t
  :window-width +sdl-minimum-window-width+
  :window-height +sdl-minimum-window-height+
  :full-screen full-screen))

(defun d (&optional (ui "sdl"))
  (a :ui ui :gfx t :window-width 1024 :window-height 768))

(defun e (&optional (ui "sdl"))
  (a :ui ui :gfx t :window-width 1280 :window-height 1024 :full-screen t))

(defun f (&optional (ui "sdl"))
  (a :ui ui :gfx t :window-width 1280 :window-height 1024 :full-screen nil))

(setf (symbol-function 'cl-user::langband)
      #'f)

(setf (symbol-function 'cl-user::gcu-langband)
      #'b)


(setf (symbol-function 'cl-user::gfx-langband)
      #'c)

(setf (symbol-function 'cl-user::sdl-langband)
      #'c)
