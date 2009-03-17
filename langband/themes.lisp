;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: themes.lisp - code to handle themes
Copyright (c) 2003 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defstruct (delayed (:copier nil))
  expr)

(defun lookup-ui-theme-var (theme var)
  "tries to lookup a theme-variable."
  (cond ((eq var 'window.width)
	 (cond ((string-equal (theme.system theme) "gcu")
		(warn "asking for wid, returning for ~s" *screen-width*)
		(if (eq *screen-width* :wait)
		    (make-delayed :expr 'window.width)
		    *screen-width*))
	       
	       ((string-equal (theme.system theme) "sdl")
		*screen-width*)
	       (t
		(error "Uknown system for theme: ~s" (theme.system theme)))))

	((eq var 'window.height)
	 (cond ((string-equal (theme.system theme) "gcu")
		(warn "asking for hgt, returning for ~s" *screen-width*)
		(if (eq *screen-height* :wait)
		    (make-delayed :expr 'window.height)
		    *screen-height*)) ;; replace with a better size a bit later
	       ((string-equal (theme.system theme) "sdl")
		*screen-height*)
	       (t
		(error "Uknown system for theme: ~s" (theme.system theme)))))
	
	((eq var 'gfxtiles.height)
	 32)
	((eq var 'gfxtiles.width)
	 32)
	((eq var 'true)
	 t)
	((eq var 'false)
	 nil)
	((eq var  '+gfxmap-frame+)
	 +gfxmap-frame+)
	((eq var  '+asciimap-frame+)
	 +asciimap-frame+)
	((eq var '+inv-frame+)
	 +inv-frame+)
	((eq var '+misc-frame+)
	 +misc-frame+)
	((eq var '+message-frame+)
	 +message-frame+)
	((eq var '+charinfo-frame+)
	 +charinfo-frame+)
	((eq var '+full-frame+)
	 +full-frame+)
	((eq var '+query-frame+)
	 +query-frame+)
	((eq var '+dialogue-frame+)
	 +dialogue-frame+)
	((eq var '+infodisp-frame+)
	 +infodisp-frame+)
	((eq var '+tiledfields-frame+)
	 +tiledfields-frame+)
	
	(t
	 var)))

;;(trace lookup-ui-theme-var)

(defun fetch-theme-var (theme var-arg)
  "Tries to return a dynamic variable in the theme."

  (let ((win nil))
    ;; first arg should be window
    ;;(warn "looking for ~s" (first var-arg))
    (dolist (i (theme.windows theme))
      ;;(warn "Comparing ~s and ~s" i (first var-arg))
      (when (eq (window.id i) (first var-arg))
	(setf win i)))
    
    (unless win
      ;;(warn "Unable to find window (var ~s)" (first var-arg))
      (return-from fetch-theme-var (make-delayed :expr (cons 'var var-arg))))

    ;;(warn "now look for ~s in ~s" (cdr var-arg) win)
    
    (ecase (second var-arg)
      (tile-height 
       (let ((size (window.tile-height win)))
	 ;;(warn "returning font-size in ~s to be ~s" var-arg size)
	 (if (plusp size)
	     size
	     (make-delayed :expr (cons 'var var-arg)))))
      
      (tile-width
       (let ((size (window.tile-width win)))
	 (if (plusp size)
	     size
	     (make-delayed :expr (cons 'var var-arg)))))

      (height (possible-expand theme (window.pixel-height win)))
      (width (possible-expand theme (window.pixel-width win)))
      (x-offset (possible-expand theme (window.x-offset win)))
      (y-offset (possible-expand theme (window.y-offset win)))
      )))

(defun possible-expand (theme expr)
  "If EXPR is a delayed expression, it tries to expand it, if not it returns the expression."
  (if (delayed-p expr)
      (handle-ui-theme-calculation theme (delayed-expr expr))
      expr))
    

(defun handle-ui-theme-calculation (theme calc)
  "Tries to calculate value for CALC."
  (cond ((integerp calc)
	 calc)
	((integerp (lookup-ui-theme-var theme calc))
	 (lookup-ui-theme-var theme calc))
	((delayed-p calc)
	 (handle-ui-theme-calculation theme (delayed-expr calc)))
	((delayed-p (lookup-ui-theme-var theme calc))
	 (lookup-ui-theme-var theme calc))
	((consp calc)
	 (cond ((find (car calc) '(- + * /))
		(let ((temp-results (mapcar #'(lambda (x) (handle-ui-theme-calculation theme x)) (cdr calc))))
		  (if (every #'integerp temp-results)
		      (reduce (car calc) temp-results)
		      (make-delayed :expr (cons (car calc) temp-results)))
		  ))
	       ((eq (car calc) 'var)
		(fetch-theme-var theme (cdr calc)))
	       (t
		(error "Don't know how to calculate value from ~s" calc))))
	(t
	 (error ">Don't know how to calculate value from ~s" calc))))

;;(trace handle-ui-theme-calculation)

(defun process-subwindow-data! (theme data)
  (unless (nonboolsym? (car data))
    (warn "Illegal subwindow-data, should have specifier first, not ~s"
	  (car data))
    (return-from process-subwindow-data! nil))

  (let ((sub (make-instance 'window :id (car data))))

    (destructuring-bind (&key key x y width height background font tile-width tile-height gfx-tiles? disabled?)
	(cdr data)

      (cond ((numberp key)
	     (setf (window.num-id sub) key))
	    ((and (nonboolsym? key)
		  (numberp (lookup-ui-theme-var theme key)))
	     (setf (window.num-id sub) (lookup-ui-theme-var theme key)))
	    (t
	     (warn "Can't handle key ~s yet" key)))

      (cond ((or (eq disabled? t) (eq disabled? nil))
	     (setf (window.disabled? sub) disabled?))
	    ((or (eq (lookup-ui-theme-var theme disabled?) t)
		 (eq (lookup-ui-theme-var theme disabled?) nil))
	     (setf (window.disabled? sub) (lookup-ui-theme-var theme disabled?)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle disabled? argument ~s" disabled?))))

      
      (cond ((or (nonboolsym? x) (integerp x) (consp x))
	     (let ((val (handle-ui-theme-calculation theme x)))
	       (setf (window.x-offset sub) val)))
	    ((window.disabled? sub)
	     (setf (window.x-offset sub) 0)) ;; dummy
	    (t
	     (warn "Can't handle x ~s yet" x)))

      (cond ((or (nonboolsym? y) (integerp y) (consp y))
	     (let ((val (handle-ui-theme-calculation theme y)))
	       (setf (window.y-offset sub) val)))
	    ((window.disabled? sub)
	     (setf (window.x-offset sub) 0)) ;; dummy
	    (t
	     (warn "Can't handle y ~s yet" y)))

      (cond ((or (nonboolsym? width) (integerp width) (consp width))
	     (let ((val (handle-ui-theme-calculation theme width)))
	       (setf (window.pixel-width sub) val)))
	    ((window.disabled? sub)
	     (setf (window.x-offset sub) 0)) ;; dummy
	    (t
	     (warn "Can't handle width ~s yet" width)))

      (cond ((or (integerp height) (nonboolsym? height) (consp height))
	     (let ((val (handle-ui-theme-calculation theme height)))
	       (setf (window.pixel-height sub) val)))
	    ((window.disabled? sub)
	     (setf (window.x-offset sub) 0)) ;; dummy
	    (t
	     (warn "Can't handle height ~s yet" height)))

      (cond ((eq tile-width nil)
	     nil)
	    ((numberp tile-width)
	     (setf (window.tile-width sub) tile-width))
	    ((numberp (lookup-ui-theme-var theme tile-width))
	     (setf (window.tile-width sub) (lookup-ui-theme-var theme tile-width)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle tile-width ~s" tile-width))))

      (cond ((eq tile-height nil)
	     nil)
	    ((numberp tile-height)
	     (setf (window.tile-height sub) tile-height))
	    ((numberp (lookup-ui-theme-var theme tile-height))
	     (setf (window.tile-height sub) (lookup-ui-theme-var theme tile-height)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle tile-height ~s" tile-height))))

      
      (cond ((eq nil background)
	     nil)
	    ((stringp background)
	     (setf (window.backgroundfile sub) background))
	    ((non-negative-integer? background) ;; this is an offset in background tilefile
	     (setf (window.backgroundfile sub) background))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle background ~s" background))))

      (cond ((eq nil font)
	     nil)
	    ((stringp font)
	     (setf (window.font sub) font))
	    ((and (consp font) (stringp (first font)))
	     (setf (window.font sub) font))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle font ~s" font))))
      
      (cond ((or (eq gfx-tiles? t) (eq gfx-tiles? nil))
	     (setf (window.gfx-tiles? sub) gfx-tiles?))
	    ((or (eq (lookup-ui-theme-var theme gfx-tiles?) t)
		 (eq (lookup-ui-theme-var theme gfx-tiles?) nil))
	     (setf (window.gfx-tiles? sub) (lookup-ui-theme-var theme gfx-tiles?)))
	    (t
	     (signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			       :desc (format nil "Can't handle gfx-tiles? argument ~s" gfx-tiles?))))

      )

    ;; hack
    (when (window.disabled? sub)
      (setf (window.x-offset sub) 0
	    (window.y-offset sub) 0
	    (window.pixel-width sub) 0
	    (window.pixel-height sub) 0))
    
    sub))

(defun process-ui-theme-data! (theme data &optional wanted-name)
  "Tries to parse the given data and set correct values in the ui-theme object
given as argument."
  
  (let ((subwindows '())
	(rest '()))
    
    (loop for i in data
	  do
	  (if (consp i)
	      (push i subwindows)
	      (push i rest)))

    (setf rest (nreverse rest))
    
    ;;(warn "Sub ~s and rest ~s" subwindows rest)

    (destructuring-bind (&key system default-font)
	rest
      (when system
	(setf (theme.system theme) system))
      (when default-font
	(setf (theme.font theme) default-font)))

    (when (and wanted-name (not (string-equal wanted-name (theme.system theme))))
      (return-from process-ui-theme-data! theme))
    
    (dolist (i subwindows)
      (let ((result (process-subwindow-data! theme i)))
	(when (typep result 'window)
	  (push result (theme.windows theme)))))

    ;; make sure all subwindows have a font
    (when (stringp (theme.font theme))
      (dolist (i (theme.windows theme))
	(unless (or (stringp (window.font i))
		    (and (consp (window.font i))
			 (stringp (first (window.font i)))))
	  (setf (window.font i) (theme.font theme)))))

    ;; ensure there are no duplicate keys or names
    (let ((names '())
	  (keys '()))
      (dolist (i (theme.windows theme))
	(if (find (window.id i) names :test #'string-equal)
	    (signal-condition 'illegal-ui-theme-data :id (window.id i)
			      :desc "Duplicate name of window")
	    (push (window.id i) names))
	(if (find (window.num-id i) keys :test #'equal)
	    (signal-condition 'illegal-ui-theme-data :id (window.id i)
			      :desc "Duplicate key of window")
	    (push (window.num-id i) keys))))
    
    theme))

(defun find-ui-theme (system-type)
  "Tries to read in a theme."
  (let ((wanted-file (game-data-path "theme.lisp"))
	(sys-str (string-downcase (string system-type))))

    ;;(warn "Looking for system ~s" sys-str)
    
    (when (probe-file wanted-file) ;; is it there?
      (with-open-file (s wanted-file
			 :direction :input)
	(loop for x = (read s nil 'eof)
	      until (eq x 'eof)
	      do
	      (cond ((and (consp x) (eq (car x) 'ui-theme))
		     (unless (stringp (second x))
		       (signal-condition 'illegal-ui-theme-data :id (second x)
					 :desc "The name/key to the theme is not a string, bailing.")
		       (return-from find-ui-theme nil))

		     (let ((the-theme (make-instance 'ui-theme :key (second x))))
		       (process-ui-theme-data! the-theme (cddr x) system-type)
		       (when (string-equal sys-str (theme.system the-theme))
			 (return-from find-ui-theme the-theme))))

		    (t
		     (signal-condition 'illegal-ui-theme-data :id "<none>"
				       :desc (format nil "Don't know how to handle data ~s" x)))
		    ))

	))
    
    nil))

(defun establish-ui-theme-size-calculations& (theme)
  "Tells the C-side of wanted sizes for subwindows."
  ;; fonts already installed
  (dolist (i (theme.windows theme))
    
    (when (delayed-p (window.x-offset i))
      (setf (window.x-offset i) (possible-expand theme (window.x-offset i))))
    (when (delayed-p (window.y-offset i))
      (setf (window.y-offset i) (possible-expand theme (window.y-offset i))))
    (when (delayed-p (window.pixel-width i))
      (setf (window.pixel-width i) (possible-expand theme (window.pixel-width i))))
    (when (delayed-p (window.pixel-height i))
      (setf (window.pixel-height i) (possible-expand theme (window.pixel-height i))))


    (unless (non-negative-integer? (window.x-offset i))
      (signal-condition 'illegal-ui-theme-data :id (theme.key theme) :desc "x-offset not legal"))
    (unless (non-negative-integer? (window.y-offset i))
      (signal-condition 'illegal-ui-theme-data :id (theme.key theme) :desc "y-offset not legal"))
    
    ;; hack, zero values are allowed when it's disabled
    (let ((min-value (if (window.disabled? i) 0 1)))
      (unless (and (integerp (window.pixel-width i))
		   (>= (window.pixel-width i) min-value))
      (error-condition 'illegal-ui-theme-data :id (theme.key theme)
		       :desc (format nil "pixel-width ~s for ~s not legal"
				     (window.pixel-width i) (window.id i))))
      (unless (and (integerp (window.pixel-height i))
		   (>= (window.pixel-height i) min-value))
	(signal-condition 'illegal-ui-theme-data :id (theme.key theme)
			  :desc (format nil "pixel-height ~s not legal for ~s" (window.pixel-height i) (window.id i))))
      )
    #||
    (warn "Passing ~s ~s ~s ~s ~s" (window.num-id i)
	  (window.x-offset i)
	  (window.y-offset i)
	  (window.pixel-width i)
	  (window.pixel-height i))
    ||#
    
    (org.langband.ffi:c-add-frame-coords! (window.num-id i)
					  (window.x-offset i)
					  (window.y-offset i)
					  (window.pixel-width i)
					  (window.pixel-height i))
    ))


(defun install-ui-theme& (theme)
  "Tries to install given ui-theme as the wanted ui-theme to use."
  (check-type theme ui-theme)
;;  (warn "Installing theme ~s" (theme.key theme))

  (loop ;;for cnt from 0
   for i in (theme.windows theme)
	do
	(progn
	  ;;(warn "Installing ~s" i)
	  ;;(describe i)
	  (org.langband.ffi:c-add-frame! (window.num-id i) (string-downcase (string (window.id i))))

	  (org.langband.ffi:c-add-frame-tileinfo! (window.num-id i)
						  (window.tile-width i) (window.tile-height i))
	  
	  (org.langband.ffi:c-add-frame-gfxinfo! (window.num-id i)
						 (if (window.gfx-tiles? i)
						     1 0))
	  
	  ;; the more complex stuff will be added when things are running
	  
	  ;; shouldn't this work?
	  (setf (aref *windows* (window.num-id i)) i)))
  
  t)
