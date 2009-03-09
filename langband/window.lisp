;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: window.lisp - code for the window system
Copyright (c) 2002-2004 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun is-window? (obj)
  "Checks if the given object is a window."
  (typep obj 'window))

(defun get-window (key)
  "Returns a WINDOW object for a given key, or will signal an ERROR"
  (cond ((is-window? key)
	 key)
	((non-negative-integer? key)
	 (aref *windows* key))
	(t
	 (error "Unknown key ~s for GET-WINDOW." key))
	))
 
(defun establish-data-in-window (win)
  "Makes the datastructures internally."
  (when win
    (let ((width (window.width win))
	  (height (window.height win)))
      
      (setf (window.data win) (make-array (list +num-gfx-layers+ width height)
					  :element-type 'u32b :initial-element 0))
      (setf (window.flagmap win) (make-array (list width height)
					     :element-type 'u32b :initial-element 0)) 
      win)))

(defun create-window (id num-id width height)
  (assert (plusp width))
  (assert (plusp height))
  (let ((win (make-instance 'window :id id :num-id num-id :width width :height height)))

    (establish-data-in-window win)
 
    win))


(defun (setf window-coord) (value win layer x y)
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     ;;#+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (declare (type fixnum layer x y))
  (let ((data-arr (window.data win))
	(flag-arr (window.flagmap win)))
    (declare (type (simple-array u32b *) data-arr flag-arr)) ;; may be improved later
    (setf (aref data-arr layer x y) value)
    (setf (aref flag-arr x y) +coord-updated+)
    value))
  

(defun window-coord (win layer x y)
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (declare (type fixnum layer x y))
  (let ((data-arr (window.data win)))
    (declare (type (simple-array u32b *) data-arr)) ;; may be improved later
    (aref data-arr layer x y)))

(defun paint-coord (win x y &optional (flag 0))
  "This function paints a coord to the window.. it has some hackish parts to be able
to do both ascii info, ascii maps and graphics."
  ;; assume sdl-only now
  (declare (type u-fixnum x y))
  (let ((win-num (window.num-id win))
	(bg (window-coord win +background+ x y))
	(dx (* (window.tile-width win) x))
	(dy (* (window.tile-height win) y)))
    (declare (type u32b bg))
    (declare (type fixnum dx dy))
    ;; graphic tiles are simple.. 
    (cond ((or (window.gfx-tiles? win)
	       (/= win-num *map-frame*)) ;; hackish, might work?
	   (org.langband.ffi:c-full-blit win-num dx dy bg flag)
	   (dotimes (i #.(1- +num-gfx-layers+))
	     (setf bg (window-coord win (1+ i) x y))
	     (when (> bg 0)
	       (org.langband.ffi:c-transparent-blit win-num dx dy bg flag))))
	  ;; characters are nasty, only the top one should be done
	  (t
	   ;; first do background
	   (org.langband.ffi:c-full-blit win-num dx dy bg flag)
	   ;; then do only the top-most
	   (loop named paint
		 for i of-type fixnum from #.(1- +num-gfx-layers+) downto 1
		 do
		 (progn
		   (setq bg (window-coord win i x y))
		   (when (> bg 0)
		     ;; ultra-ugly hack!! nasty nasty
		     (if (= win-num *map-frame*) ;; move out of loop later
			 (org.langband.ffi:c-full-blit win-num dx dy bg flag)
			 (org.langband.ffi:c-transparent-blit win-num dx dy bg flag))
		     
		     (return-from paint nil))))
	   ))
  
    (setf (aref (window.flagmap win) x y) 0)))

(defun paint-window (win)
  (when (integerp win)
    (setf win (aref *windows* win)))

  (let ((wid (window.width win))
	(hgt (window.height win))
	(flag +winflag-delay-paint+))
 
    (loop for x from 0 below wid
	  do
	  (loop for y from 0 below hgt
		do
		(paint-coord win x y flag)))
    
    (org.langband.ffi:c-flush-coords! (window.num-id win) 0 0
				      (* (window.tile-width win) wid)
				      (* (window.tile-height win) hgt))
    ))


(defun refresh-window (&optional (the-win nil) (flag 0))
  "Repaints any updated parts of the window."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (let* ((win (etypecase the-win
		(window the-win)
		(integer (if (< the-win 0)
			    *cur-win*
			    (aref *windows* the-win)))))
	 (wid (window.width win))
	 (hgt (window.height win))
	 (anything-painted nil))

    (declare (type fixnum wid hgt))
    
    (loop for x of-type fixnum from 0 below wid
	  do
	  (loop for y of-type fixnum from 0 below hgt
		do
		(when (plusp (aref (window.flagmap win) x y))
		  (setf anything-painted t)
		  (paint-coord win x y flag) ;; implicitly resets flagmap
		  )))
    
    (when (and anything-painted (bit-flag-set? flag +winflag-delay-paint+))
      ;;(warn "Flush all coords in ~s" win)
      (org.langband.ffi:c-flush-coords! (window.num-id win) 0 0
					(* (window.tile-width win) (1- wid))
					(* (window.tile-height win) (1- hgt))))
    ))

(defun flush-window (the-win)
  "Flushes the entire window with force."
    (let* ((win (etypecase the-win
		(window the-win)
		(integer (if (< the-win 0)
			    *cur-win*
			    (aref *windows* the-win)))))
	   (tile-wid (window.tile-width win))
	   (tile-hgt (window.tile-height win)))
      (org.langband.ffi:c-flush-coords! (window.num-id win)
					0 0
					(* (window.width win) tile-wid)
					(* (window.height win) tile-hgt))
      the-win))

(defun flush-coords (win x y w h)
  "Takes as arguments a WINDOW object (not checked), and tile-numbers for
X, Y, W and H (not absolute pixels)."
  (let ((tile-wid (window.tile-width win))
	(tile-hgt (window.tile-height win)))
    (org.langband.ffi:c-flush-coords! (window.num-id win)
				      (* x tile-wid)
				      (* y tile-hgt)
				      (* w tile-wid)
				      (* h tile-hgt))))

(defun clear-coord (win x y)
  "Clears the coord of any values.  Does not force repaint"
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  (let ((coord-table (window.data win))
	(flag-table (window.flagmap win)))
    (declare (type (simple-array u32b *) coord-table flag-table)) ;; may be improved later
    (setf (aref flag-table x y) 1)
    (loop for j of-type fixnum from 0 below +num-gfx-layers+
	  do
	  (setf (aref coord-table j x y) 0))))

(defun clear-row (win x y &optional (len +max-wincol+) (layer -1))
  "x,y are start coordinates for clearn, when LEN is given it refers
to length cleared from x,y  LEN will be chopped if it exceeds boundaries."

  #||
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))
  ||#
  (declare (type fixnum x y))
  (when (integerp win)
    (setf win (aref *windows* win)))
  
  (when (minusp x)
    (setf x 0))
    
  (let ((num-to-del (min len (- (window.width win) x 1)))
	(coord-table (window.data win))
	(flag-table (window.flagmap win))
	(tile-wid (window.tile-width win))
	(tile-hgt (window.tile-height win)))
    
    (declare (type (simple-array u32b *) coord-table flag-table)) ;; may be improved later
    (declare (type fixnum tile-wid tile-hgt))
    ;;(warn "Del ~s" num-to-del)
    (when (plusp num-to-del)
      ;; all layers
      (cond ((or (eq layer nil) (minusp layer))
	     (loop for i from 0 below num-to-del
		   do
		   (loop for j from 0 below +num-gfx-layers+
			 do
			 (setf (aref coord-table j (+ x i) y) 0)
			 (setf (aref flag-table (+ x i) y) 0)))
	     (org.langband.ffi:c-clear-coords! (window.num-id win)
					       (* tile-wid x)
					       (* tile-hgt y)
					       (* tile-wid num-to-del)
					       tile-hgt))
	     
	    ;; just one layer
	    ((< layer +num-gfx-layers+)
	     ;; needs full repaint
	     (let ((flag +winflag-delay-paint+))
	       (loop for i from 0 below num-to-del
		     do
		     (setf (aref coord-table layer (+ x i) y) 0)
		     (paint-coord win (+ x i) y flag))
	       (flush-coords win x y num-to-del 1)))
	     
	    (t
	     (error "Illegal layer ~s given to CLEAR-ROW for window ~s."
		    layer (window.id win))))

      ;;(warn "call for repaint in ~s" win)
      (setf (window.repaint? win) t)

      nil)))

(defun clear-window-from (win row &optional (layer -1))
  "Clears window from given row and downwards.  can be made more efficient."
  (when (integerp win)
    (setf win (aref *windows* win)))

  (loop for i of-type fixnum from row below (window.height win)
	do
	(clear-row win 0 i +max-wincol+ layer))
  nil)

(defun clear-window (the-win &optional (layer -1))
  "Tries to clear the window. If layer is negative, all layers are cleared, otherwise
a specific layer can be cleared."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     #+cmu (ext:inhibit-warnings 3)))

  (let* ((win (cond ((typep the-win 'window)
		     the-win)
		    ((< the-win 0)
		     *cur-win*)
		    (t
		     (aref *windows* the-win))))
	 (coord-table (window.data win))
	 (flag-table (window.flagmap win))
	 (wid (window.width win))
	 (hgt (window.height win)))

    (declare (type (simple-array u32b *) coord-table flag-table)) ;; may be improved later

    (cond ((or (eq layer nil) (minusp layer))
	   ;; all layers?
	   (loop for x of-type fixnum from 0 below wid
		 do
		 (loop for y of-type fixnum from 0 below hgt
		       do
		       (loop for j from 0 below +num-gfx-layers+
			     do
			     (setf (aref coord-table j x y) 0
				   (aref flag-table  x y) 0))))
	   
	   (org.langband.ffi:c-clear-coords! (window.num-id win) 0 0
					     (window.pixel-width win)
					     (window.pixel-height win)
					     ;;(* (window.tile-width win) (window.width win))
					     ;;(* (window.tile-height win) (window.height win))
					     ))
	  
	  
	  ((< layer +num-gfx-layers+) ;; just one, but legal layer
	   (loop for x of-type fixnum from 0 below wid
		 do
		 (loop for y of-type fixnum from 0 below hgt
		       do
		       (setf (aref coord-table layer x y) 0
			     (aref flag-table x y) 0)))
	   ;; force a full repaint
	   (paint-window the-win))
	  
	  (t
	   (error "Illegal layer ~s given to CLEAR-WINDOW for window ~s."
		  layer (window.id win))))
				
    
    nil))

(defun activate-window (win)
  (when-bind (the-win (get-window win))
    (unless (window.disabled? the-win)
      (setf (window.visible? the-win) t)
      t)))

(defun deactivate-window (win)
  (when-bind (the-win (get-window win))
    (unless (window.disabled? the-win)
      (setf (window.visible? the-win) nil)
      t)))


(defvar *win/colour* +term-white+)

(defun win/write-char (win x y val)
  (setf (window-coord win +foreground+ x y) (text-paint-value *win/colour* val))
  (paint-coord win x y +winflag-delay-paint+)
  1)

(defun win/write-str (win x y str)
  (loop for chr across str
	for i from x
	do
	(win/write-char win i y chr))
  (length str))

;; should be enough
(defvar *write-int-dummy* "                                          ")
(defvar *winformat-forced-numbersign* nil)
(defvar *winformat-padchar* #\Space)

;; Borrowed and modified from CMUCL.
(defun win/write-int (win x y int padding)

  (let ((count 0)
	(quotient int)
	(remainder nil)
	(minus-sign nil))

    ;;(warn "num is ~s, pad is ~s" quotient padding)
    (when (minusp quotient)
      (setf quotient (- quotient)
	    minus-sign t))

    ;; eat a padding for sign
    (when (or minus-sign *winformat-forced-numbersign*)
      (when (integerp padding)
	(decf padding)))

    ;;(warn ">num is ~s, pad is ~s" quotient padding)
    
    ;; print number in reverse
    (loop named do-numbers
	  for i from 0
	  do
	  (progn
	    (multiple-value-setq (quotient remainder)
	      (truncate quotient *print-base*))

	    (let ((val (code-char (+ (char-code #\0) remainder))))
	      ;;(warn "assigning val ~s to ~s" val i)
	      (setf (#+sbcl char #-sbcl schar *write-int-dummy* i) val)
	      (incf count))

	    (when (zerop quotient)
	      (return-from do-numbers t))))

    ;;(warn "padding is ~s and count is ~s" padding count)

    (when (integerp padding)
      (when (> padding count)
	(dotimes (i (- padding count))
	  (win/write-char win (+ i x) y *winformat-padchar*))
	(incf x (- padding count))))

    (when (or minus-sign *winformat-forced-numbersign*)
      (if minus-sign
	  (win/write-char win x y #\-)
	  (win/write-char win x y #\+))
      (incf x))
    
    (loop for j from 0
	  for i from (1- count) downto 0
	  for col = (+ x j)
	  do
	  (let ((val (#+sbcl char #-sbcl schar *write-int-dummy* i))) 
	    ;;(warn "writing ~s from ~s to ~s" val i col)
	    (win/write-char win col y val)))

    (if (and (integerp padding) (> padding count))
	padding
	count)))



(defun win/format (win x y colour format-str &rest args)
  "A non-consing format that writes directly to a window.  It understands the format
directives ~a ~d ~~ ~% and ~v  (~v is similar to ~vd in CL:FORMAT and takes two
arguments)."
  
  (let ((*win/colour* colour)
	(col x)
	(row y))
    
    (flet ((output-int (arg padding)
	     (let ((*print-base* 10))
	       (cond ((< arg 0)
		      ;;(when (integerp padding) (decf padding))
		      (incf col (win/write-int win col row arg padding)))
		     (t					  
		      (incf col (win/write-int win col row arg padding))))
	       )))

	 
      (let ((last-char #\a)
	    (arg-iter args))
	(loop for chr across format-str
	      do
	    (cond ((eql last-char #\~)
		   (ecase chr
		     (#\a (let ((arg (car arg-iter)))
			    (etypecase arg
			      (symbol
			       (when (keywordp arg)
				 (incf col (win/write-char win col row #\:)))
			       (incf col (win/write-str win col y (symbol-name arg))))
			      
			      (integer
			       (output-int arg nil))
				    
			      (string
			       (incf col (win/write-str win col y arg)))
			      
			      )
			    (setf arg-iter (cdr arg-iter))))
			  
		     (#\d (let ((arg (car arg-iter)))
			    (etypecase arg
			      (integer
			       (output-int arg nil)))
			    (setf arg-iter (cdr arg-iter))))

		     (#\v (let ((arg (car arg-iter)))
			    (etypecase arg
			      (integer
			       (output-int (cadr arg-iter) arg)))
			    (setf arg-iter (cddr arg-iter))))
		     
		     ;;(#\% (win/write-char win col row #\Newline))
		     (#\~ (incf col (win/write-char win col row #\~))))
		   (setf last-char #\a)	;; dummy
		   )
		  (t
		   (if (eql chr #\~)
		       nil
		       (incf col (win/write-char win col row chr)))
		   (setf last-char chr)))
	    )))

    (flush-coords win x y (- col x -1) 1)
    t))

;;; ==========
;;; selectable-ui-object and button related code

(defun make-selectable-ui-object (char topx topy botx boty
				  &key text text-colour text-colour-hi
				  button-colour button-colour-hi
				  highlighted? tile)
  "Creates and returns a selectable-ui-object which is init'ed."
  (let ((obj (make-instance 'selectable-ui-object)))
    (cond ((characterp char)
	   (setf (selectable.char obj) char))
	  (t
	   (warn "Illegal char ~s in selectable ui-obj." char)))
    
    (cond ((non-negative-integer? topx)
	   (setf (selectable.topx obj) topx))
	  (t
	   (warn "Illegal top x coord ~s in selectable ui-obj, must be positive int." topx)))
    
    (cond ((non-negative-integer? topy)
	   (setf (selectable.topy obj) topy))
	  (t
	   (warn "Illegal top y coord ~s in selectable ui-obj, must be positive int." topy)))
    
    (cond ((and (non-negative-integer? botx)
		(>= botx (selectable.topx obj)))
	   (setf (selectable.botx obj) botx))
	  ;; ie illegal value
	  ((non-negative-integer? botx)
	   (warn "Illegal bottom x coord ~s in selectable ui-obj, must be >= to topx." botx))
	  (t
	   (warn "Illegal bottom x coord ~s in selectable ui-obj, must be positive int." botx)))

    (cond ((and (non-negative-integer? boty)
		(>= boty (selectable.boty obj)))
	   (setf (selectable.boty obj) boty))
	  ;; ie illegal value
	  ((non-negative-integer? boty)
	   (warn "Illegal bottom y coord ~s in selectable ui-obj, must be >= to topy." boty))
	  (t
	   (warn "Illegal bottom y coord ~s in selectable ui-obj, must be positive int." boty)))
    
    (cond ((and (stringp text) (plusp (length text)))
	   (setf (selectable.text obj) text))
	  ((eq text nil))
	  (t
	   (warn "Illegal text ~s for selectable ui-obj, must be a string." text)))
    
    (cond ((is-colour-code? text-colour)
	   (setf (selectable.text-colour obj) text-colour))
	  ((eq text-colour nil))
	  (t
	   (warn "Illegal text-colour ~s for selectable ui-obj, must be a colour-code." text-colour)))

    (cond ((is-colour-code? text-colour-hi)
	   (setf (selectable.text-colour-hi obj) text-colour-hi))
	  ((eq text-colour-hi nil))
	  (t
	   (warn "Illegal text-colour-hi ~s for selectable ui-obj, must be a colour-code." text-colour-hi)))

    (cond ((keywordp button-colour)
	   (setf (selectable.button-colour obj) button-colour))
	  ((eq button-colour nil))
	  (t
	   (warn "Illegal button-colour ~s for selectable ui-obj, must be a keyword." button-colour)))

    (cond ((keywordp button-colour-hi)
	   (setf (selectable.button-colour-hi obj) button-colour-hi))
	  ((eq button-colour-hi nil))
	  (t
	   (warn "Illegal button-colour-hi ~s for selectable ui-obj, must be a keyword." button-colour-hi)))

    (cond ((eq highlighted? t)
	   (setf (selectable.highlighted? obj) t))
	  ((eq highlighted? nil))
	  (t
	   (warn "Illegal value ~s for predicate highlighted?" highlighted?)))
      
    
    (unless (eq tile nil)
      (warn "tiles as selectable ui-objs not implemented."))
    
    
    obj))


(defun buttonify-text (win text-colour text col row button-colour)
  "Prints buttonified text with a certain button-background.
If WIN is not 8x16 tiled it will just work as OUTPUT-STRING!"

  (unless (and (= (window.tile-width win) 8)
	       (= (window.tile-height win) 16))
    (output-string! win col row text-colour text)
    (return-from buttonify-text (values)))

  (let* ((button-colour-base (case button-colour
			       (:blue 0)
			       (:purple 3)
			       (:matte-orange 6)
			       (:matte-brown 9)
			       (:green 30)
			       (:red 33)
			       (:yellow 36)
			       (:beige 39)
			       (otherwise 0)))
	 (button-bg (tile-paint-value +tilefile-buttons-8x16+ (1+ button-colour-base)))
	 (flag +winflag-delay-paint+)
	 (text-len (length text)))
    
    ;; the first marker
    (setf (window-coord win +decor+ (1- col) row)
	  (tile-paint-value +tilefile-buttons-8x16+ button-colour-base))
    (paint-coord win (1- col) row flag)

    ;; the text
    (loop for x across text
	  for i from col
	  do
	  (setf (window-coord win +decor+ i row) button-bg
		(window-coord win +foreground+ i row) (text-paint-value text-colour x))
	  (paint-coord win i row flag))

    ;; the last marker
    (setf (window-coord win +decor+ (+ col text-len) row)
	  (tile-paint-value +tilefile-buttons-8x16+ (+ 2 button-colour-base)))
    (paint-coord win (+ col text-len) row flag)

    ;; flush in the end
    (flush-coords win (1- col) row (+ 2 text-len) 1)
    
    (values)))

(defun buttonify-selectable-ui-object (win obj)
  "buttonifies the given OBJ to the window WIN."
  (let* ((hi? (selectable.highlighted? obj))
	 (text-colour   (if hi?
			    (selectable.text-colour-hi obj)
			    (selectable.text-colour obj)))
	 (button-colour (if hi?
			    (selectable.button-colour-hi obj)
			    (selectable.button-colour obj))))
  
    (buttonify-text win text-colour (selectable.text obj)
		    (selectable.topx obj) (selectable.topy obj)
		    button-colour)))


(defun make-simple-button (char-trigger text x y &key text-colour button-colour)
  "Returns a very simple button with a char-trigger and some text."
  (make-selectable-ui-object char-trigger x y (+ x (length text)) y
			     :text text :text-colour text-colour
			     :button-colour button-colour))

(defun make-note-buttons (win buttons &key text-colour button-colour)
  "Returns a list of buttons suitable for the note-line at the bottom
of the given window. The buttons argument is a list of (char \"text\")"
  
  (let* ((button-space 3)
	 (text-len (loop for i in buttons summing (+ button-space (length (second i)))))
	 (note-row (get-last-window-line win))
	 (note-col (- (int-/ (get-last-window-column win) 2)
		      (int-/ text-len 2)))
	 (alts '())
	 (cur-x note-col)
	 (cur-y note-row))

    
    (loop for button in buttons
	  do
	  (push (make-simple-button (first button) (second button) cur-x cur-y
				    :text-colour text-colour
				    :button-colour button-colour) alts)
	  (incf cur-x (+ button-space (length (second button)))))

    (nreverse alts)))
