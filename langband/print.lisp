;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: print.lisp - various display code that prints various stuff
Copyright (c) 2000-2004 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun define-printfield-handler (key window handler &key hook-on-redraw)
  (let ((info (gethash key *printfield-info*)))
    (unless (field-printer-p info)
      (setf info (make-field-printer :key key)))

    (setf (field-printer.handler info) handler
	  (field-printer.window-key info) window)
    ;;(warn "handling ~s ~s ~s" key window handler)
    (setf (gethash key *printfield-info*) info)

    (when hook-on-redraw
      (pushnew info (gethash hook-on-redraw *printfield-hooks*)))
    
    info))

(defun trigger-printfield-hooks& (variant dungeon player key)
  "Triggers any registered printfield hooks for given key."
  (when-bind (triggers (gethash key *printfield-hooks*))
    ;;(warn "TRIGGERS  ~s" triggers)
    (assert (consp triggers))
    (dolist (trigger triggers)
      (trigger-printfield variant dungeon player trigger))
    t))


(defun trigger-printfield (variant dungeon player printfield)

  (let ((win (get-window (field-printer.window-key printfield))))

    (when (and (is-frame-shown? variant win)
	       (functionp (field-printer.handler printfield))
	       (non-negative-integer? (field-printer.col printfield))
	       (non-negative-integer? (field-printer.row printfield)))
      
      ;; call the handler
      (funcall (field-printer.handler printfield) win
	       (field-printer.col printfield)
	       (field-printer.row printfield)
	       variant player dungeon))))


;; hack
(defmacro printfield-handler (arguments &body body)
  (assert (= (length arguments) 6))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))


(defun register-field-order (window order)
  "Registers the order of fields that need printing for
a certain window. <nothing> is a magic constant."
  (declare (ignorable window))
  (let ((row-counter 0)
	(col-counter 0))
	
    (dolist (i order)
      (let ((the-key nil)
	    (num-rows 1))
	(cond 	      
	      ((consp i)
	       (destructuring-bind (key &key (rows 1)) i
		 (if (or (eq nil key) (eq key '<nothing>))
		     (setf the-key nil
			   row-counter (+ rows row-counter))
		     (setf the-key key
			   num-rows rows))))
	      
	      ((symbolp i)
	       (if (or (eq i nil) (eq i '<nothing>))
		   (setf the-key nil
			   row-counter (+ 1 row-counter))
		   (setf the-key i
			 num-rows 1)))
	      (t
	       (error "Don't know how to treat printfield-order ~s" i)))
	
	(when the-key
	  (let ((info (gethash the-key *printfield-info*)))
	    (unless (field-printer-p info)
	      (setf info (make-field-printer :key the-key)))
	    
	    (assert (non-negative-integer? num-rows))
	    (setf (field-printer.row info) row-counter
		  (field-printer.col info) col-counter)

	    ;;(warn "row is ~s for ~s" row-counter the-key)
	    (incf row-counter num-rows)

	    (setf (gethash the-key *printfield-info*) info)
	    ))))
    
    *printfield-info*))


;; very very inefficient, and wrong.. should use padding
(defun print-number (term colour number padding row col)
  ;; hack
  (let ((win (get-window term)))
    ;;(output-string! win col row colour (format nil "~vd" padding number))
    (win/format win col row colour "~v" padding number) ;; padding number))
    nil))



;; screws up when printing lower values on the character sheet
;; this is also vanilla/ad&d specific stuff
(defun print-stat-value (term colour stat row col)

  (let ((win (get-window term)))
    ;; a bit ugly but we avoid consing
    (cond ((>= stat 118)
	   (output-string! win col row colour "18/")
	   (win/format win (+ col 3) row colour "~d" (- stat 18)))
	  ((> stat 27)
	   (output-string! win col row colour " 18/")
	   (win/format win (+ col 4) row colour "~d" (- stat 18)))
	  ((> stat 18)
	   (output-string! win col row colour " 18/")
	   (win/format win (+ col 4) row colour "0~d" (- stat 18)))
	  ((> stat 9)
	   (win/format win col row colour "    ~d" stat))
	  (t
	   (win/format win col row colour "     ~d" stat)))
    t))
	  
(defmethod print-hunger ((variant variant) (player player) setting)

  (when (basic-frame-shown? variant)
      
    (let* ((win (get-window +charinfo-frame+))
	   (hunger (player.satiation player))
	   (loc (setting-lookup setting "hunger"))
	   (hunger-info (%get-hungerlvl hunger)))
      
      (output-string! win 0 loc (second hunger-info) (third hunger-info))
      
      t)))
    
(defmethod print-basic-frame ((variant variant) dungeon player)
  "Prints the left frame with basic info"

  ;; should we just skip?
  (unless (basic-frame-shown? variant)
    (return-from print-basic-frame nil))
  
  (trigger-printfield-hooks& variant dungeon player '[race])
  (trigger-printfield-hooks& variant dungeon player '[class])
  (trigger-printfield-hooks& variant dungeon player '[level])
  (trigger-printfield-hooks& variant dungeon player '[stats])
  (trigger-printfield-hooks& variant dungeon player '[armour])
  (trigger-printfield-hooks& variant dungeon player '[hp])
  (trigger-printfield-hooks& variant dungeon player '[gold])
  (trigger-printfield-hooks& variant dungeon player '[xp])
  (trigger-printfield-hooks& variant dungeon player '[speed])
  
  (print-depth variant *level* nil)
  t)

    #||
    #+maintainer-mode
    (let ((food-set (slot-value pr-set 'food))
	  (energy-set (slot-value pr-set 'energy)))
      
      (print-field "Food" food-set +charinfo-frame+)
      (print-field "Energy" energy-set +charinfo-frame+)
      
      (print-number +charinfo-frame+ +term-l-green+ (player.satiation player) 5
		     (car food-set) (+ (cdr food-set) 7))
      (print-number +charinfo-frame+ +term-l-green+ (player.energy player) 5
		     (car energy-set) (+ (cdr energy-set) 7))
      )
    ||#

  ;; ADD LATER






#||    
    (let ((s nil))
      (cond ((= speed +speed-base+)
             (setf s "            "))
            ((> speed +speed-base+)
	     (setf s (format nil "Fast (+~d)" (- speed +speed-base+))))
            (t
             (setf s (format nil "Slow (-~d)" (abs (- speed +speed-base+))))
             (setf colour +term-l-umber+)
             ))

      ;;(warn "Printing speed ~s" s)
      (with-frame (+misc-frame+)
	(put-coloured-str! colour s column 0))
      
      )

    t))
    ||#
  
(defun display-player-misc (variant player term settings)
  (declare (ignore term))

  (let* ((the-class (player.class player))
	 (the-lvl (player.power-lvl player))
	 (title (get-title-for-level the-class the-lvl))
	 (offset-x (setting-lookup settings "offset-x" 0))
	 (offset-y (setting-lookup settings "offset-y" 0))
	 (title-attr (setting-lookup settings "title-attr" +term-white+))
	 (value-attr (setting-lookup settings "value-attr" +term-blue+))
	 (title-col  (+ offset-x (setting-lookup settings "title-x" 1)))
	 (title-row  (+ offset-y (setting-lookup settings "title-y" 2))))


    (flet ((print-info (title text row)
	     (put-coloured-str! title-attr title title-col row)
	     (put-coloured-str! value-attr text (+ 11 title-col) row)))

      (print-info "Name" (player.name player)       (+ 0 title-row))
      (print-info "Gender" (get-gender-name player) (+ 1 title-row))
      (print-info "Race" (get-race-name player)     (+ 2 title-row))
      (print-info "Class"  (get-class-name player)  (+ 3 title-row))
      (print-info "Title" title                     (+ 4 title-row))
      
      (print-info "Hitpoints" (format nil "~d/~d" (current-hp player)
			       (maximum-hp player))
		  (+ 5 title-row))
      
      ;; move later, no rush
      (print-info "Mana" (format nil "~d/~d" (current-mana player)
			       (maximum-mana player))
		  (+ 6 title-row))
      
      ;; select better picture later
      (when (use-images?)
	(when-bind (picture (get-character-picture variant player))
	  (let ((pic-col (+ offset-x (setting-lookup settings "picture-x" (+ 22 title-col))))
		(pic-row (+ offset-y (setting-lookup settings "picture-y" title-row))))
	    
	    (paint-gfx-image& picture pic-col pic-row))))
      
      )))

(defmethod display-player-extra ((variant variant) (player player) term settings)

  (let* ((offset-x (setting-lookup settings "offset-x" 0))
	 (offset-y (setting-lookup settings "offset-y" 0))
	 (title-attr (setting-lookup settings "title-attr" +term-white+))
	 (value-attr (setting-lookup settings "value-attr" +term-l-blue+))
	 (badvalue-attr (setting-lookup settings "value-badattr" +term-yellow+))
	 (title-col  (+ offset-x (setting-lookup settings "extra-x" 26)))
	 (title-row  (+ offset-y (setting-lookup settings "extra-y" 3)))
	 (col title-col)
	 (f-col (+ 7 title-col))
	 (row title-row)
	 (cur-xp (player.current-xp player))
	 (max-xp (player.maximum-xp player))
	 (lvl (player.power-lvl player))
	 (misc (player.misc player))
	 )
    

    (put-coloured-str! title-attr "Age" col (+ row 0))
    (put-coloured-str! value-attr (format nil "~4d winters" (playermisc.age misc)) f-col (+ row 0))

    (put-coloured-str! title-attr "Height" col (+ row 1))
    (put-coloured-str! value-attr (format nil "~4d cms" (playermisc.height misc))  f-col (+ row 1))

    (put-coloured-str! title-attr "Weight" col (+ row 2))
    (put-coloured-str! value-attr (format nil "~4d kg" (playermisc.weight misc))  f-col (+ row 2))
    (put-coloured-str! title-attr "Status" col (+ row 3))
    (put-coloured-str! value-attr (format nil "~4d reputation"  (playermisc.status misc))  f-col (+ row 3))

    ;; always in maximize and preserve, do not include

    ;; another area
    (setq col (+ offset-x (setting-lookup settings "elem-x" 1)))
    (setq row (+ offset-y (setting-lookup settings "elem-y" 11)))
    (setq f-col (+ col 9))

    ;; hack, remove later
    (decf row 5)

    (put-coloured-str! title-attr "Level" col (+ row 5))
    (put-coloured-str! (if (>= lvl
			       (player.max-level player))
			   value-attr
			   badvalue-attr)
		       (format nil "~10d" lvl)  f-col (+ row 5))
  
  
    (put-coloured-str! title-attr "Cur Exp" col (+ row 6))
    
    (put-coloured-str! (if (>= cur-xp
			       max-xp)
			   value-attr
			   badvalue-attr)
		       (format nil "~10d" cur-xp) f-col (+ row 6))
    
    (put-coloured-str! title-attr "Max Exp" col (+ row 7))
    
    (put-coloured-str! value-attr
		       (format nil "~10d" max-xp) f-col (+ row 7))
    

    (put-coloured-str! title-attr "Adv Exp" col (+ row 8))
    (cond ((<= (variant.max-charlevel variant) (player.power-lvl player))
	   (put-coloured-str! value-attr "XXXXXXXXXX" f-col (+ row 8)))
	  (t
	   (put-coloured-str! value-attr
			      (format nil "~10d" (aref (player.xp-table player)
						       (player.power-lvl player)))
			      f-col (+ row 8))))
    
    
    (put-coloured-str! title-attr "Gold" col (+ row 9))

    (put-coloured-str! value-attr
		       (format nil "~10d" (player.gold player)) f-col (+ row 9))

    (put-coloured-str! title-attr "Burden" col (+ row 11))
    (let* ((weight (player.burden player))
	   (pound (int-/ weight 10))
	   (kg (floor (* 0.45 pound)))
	   (frac (mod weight 10))
	   (str (format nil "~10d.~d kg" kg frac)))
      (put-coloured-str! value-attr str (- f-col  2) (+ row 11)))

    
    (display-player-combat-ratings variant player term settings)
    
    ))

(defmethod display-player-skills ((variant variant) (player player) term settings)

  (let* ((offset-x (setting-lookup settings "offset-x" 0))
	(offset-y (setting-lookup settings "offset-y" 0))
	(row (+ offset-x (setting-lookup settings "skills-y" 10)))
	(col (+ offset-y (setting-lookup settings "skills-x" 42))))

    (when (integerp term)
      (setf term (aref *windows* term)))
    
    (output-string! term col row +term-l-blue+ "Skill-display not implemented.")))


(defun display-player-stats (variant player term settings)
  ;;  (warn "Displaying character.. ")
  
  (let* ((offset-x (setting-lookup settings "offset-x" 0))
	 (offset-y (setting-lookup settings "offset-y" 0))
	 (col (+ offset-x (setting-lookup settings "stats-x" 42)))
	 (row (+ offset-y (setting-lookup settings "stats-y" 3)))
	 (stats-attr    (setting-lookup settings "stats-attr" +term-white+))
	 (stats-ok-val  (setting-lookup settings "statok-attr" +term-l-green+))
	 (stats-bad-val (setting-lookup settings "statbad-attr" +term-yellow+))
	 
	 ;;	(num-stats 6)
	 ;; more pressing variables
	 (stat-len (variant.stat-length variant))
	 (base (player.base-stats player))
	 ;;	(cur (player.curbase-stats player))
	 (mod (player.modbase-stats player))
	 (active (player.active-stats player))
	 (racial-adding (race.stat-changes (player.race player)))
	 (class-adding (class.stat-changes (player.class player)))
	 (sustains (get-stat-sustains player))
	 )
    ;; labels
    
    (put-coloured-str! stats-attr "  Self" (+ col  5) (1- row))
    (put-coloured-str! stats-attr " RB"    (+ col 12) (1- row))
    (put-coloured-str! stats-attr " CB"    (+ col 16) (1- row))
    (put-coloured-str! stats-attr " EB"    (+ col 20) (1- row))
    (put-coloured-str! stats-attr "  Best" (+ col 24) (1- row))

    
    (dotimes (i stat-len)
      (let ((its-base (gsdfn base i))
	    ;;(its-cur (gsdfn cur i))
	    (its-mod (gsdfn mod i))
	    (its-active (gsdfn active i))
	    
	    (cur-race-add (gsdfn racial-adding i))
	    (cur-class-add (gsdfn  class-adding i))
	    (sustained? (bit-flag-set? sustains (stat.bit-flag (get-stat-obj variant i)))))
	
	(put-coloured-str! stats-attr (get-stat-name-from-num i)
			   col
			   (+ i row))
	
	
	;; base stat
	(print-stat-value term stats-ok-val its-base (+ row i) (+ col 5))
	
	;;	(put-coloured-str! +term-l-green+ (%get-stat its-base)
	;;		       (+ col 5) (+ row i))
	
	
	;; racial bonus
	(put-coloured-str! stats-ok-val (format nil "~3@d" cur-race-add)
			   (+ col 12)
			   (+ row i))
	
	;; class bonus
	(put-coloured-str! stats-ok-val (format nil "~3@d" cur-class-add)
			   (+ col 16)
			   (+ row i))

	;; equipment
	(put-coloured-str! stats-ok-val " +0"
			   (+ col 20)
			   (+ row i))
	

	;; max stat
	(print-stat-value term stats-ok-val its-mod (+ row i) (+ col 24))
	
	;;	(put-coloured-str! +term-l-green+ (%get-stat its-mod)
	;;		       (+ col 24) (+ row i))
	
	;; if active is lower than max
	(when (< its-active its-mod)
	  ;; max stat
	  (print-stat-value term stats-bad-val its-active (+ row i) (+ col 28)))
	
	;;	  (put-coloured-str! +term-yellow+ (%get-stat its-active)
	;;			 (+ col 28) (+ row i))

	(when sustained?
	  (put-coloured-str! stats-attr "S" (+ col 31) (+ row i)))
	
	))
    ))

(defmethod display-creature ((variant variant) (player player) &key mode)

  (declare (ignore mode))
  (let ((term +full-frame+) ;; should be passed from the outside!
	(display-settings (get-setting variant :char-display)))
    (clear-window term +foreground+)
    (display-player-misc   variant player term display-settings)
    (display-player-stats  variant player term display-settings)
    (display-player-extra  variant player term display-settings)
    (display-player-skills variant player term display-settings)
    ))


;; no warning on duplicates
(defun register-help-topic& (variant topic)
  "Registers a help-topic with the variant."
  (when (typep topic 'help-topic)

    (cond ((probe-file (help-topic.data topic))
	   (setf (gethash (help-topic.id topic) (variant.help-topics variant))
		 topic))
	  (t
	   (warn "Unable to find help file ~a." (help-topic.data topic)))
	  )))

(defun %show-help-file (fname)
  (let ((frame-height (get-frame-height))
	(key-read nil))
    (with-open-file (in-str (pathname fname)
			    :direction :input)
      
      ;; hack
      (clear-window *cur-win*)
      (loop named reader
	    for i from 0
	    for y from 2
	    for str = (read-line in-str nil 'eof)
	    do
	    (when (eq str 'eof)
	      (return-from reader nil))
	    (when (stringp str)
	      ;; can be too long, check later
	      (put-coloured-str! +term-white+ str 1 y))

	    ;; time to break?
	    (when (and (> i 0) (= 0 (mod i (- frame-height 4))))
	      (setf key-read (pause-at-line! (- frame-height 1)
					     :msg "[Press <space> to continue]"
					     :attr +term-yellow+))
	      (when (eql key-read #\Escape)
		(return-from %show-help-file nil))
	      (clear-window *cur-win*)
	      (setf y 1))
	  )
      
      (pause-last-line! :attr +term-yellow+)
      nil)))

(defun display-help-topics (variant title start-row)
  "Displays help-topics to screen and asks for input on further help."
  (let ((topics (variant.help-topics variant))
	(title-len (length title)))

    (flet ((show-title ()
	     (put-coloured-str! +term-l-blue+ title 12 start-row)
	     (put-coloured-str! +term-l-blue+ (make-string title-len :initial-element #\=)
				12 (1+ start-row)))
	   (show-entries ()
	     (loop for cnt from 3
		   for i being the hash-values of topics
		   do
		   (let ((key (help-topic.key i)))
		     (put-coloured-str! +term-l-green+ (format nil "~a." key) 3 (+ start-row cnt))
		     (put-coloured-str! +term-l-green+ (help-topic.name i)    6 (+ start-row cnt))
		     )))
	   (get-valid-key ()
	     (put-coloured-str! +term-l-blue+ "-> Please enter selection (Esc to exit): " 3 20)
	     (read-one-character)))

      (loop
       (clear-window *cur-win*)
       (show-title)
       (show-entries)
       (let ((key (get-valid-key)))
	 (loop for i being the hash-values of topics
	       ;;for topic-key = (help-topic.key i)
	       for topic-data = (help-topic.data i)
	       do
	       (when (eql key (help-topic.key i))
		 (cond ((stringp topic-data)
			(%show-help-file topic-data))
		       (t
			(warn "Unable to show help ~s" topic-data))
		       )))
	 (when (eql key +escape+)
	   (return-from display-help-topics nil)))
       ))))


(defmethod print-resistance-table ((var-obj variant) (player player) settings)
;;  (declare (ignore player))
    (clear-window *cur-win*)

    (let ((title-attr (setting-lookup settings "title-attr" +term-l-blue+))
	  (unres-attr (setting-lookup settings "unres-attr" +term-l-red+))
	  (res-attr   (setting-lookup settings "res-attr"   +term-l-green+))
	  (title-col  (setting-lookup settings "title-x"    2))
	  (title-row  (setting-lookup settings "title-y"    0))
	  (list-col   (setting-lookup settings "list-x"     1))
	  (list-row   (setting-lookup settings "list-y"     3)))

    (put-coloured-str! title-attr "RESISTANCES" title-col title-row)
    (put-coloured-str! title-attr "===========" title-col (1+ title-row))

    (let ((elms (variant.elements var-obj))
	  ;;(resists (player.resists player))
	  (row list-row))

      (put-coloured-str! title-attr (format nil "~13a ~14a ~7a" "Name" "Symbol" "Idx")
			 (+ 2 list-col) row)
      (incf row)
			 
      (dolist (i elms)
	(let* ((idx (element.number i))
	       (str (format nil "~13a ~14a ~7s" (element.name i) (symbol-name (element.symbol i)) idx)))
	  (cond ((resists-element? player i)
		 (put-coloured-str! res-attr "*" list-col row)
		 (put-coloured-str! res-attr str (+ 2 list-col) row))
		(t
		 (put-coloured-str! unres-attr str (+ 2 list-col) row)))
	  
	  (incf row)))

      (pause-last-line!)
      )))

(defmethod print-misc-info ((variant variant) (player player) setting)
  (declare (ignore setting))
  (clear-window *cur-win*)
  (put-coloured-str! +term-l-blue+ "MISC INFO" 2 0)
  (put-coloured-str! +term-l-blue+ "=========" 2 1)

  (put-coloured-str! +term-l-green+ "Height:" 2 3)
  (put-coloured-str! +term-yellow+ (format nil "~5d" 0) 12 3)
;;  (put-coloured-str! +term-l-green+ "Weight:" 2 4)
;;  (put-coloured-str! +term-yellow+ (format nil "~5d" (creature.weight player)) 12 4)
  (pause-last-line!)
  )
