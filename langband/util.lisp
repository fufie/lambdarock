;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: util.lisp - utility-code dependant on other classes/code
Copyright (c) 2000-2004 - Stig Erik Sandoe

||#

(in-package :org.langband.engine)

(defun %get-legal-letters (active-items)
  "Returns a string with legal letters to select."

  (when (or (eq active-items nil)
	    (= (length active-items) 0)
	    (notany #'identity active-items))
    (return-from %get-legal-letters ""))
  
  (with-output-to-string (str)

    (let ((last-leading nil)
	  (has-been-leaders nil))
      (loop for i from 0
	    for x across active-items
	    do
	    (progn
	    (cond ((and x (not last-leading))
		   (setf last-leading i)
		   (when has-been-leaders
		     (format str ","))
		   (format str "~a" (i2a i)))
		  (x t)
		  
		  ((and (not x) last-leading)
		   (if (= (1- i) last-leading)
		       nil
		       (format str "-~a" (i2a (1- i))))
		   (setf last-leading nil))

		  ((not x)
		   (setf last-leading nil))
		  )
	    (when (and last-leading (not has-been-leaders))
	      (setf has-been-leaders t))
	      )
	    
	    finally
	    (when last-leading
	      (if (= (1- i) last-leading)
		  nil
		  (format str "-~a" (i2a (1- i))))
	      (setf last-leading nil)
	      )
	      
	    ))

    ))

(defun select-displayed-alternative (alternatives)
  "Allows both keyboard and mouse input-selection of a selection-event with
   alternatives being a list of alternatives. An alternative is a 
   selectable-ui-obj with legal char, topx, topy, botx and boty. The returned value is
   the char, or #\* if the right mouse was clicked (might change).  The coords
   refer to tile, not pixel."

  ;; (warn "want alt from ~s" alternatives)
  
  (loop
   (let ((ev (fetch-event *input-event* nil)))
     (when ev
       (cond ((eq (input-event.type ev) :key)
	      ;; fix
	      (return-from select-displayed-alternative (kbd-event.key (input-event.keypress ev))))
	     ((eq (input-event.type ev) :mouse)
	      (let ((m-ev (input-event.mouseclick ev)))
		(when (eq (mouse-event.button m-ev) :right)
		  (return-from select-displayed-alternative #\*))
		(when (eq (mouse-event.button m-ev) :left)
		  (multiple-value-bind (click-win loc-x loc-y)
		      (%locate-mouse-click-window (mouse-event.x m-ev)
						  (mouse-event.y m-ev))
		    
		    ;; ok, if we're in the dialogue-window it might be a click
		    (when (or (= +dialogue-frame+ (window.num-id click-win))
			      ;; some stuff is in full frame too
			      (= +full-frame+ (window.num-id click-win)))
		      (let ((col (int-/ loc-x (window.tile-width click-win)))
			    (row (int-/ loc-y (window.tile-height click-win))))
			;;(warn "checking ~s,~s vs ~s" col row alternatives)

			(loop for x in alternatives
			      do
			      (when (and (>= col (selectable.topx x))
					 (>= row (selectable.topy x))
					 (<= col (selectable.botx x))
					 (<= row (selectable.boty x)))
				(return-from select-displayed-alternative (selectable.char x))))
      
			))
		    ))
		)))
       ))))


;; this one is a fookin' nightmare
(defmethod select-item ((dungeon dungeon) (player player) allow-from
			&key prompt prompt-frame
			(no-item-msg "No items can be selected.")
			(where :backpack)
			selection-function
			printer-function)
  "Selects and returns a CONS (where . which) when succesful or
NIL.  Where is a keyword :floor, :backpack or :equip and which is either
a number or a symbol identifying the place."

  (let ((allow-floor (if (or (eq allow-from :floor)
			     (find :floor allow-from))
			 t
			 nil))
	
	(allow-equip (if (or (eq allow-from :equip)
			     (find :equip allow-from))
			 t
			 nil))
	
	(allow-backpack (if (or (eq allow-from :backpack)
				(find :backpack allow-from))
			    t
			    nil))
	(the-prompt (if prompt prompt "Inventory command:"))

	(the-place where)
	(printed-prompt nil)
	(place-len 0)
	(select-alts '())
	)

    (prog1
	(block %select-item

	  (flet ((do-query (show-mode prompt-win display-win)
		   (let* ((item-table (get-item-table dungeon player the-place))
			  (item-active (make-array (items.cur-size item-table) :initial-element t))
			  (legal-letters ""))
	       

		     (setf place-len (items.cur-size item-table))

	       
		     (when (functionp selection-function)
		       (dotimes (i place-len)
			 (setf (aref item-active i) (funcall selection-function item-table i
							     (item-table-get item-table i)))))
	       
		     (setq legal-letters (%get-legal-letters item-active))

		     (unless (cave-objects dungeon (location-x player) (location-y player))
		       (setf allow-floor nil))

		     (let ((show-floor-option (and allow-floor (not (eq the-place :floor)))))

		       ;; also check if equip/inven allowed
		       (cond ((and (= 0 (length legal-letters)) (not show-floor-option))
			      (setf printed-prompt no-item-msg))
			     (t
			      ;;(warn "select item ~s ~s" prompt-win display-win)
			      ;; inefficient crap
			      (setf printed-prompt (format nil "(Inven: ~a~a~a, ESC) ~a"
							   legal-letters
							   (if (not show-mode)
							       ", * or right-click to see"
							       "")
							   (if show-floor-option
							       ", - for floor"
							       "")
							   the-prompt))
			      )))

		     (let ((*cur-win* prompt-win))
		       (put-coloured-line! +term-white+ printed-prompt 0 0))

		     ;; sick
		     (when (equal printed-prompt no-item-msg)
		       (return-from %select-item nil))

		     (setf select-alts '())
	     
		     (when show-mode
		       (let ((*cur-win* display-win))

			 ;; do this inline so we can get proper ui-objects?
		 
			 (item-table-print item-table
					   :show-pause nil
					   :print-selection selection-function
					   :printer-function printer-function)
		 

			 (setf select-alts (loop for count from 0
						 for y across item-active
						 with line = 0
						 when y collect
						 (make-selectable-ui-object (i2a count)
									    0 (incf line)
									    200 line)))
			 ))


	     
		     ;; add setting of cursor.

		     ;; ensure that the framebuffer is flipped (really needed?)
		     (org.langband.ffi:c-flip-framebuffer)

		     ;; we need to read an event at this point.
	       
		     ;; how to do graceful exit of loop?
	     
		     (let ((read-char (select-displayed-alternative select-alts))
			   (*cur-win* prompt-win))

	       
		       ;;(warn "read-char ~s" read-char)
		       (cond ((eql read-char +escape+)
			      ;;(warn "clear stuff..")
			      ;; clear prompt and get out
			      (clear-row prompt-win 0 0)
			      (return-from %select-item nil))
		     
			     ((eql read-char #\*)
			      (return-from do-query :show))
		     
			     ((eql read-char #\/)
			      (when (or (and allow-equip    (eq the-place :backpack))
					(and allow-backpack (eq the-place :equip))
					allow-floor)
				(when show-mode
				  (loop for i from 1 below (1+ place-len)
					do (clear-row display-win 0 i)))
				(setq the-place (if (eq the-place :backpack) :equip :backpack))))

		     
			     ((and allow-floor (eq read-char #\-))
			      (when show-mode
				(loop for i from 1 below (1+ place-len)
				      do (clear-row display-win 0 i)))
			      (setq the-place :floor))
		     
		     
			     ;; improve this code, it just bails out now.
			     ((alpha-char-p read-char)
			      (put-coloured-line! +term-white+ "" 0 0)
			      (let ((num -1))
				(cond ((lower-case-p read-char) (setf num (a2i read-char)))
				      ((upper-case-p read-char) 
				       (setf num (a2i (char-downcase read-char))))
				      (t (warn "Illegal character read in select-item: ~s" read-char)))

				(cond ((and (>= num 0) (< num place-len))
				       (return-from %select-item (cons the-place num)))
				      (t
				       ;; maybe go through loop again instead?
				       ;; beep?
				       ))
				))
		     
			     (t
			      ;; maybe go through loop again instead?
			      ;;(error "Fell through on read-char in select-item ~s" read-char)
			      ))
	       
		       (put-coloured-line! +term-white+ "" 0 0)))))

	   
      
	    ;; hackish, first try a query in the message-frame, if show-mode is asked for
	    ;; jump to the loop embedded in a with-dialogue so that we stay there
	    (block read-loop
	      (let ((prompt-win (cond ((non-negative-integer? prompt-frame)
				       (aref *windows* prompt-frame))
				      ((typep prompt-frame 'window)
				       prompt-frame)
				      (t
				       (aref *windows* +query-frame+))))
		    (display-win (aref *windows* +dialogue-frame+))
		    (show-mode nil))
		(loop until show-mode
		      do (when (eq :show (do-query nil prompt-win display-win))
			   (setf show-mode t)))

		(with-dialogue ()
		  (loop (do-query t display-win display-win)))
	  
		)))) ;; end of %select-item

      
      ;; this gets wrong, because *cur-win* is the map, and that's the wrong
      ;; place to clear stuff!
      ;;(warn "*cur-win* is ~s" *cur-win*)

      ;; clear prompt
      ;;(put-coloured-line! +term-white+ "" 0 0)
    
      nil)))

(defmethod select-item :after (dungeon player allow-from
				       &key prompt prompt-frame
				       no-item-msg
				       where
				       selection-function
				       printer-function)
  
  (declare (ignore dungeon player allow-from prompt no-item-msg where selection-function printer-function))
  
  ;; just clears the prompt
  (let ((prompt-win (cond ((non-negative-integer? prompt-frame)
			   (aref *windows* prompt-frame))
			  ((typep prompt-frame 'window)
			   prompt-frame)
			  (t
			   (aref *windows* +query-frame+)))))

    (let ((*cur-win* prompt-win))
      (put-coloured-line! +term-white+ "" 0 0))))


(defun select-and-return-item (dungeon player allow-from
			       &key prompt (where :backpack)
			       selection-function)
  "Wrapper for select-item which just gets and returns the 'removed' object, or NIL."
  (when-bind (selection (select-item dungeon player allow-from
				     :prompt prompt
				     :where where
				     :selection-function selection-function))

    (item-table-remove! (get-item-table dungeon player (car selection))
			(cdr selection) :only-single-items t)))

(defun get-ensured-floor-table (dungeon the-x the-y)
  "Tries to get the items-on-floor for given coordinate, if it doesn't
exist it will be created, assigned to the coordinate and returned."
  (let ((cur-objs (cave-objects dungeon the-x the-y)))
    (unless cur-objs
      (setf cur-objs (make-floor-container dungeon the-x the-y))
      (setf (cave-objects dungeon the-x the-y) cur-objs))
    cur-objs))

(defmethod get-item-table ((dungeon dungeon) (player player) which-table &key x y)
  "Returns item-table or NIL."
  
  (ecase which-table
    (:floor
     (let* ((the-x (if x x (location-x player)))
	    (the-y (if y y (location-y player))))
       (get-ensured-floor-table dungeon the-x the-y)))
    (:backpack (aobj.contains (get-creature-inventory player)))
    (:inventory (aobj.contains (get-creature-inventory player)))
    (:equip (player.equipment player))
    (:equipment (player.equipment player))
    ))

;;; === Equipment-implementation for floors ===

(defmethod item-table-get ((table items-on-floor) idx)
  (cond ((and (integerp idx) (<= 0 idx) (< idx (items.cur-size table)))
	 (nth idx (items.objs table)))
	(t nil)))


(defmethod item-table-add! ((table items-on-floor) obj &optional key)
  (declare (ignore key))
;;  (lang-warn "Pushing ~a [~a,~a] onto floor [~a,~a]"
;;	    obj (location-x obj) (location-y obj)
;;	    (location-x table) (location-y table))
  (let ((tab-x (location-x table))
	(tab-y (location-y table))
	(dungeon (items.dungeon table)))
	
    (setf (location-x obj) tab-x
	  (location-y obj) tab-y)
    (push obj (dungeon.objects dungeon))
    (push obj (items.objs table))
    (incf (items.cur-size table))

;;    (warn "Added ~s to floor at ~s,~s" obj tab-x tab-y)
    ;; let's notify about the change
    (note-spot! dungeon tab-x tab-y)
    (light-spot! dungeon tab-x tab-y)
    
    t))

(defmethod item-table-remove! ((table items-on-floor) key &key only-single-items)
  ;;(warn "Remove ~s ~s" key only-single-items)
  
  (cond ((item-table-verify-key table key)
	 (let ((ret-obj nil)
	       (num-key (typecase key
			  (character (a2i key))
			  (number key)
			  (active-object (position key (items.objs table)))
			  (t nil))))
	   (when (numberp num-key)
	     (let ((old-obj (elt (items.objs table) num-key)))

	       ;; if only one, else remove
	       (cond ((and only-single-items (> (aobj.number old-obj) 1))
		      (setf ret-obj (create-aobj-from-kind (aobj.kind old-obj)))
		      (decf (aobj.number old-obj)))
		     (t
		      (setf (items.objs table) (delete old-obj (items.objs table)))
		      (remove-item-from-dungeon! (items.dungeon table) old-obj)
		      (decf (items.cur-size table))
		      (setf ret-obj old-obj)))))
	   
	   ret-obj))
	(t
	 (warn "illegal key ~a" key)
	 nil)))

(defmethod item-table-clean! ((table items-on-floor))

  (when (next-method-p)
    (call-next-method table))
  (let ((dungeon (items.dungeon table)))
    (dolist (i (items.objs table))
      (remove-item-from-dungeon! dungeon i)))

  (setf (items.objs table) nil))

(defmethod item-table-find ((table items-on-floor) key)
  (when (item-table-verify-key table key)
    (typecase key
      (character (elt (items.objs table) (a2i key)))
      (number (elt (items.objs table) key))
      (t
       (warn "unknown type ~a of key" (type-of key))
       nil))))


(defmethod item-table-sort! ((table items-on-floor) sorter)
  (declare (ignore sorter))
  ;; the floor is never sorted
  nil)

(defmethod item-table-iterate! ((table items-on-floor) function)
  (loop for i from 0
	for obj in (items.objs table)
	do
	(funcall function table i obj)))

(defmethod item-table-more-room? ((table items-on-floor) &optional obj)
  (declare (ignore obj))
  t)

(defmethod item-table-print ((table items-on-floor)
			     &key show-pause start-x start-y
			     print-selection printer-function)
  
  (let ((x (if start-x start-x 5));; 25))
	(y (if start-y start-y 1))
	(i 0))

    (flet ((iterator-fun (a-table key val)
	     (when (and (functionp print-selection)
			(eq nil (funcall print-selection a-table key val))) ;; should it be printed?
	       ;;(warn "obj ~s is not to be printed, cur-key ~s" val key)
	       (return-from iterator-fun nil))

	     (assert (integerp key))
	     (let ((attr (get-text-colour val))
		   (desc (with-output-to-string (s)
			   (write-obj-description *variant* val s))))
	       (put-coloured-line! +term-white+ "" (- x 2) (+ i y))
	       (put-coloured-str! +term-white+ (format nil "~a) " (i2a key)) x (+ i y))
	       (put-coloured-str! attr desc (+ x 4) (+ i y))
	       (incf i))))
      
    (item-table-iterate! table #'iterator-fun)
    
    (when show-pause
      (pause-last-line!))

    )))




(defmethod calculate-score (variant player)
  (declare (ignore variant))
  (+ (player.maximum-xp player) (* 100 (player.depth player))))


(defmethod can-creature-drop? ((variant variant) (mon active-monster))
  (let ((kind (amon.kind mon)))
    (if (monster.treasures kind)
	t
	nil)))

;; this one conses badly
(defmethod creature-drop! ((variant variant) (mon active-monster) (level level))
  (let ((kind (amon.kind mon))
	 (to-drop '()))
     ;; first iterate over the drops and decide what is actually dropped
     (dolist (i (monster.treasures kind))
       (check-type i treasure-drop)
       (when (< (random 100) (* 100 (drop.chance i)))
	 (let ((amount (drop.amount i)))
	   (when (consp amount)
	     (setf amount (roll-dice (car amount) (cdr amount))))
	   (when (plusp amount)
	     (push (list amount (drop.quality i) (drop.type i)) to-drop)))))

     ;; now process the actual drops
     (flet ((drop-an-obj (quality type)
	      (declare (ignore quality))
	      (case type
		((:any :item)
		 (let ((new-obj (get-active-object-by-level variant level)))
		   (drop-near-location! variant (level.dungeon level)
					new-obj (location-x mon) (location-y mon)))
		 )
		(:gold
		 (let ((new-gold (create-gold variant (level.dungeon level) :originator mon)))
		   (drop-near-location! variant (level.dungeon level)
					new-gold (location-x mon) (location-y mon)))
		 ))

	      ))
;;	      (warn "Dropping ~s ~s at (~s,~s)" quality type (location-x mon) (location-y mon)
     
       ;; now we have a list of objects (hopefully) to drop
       
       (dolist (i to-drop)
	 (assert (consp i))
	 (dotimes (j (car i))
	   (drop-an-obj (second i) (third i))))

       )
  ;; let us drop anything the monster carried

     (when-bind (inv (get-creature-inventory mon))
       (loop for x across (items.objs (aobj.contains inv))
	     when x
	     do
	     (drop-near-location! variant (level.dungeon level) x (location-x mon) (location-y mon))))
     ))

(defmethod drop-near-location! ((variant variant) (dungeon dungeon) (object active-object) x y)
  ;; we do this hackish
  (flet ((poss-drop (tx ty)
	   (when (bit-flag-set? (floor.flags (cave-floor dungeon tx ty))
				+floor-flag-allow-items+) ;; must allow items
	     (let ((tbl (get-ensured-floor-table dungeon tx ty)))
	       ;;(warn "Dropped ~s at (~s,~s)" object tx ty)
	       (item-table-add! tbl object)
	       (return-from drop-near-location! t)))))
    (poss-drop x y)
    (poss-drop (1+ x) y)
    (poss-drop x (1+ y))
    (poss-drop (1+ x) (1+ y))
    nil))


(defmethod update-monster! ((variant variant) (mon active-monster) full-update?)
  (let* ((player *player*)
	 (px (location-x player))
	 (py (location-y player))
	 (mx (location-x mon))
	 (my (location-y mon))
	 (dungeon *dungeon*)
	 (kind (amon.kind mon))
	 (seen nil) ;; seen at all
	 (by-eyes nil) ;; direct vision
	 (distance 666))
    
    (cond (full-update?  ;; get full distance
	   ;; no inlining
	   (setf distance (distance px py mx my)
		 (amon.distance mon) distance)
	   )
	  (t
	   ;; simple
	   (setf distance (amon.distance mon))))

    ;; more stuff
    (when (bit-flag-set? (amon.vis-flag mon) +monster-flag-mark+)
      (setf seen t))

    (when (<= distance +max-sight+)
      ;; skip telepathy

      ;; normal line of sight:
      (when (and (player-has-los-bold? dungeon mx my)
		 t) ;; add blindness check

	;; infravision
	(when (<= distance (player.infravision player))
	  (unless (has-ability? kind '<cold-blood>) ;; unless cold-blooded, infravision works
	    (setf seen t
		  by-eyes t)))

	(when (player-can-see-bold? dungeon mx my)
	  (cond ((has-ability? kind '<invisible>)
		 ;; can the player see it with his see-inv?
		 (when (<= distance (player.see-invisible player))
		   (setf seen t
			 by-eyes t))
		 )
		(t
		 (setf seen t
		       by-eyes t)))
	  
	  ;; skip lore
	  )))

    ;; is it visible?
    (cond (seen 
	   (unless (amon.seen-by-player? mon)
	     (setf (amon.seen-by-player? mon) t)
	     (light-spot! dungeon mx my)
	     ;; skip health bar
	     (disturbance variant player mon :major)
	     ))
	  ;; no longer visible
	  (t
	   (when (amon.seen-by-player? mon)
	     (setf (amon.seen-by-player? mon) nil)
	     (light-spot! dungeon mx my)
	     ;; skip health bar
	     (disturbance variant player mon :major)
	     )))

    (cond (by-eyes
	   (unless (bit-flag-set? (amon.vis-flag mon) +monster-flag-view+)
	     (bit-flag-add! (amon.vis-flag mon) +monster-flag-view+)
	     ;; skip disturb
	     ))
	  (t
	   (when (bit-flag-set? (amon.vis-flag mon) +monster-flag-view+)
	     (bit-flag-remove! (amon.vis-flag mon) +monster-flag-view+)
	     ;; skip disturb
	     )))
	  
    ))

(defmethod update-monsters! ((variant variant) (dungeon dungeon) full-update?)
  (let ((monsters (dungeon.monsters dungeon)))
    (dolist (i monsters)
      (when (creature-alive? i)
	(update-monster! variant i full-update?)))))

(defmethod deliver-damage! ((variant variant) source (target active-monster) damage &key note dying-note)
  "Delivers damage to someone."
  (declare (ignore note))

  ;;(warn "deliver-damage ~s to ~s from ~s" damage (get-creature-name target) (get-creature-name source))
  
  (let ((did-target-die? nil))
  
    ;; wake it up
    ;; deliver damage
    (setf (current-hp target) (- (current-hp target) damage))

    
    (when (minusp (current-hp target))
      (setf did-target-die? t)
      ;; make a message about it
      (cond (dying-note
	     (format-message! "~@(~A~) ~a." (get-creature-desc target #x00) dying-note))
	    ((is-player? source)
	     (format-message! "You have slain ~a." (get-creature-desc target #x00)))
	    ((is-monster? source)
	     (format-message! "~@(~A~) has slain ~a." (get-creature-desc source #x04)
			      (get-creature-desc target #x00)))
	    (t
	     (warn "HEEEELP")))

      (let ((attacker *player*)
	    (dungeon *dungeon*)
	    (target-xp (get-xp-value target)))
	(modify-xp! attacker (if target-xp target-xp 0))
	(kill-target! variant dungeon attacker target (location-x target) (location-y target))
	))

    ;; skip fear
    did-target-die?))
  

(defmethod deliver-damage! ((variant variant) (source active-trap) (target player) damage &key note dying-note)
  
  (declare (ignore dying-note note))
  
  (let ((did-target-die? nil))
  
    ;; wake it up
    ;; deliver damage
    (setf (current-hp target) (- (current-hp target) damage))

    (ask-for-redraw! target '[hp])
    
    (when (minusp (current-hp target))
      (setf did-target-die? t)

      (format-message! "You were killed by a ~a" (trap.name (decor.type source)))
      (kill-target! variant *dungeon* source target (location-x target) (location-y target))
      )

    did-target-die?))


(defconstant +random-normal-number+ 256 "Number of entries in table.")
(defconstant +random-normal-deviation+ 64 "The standard deviation of the table.")

(defparameter *random-normal-table* #256(
          206     613    1022    1430    1838    2245    2652    3058
         3463    3867    4271    4673    5075    5475    5874    6271
         6667    7061    7454    7845    8234    8621    9006    9389
         9770   10148   10524   10898   11269   11638   12004   12367
        12727   13085   13440   13792   14140   14486   14828   15168
        15504   15836   16166   16492   16814   17133   17449   17761
        18069   18374   18675   18972   19266   19556   19842   20124
        20403   20678   20949   21216   21479   21738   21994   22245

        22493   22737   22977   23213   23446   23674   23899   24120
        24336   24550   24759   24965   25166   25365   25559   25750
        25937   26120   26300   26476   26649   26818   26983   27146
        27304   27460   27612   27760   27906   28048   28187   28323
        28455   28585   28711   28835   28955   29073   29188   29299
        29409   29515   29619   29720   29818   29914   30007   30098
        30186   30272   30356   30437   30516   30593   30668   30740
        30810   30879   30945   31010   31072   31133   31192   31249

        31304   31358   31410   31460   31509   31556   31601   31646
        31688   31730   31770   31808   31846   31882   31917   31950
        31983   32014   32044   32074   32102   32129   32155   32180
        32205   32228   32251   32273   32294   32314   32333   32352
        32370   32387   32404   32420   32435   32450   32464   32477
        32490   32503   32515   32526   32537   32548   32558   32568
        32577   32586   32595   32603   32611   32618   32625   32632
        32639   32645   32651   32657   32662   32667   32672   32677

        32682   32686   32690   32694   32698   32702   32705   32708
        32711   32714   32717   32720   32722   32725   32727   32729
        32731   32733   32735   32737   32739   32740   32742   32743
        32745   32746   32747   32748   32749   32750   32751   32752
        32753   32754   32755   32756   32757   32757   32758   32758
        32759   32760   32760   32761   32761   32761   32762   32762
        32763   32763   32763   32764   32764   32764   32764   32765
        32765   32765   32765   32766   32766   32766   32766   32767))


(defun normalised-random (mean stand)
  "Generate a random integer number of NORMAL distribution
 
  The table above is used to generate a psuedo-normal distribution,
  in a manner which is much faster than calling a transcendental
  function to calculate a true normal distribution."

  ;; paranoia
  (when (< stand 1)
    (return-from normalised-random mean))
  
  (let ((low 0)
	(high +random-normal-number+)
	(tmp (random 32768)))
    (loop while (< low high)
	  do
	  (let ((mid (int-/ (+ low high) 2)))
	    (cond ((< (aref *random-normal-table* mid) tmp)
		   (setf low (1+ low)))
		  (t
		   (setf high mid)))))
    (let ((offset (int-/ (* stand low) 
			 +random-normal-deviation+)))
      (if (< (random 100) 50)
	  (- mean offset)
	  (+ mean offset)))))
  

(defun get-level-appropriate-enchantment (variant level max)
  "Returns an appropriate bonus for items generated on the level."
  
  (let ((max-depth (variant.max-depth variant)))
    ;; hack
    (when (typep level 'level)
      (setf level (level.depth level)))
    (assert (numberp max))
    (assert (numberp level))
    (when (> level (1- max-depth))
      (setf level (1- max-depth)))

    (let ((bonus (int-/ (* max level)
			max-depth))
	  (extra (mod (* max level)
			max-depth))
	  (stand (int-/ max 4)))
      
      ;; hack
      (when (< (random max-depth) extra)
	(incf bonus))

      ;; hack
      (when (< (random 4) (mod max 4))
	(incf stand))

      (let ((value (normalised-random bonus stand)))
	(cond ((minusp value) 0)
	      ((> value max) max)
	      (t value)))
      )))

(defvar *currently-showing-inv* :inventory)
;;(defvar *currently-showing-inv* :equipment)
  
(defmethod update-inventory-row (variant player)
  
  (unless (eq (get-system-type) 'sdl)
    (return-from update-inventory-row t))

  ;; hack, remove

  (update-button-row variant player)
  
  (unless (is-frame-shown? variant +inv-frame+)
    (return-from update-inventory-row t))

  
  (let (;;(off-button (tile-paint-value +tilefile-buttons+ 0))
	;;(on-button (tile-paint-value +tilefile-buttons+ 1))
	(win (get-window +inv-frame+)))
    
    (with-frame (+inv-frame+)
      (clear-window +inv-frame+) ;; hack
      (let ((table nil)
	    (pl-inv (get-creature-inventory player)))

	(when pl-inv

	  (cond ((eq *currently-showing-inv* :inventory)
		 (setf table (aobj.contains pl-inv)))
		((eq *currently-showing-inv* :equipment)
		 (setf table (player.equipment player)))
		(t
		 (warn "Unable to find good equipment for ~s" *currently-showing-inv*)
		 (return-from update-inventory-row nil)))
	  
	  (loop for i from 0 below (items.cur-size table)
		for obj = (aref (items.objs table) i)
		unless (and (typep table 'items-worn)
			    (worn-item-slot-hidden (aref (variant.worn-item-slots variant) i)))
		do
		(progn
		  ;;(setf (window-coord win +foreground+ i 0) (tile-paint-value 43 i))
		  (when obj
		    (setf (window-coord win +effect+ i 0) (text-paint-value +term-white+ (+ #.(char-code #\a) i)))
		    (setf (window-coord win +foreground+ i 0) (gfx-sym obj))
		    )))))

      #||
      (let ((col (- (get-frame-width +inv-frame+) 1))
	    (equip-button (if (eq *currently-showing-inv* :inventory)
			      off-button
			      on-button))
	    (back-button (if (eq *currently-showing-inv* :inventory)
			     on-button
			     off-button)))

	(setf (window-coord win +foreground+ col 0) (tile-paint-value 10 0)
	      (window-coord win +background+ col 0) back-button
	      (window-coord win +foreground+ col 1) (tile-paint-value +tilefile-armour+ 34)
	      (window-coord win +background+ col 1) equip-button
	      )

	(let ((col (- (get-frame-width +inv-frame+) 2))
	      (map-button (if (eq *current-map-mode* :ascii)
			      off-button
			      on-button))
	      (ascii-button (if (eq *current-map-mode* :ascii)
				on-button
				off-button)))

	  (setf (window-coord win +foreground+ col 0) (tile-paint-value 10 19)
		(window-coord win +background+ col 0) map-button
		(window-coord win +foreground+ col 1) (text-paint-value +term-l-blue+ #.(char-code #\A))
		(window-coord win +background+ col 1) ascii-button)
      
	  )

	;;(%print-runes win)
      
	;; should do all the actual painting

      )
      ||#
      (refresh-window win)
      
      t)))


(defun colour-area (win colour-idx col row wid hgt)
  (let ((col-x (mod colour-idx 5))
	(col-y (floor colour-idx 5)))

    ;;(warn "Colour ~s win with ~s ~s" win col-x col-y)
    (loop for y from row below (+ hgt row) do
	  (loop for x from col below (+ wid col) do
		(let ((tile-idx (+ (* 2 col-x) (* 20 col-y))))
		  ;;(warn "col-x is ~s and col-y is ~s -> idx is ~s " col-x col-y tile-idx)
		  (incf tile-idx (rem x 2))
		  (incf tile-idx (* 10 (rem y 2)))
		  ;;(warn "(x,y ~s,~s) (col-x,col-y ~s,~s) -> idx is ~s " x y col-x col-y tile-idx)
		  (setf (window-coord win +background+ x y) (tile-paint-value +tilefile-backgrounds+ tile-idx))
		  )))
    win))  

(defun colour-window (win colour-idx)
  "Tries to colour the window with a background from the background tilefile."
  (colour-area win colour-idx 0 0 (window.width win) (window.height win)))


(defun update-button-row (variant player)
  "deprecated.. remove eventually."
  (declare (ignorable player))

  #||
  (when (is-frame-shown? variant +tiledfields-frame+)
    (let ((win (get-window +tiledfields-frame+)))
      (colour-window win 0)))
  ||#
  
  (loop for x being the hash-values of *printfield-info*
	do
	(when (is-frame-shown? variant (field-printer.window-key x))
	  ;;(format t "~&~s ~a ~a~%" (button-info-key x) (button-info-col x) (button-info-row x))
	  (when (and (functionp (field-printer.handler x))
		     (non-negative-integer? (field-printer.col x))
		     (non-negative-integer? (field-printer.row x)))
	      (funcall (field-printer.handler x) (get-window (field-printer.window-key x))
		       (field-printer.col x) (field-printer.row x)
		       variant player *dungeon*)
	      )))

  (refresh-window +tiledfields-frame+)
  (refresh-window +charinfo-frame+))

      

      #||
      (dotimes (i stat-len)
	(setf (idx-value (+ i  0)) (svref (player.active-stats player) i)
	      (idx-value (+ i 20)) (get-stat-name-from-num i)))

      ;; hackish
      (setf (idx-value 27) "Lvl"
	    (idx-value 28) "AC"
	    (idx-value 29) "HP"
	    (idx-value 30) "MP")

      (setf (idx-value 7) (player.power-lvl player)
	    (idx-value 8) (+ (get-armour-rating (player.perceived-abilities player))
			     (get-armour-modifier (player.perceived-abilities player)))
	    (idx-value 9) (current-hp player)
	    (idx-value 10) (maximum-hp player))

      ;;(setf (window-coord win +effect+ 0 row) (text-paint-value +term-white+ (+ #.(char-code #\a) 20)))
      (loop for row from 0 below 9 do
	    (progn
	      (setf (window-coord win +foreground+ 0 row) (idx-paint-value +term-l-blue+ (+ 20 row)))
	      (setf (window-coord win +foreground+ 1 row) (idx-paint-value +term-white+  (+  0 row))))
	    )

      (let ((row 0)
	    (start 10))
	(setf (window-coord win +background+ 0 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 0))
	      (window-coord win +background+ 1 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 1))
	      (window-coord win +background+ 0 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 0))
	      (window-coord win +background+ 1 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 1)))
	)

      (let ((row 2)
	    (start 10))
	(setf (window-coord win +background+ 0 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 0))
	      (window-coord win +background+ 1 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 1))
	      (window-coord win +background+ 0 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 0))
	      (window-coord win +background+ 1 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 1)))
	)
      
      (let ((row 4)
	    (start 10))
	(setf (window-coord win +background+ 0 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 0))
	      (window-coord win +background+ 1 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 1))
	      (window-coord win +background+ 0 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 0))
	      (window-coord win +background+ 1 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 1)))
	)

      (let ((row 9)
	    (start 12))
	(setf (window-coord win +foreground+ 0 (+ 0 row)) (idx-paint-value +term-l-blue+ (+ 20 row))
	      (window-coord win +foreground+ 1 (+ 0 row)) (idx-paint-value +term-white+  (+  0 row))
	      (window-coord win +foreground+ 1 (+ 1 row)) (idx-paint-value +term-white+  (+  1 row)))

	      
	(setf (window-coord win +background+ 0 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 0))
	      (window-coord win +background+ 1 (+ 0 row)) (tile-paint-value +tilefile-buttons+ (+ start 1))
	      (window-coord win +background+ 0 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 0))
	      (window-coord win +background+ 1 (+ 1 row)) (tile-paint-value +tilefile-buttons+ (+ start 10 1)))
	)
      ||#
      
      ;;(setf (window-coord win +foreground+ 0 row) 0) ;;(tile-paint-value 10 10))


(defmethod switch-inventory-view (variant player &key wanted-view)
  (declare (ignore wanted-view))
  (if (eq *currently-showing-inv* :inventory)
      (setf *currently-showing-inv* :equipment)
      (setf *currently-showing-inv* :inventory))
  (update-inventory-row variant player)
  *currently-showing-inv*)


(defmethod switch-map-mode (variant dungeon player &optional wanted-mode)
  "Tries to switch map-mode.  If wanted-mode is given, that mode is attempted,
otherwise it will toggle."

  (cond ((eq wanted-mode nil)
	 ;; toggle
	 (if (eq *current-map-mode* :ascii)
	     (setf *current-map-mode* :gfx-tiles)
	     (setf *current-map-mode* :ascii)))
	((and (eq wanted-mode :ascii)
	      (eq *current-map-mode* :gfx-tiles))
	 (setf *current-map-mode* :ascii))
	((and (eq wanted-mode :gfx-tiles)
	      (eq *current-map-mode* :ascii))
	 (setf *current-map-mode* :gfx-tiles))
	(t
	 ;; do nothing
	 (return-from switch-map-mode nil)))
  
  ;; we had a change!
  (update-inventory-row variant player)

  (cond ((eq *current-map-mode* :ascii)
	 (deactivate-window +gfxmap-frame+)
	 (activate-window +asciimap-frame+)
	 (setf *map-frame* +asciimap-frame+))
	
	((eq *current-map-mode* :gfx-tiles)
	 (deactivate-window +asciimap-frame+)
	 (activate-window +gfxmap-frame+)
	 (setf *map-frame* +gfxmap-frame+))
	
	(t ))

  (clear-window *map-frame*)
  (verify-viewport dungeon player)
  (print-map dungeon player *map-frame*)
  (refresh-window *map-frame*)

  *current-map-mode*)

(defmethod decor-operation ((variant variant) (door active-door) operation &key value)
  "This one uses *dungeon* so please set it to a meaningful value before calling
this function."
  (let ((x (location-x door))
	(y (location-y door))
	(dungeon *dungeon*))
  
    (ecase operation
      (:close
       (unless (eq value t)
	 (warn "Close with non-T argument ~s" value))
       (setf (door.closed? door) t)
       (bit-flag-add! (cave-flags dungeon x y) +cave-wall+)

       (setf (decor.type door) (get-door-type "closed-door")))

      (:open
       (unless (eq value t)
	 (warn "Open with non-T argument ~s" value))
       ;; fix locks
       #||
       (when (plusp (door.lock door))
	 (warn "Lock-level ~s" (door.lock door)))
       ||#
       (setf (door.closed? door) nil)
       (bit-flag-remove! (cave-flags dungeon x y) +cave-wall+)
       (setf (decor.type door) (get-door-type "open-door")))

    
      (:break
       (unless (eq value t)
	 (warn "Break with non-T argument ~s" value))
       (setf (is-broken? door) t)
       (setf (door.closed? door) nil)
       (bit-flag-remove! (cave-flags dungeon x y) +cave-wall+)

       (setf (decor.type door) (get-door-type "destroyed-door"))))

    ))

(defmethod decor-operation ((variant variant) (door active-door) (operation (eql :visible)) &key value)
  (unless (eq value t)
    (warn "Visible with non-T argument ~s" value))

  (setf (decor.visible? door) t)
  ;;(light-spot! dungeon x y)
  )

(defmethod decor-operation ((variant variant) (door active-trap) (operation (eql :visible)) &key value)
  (unless (eq value t)
    (warn "Visible with non-T argument ~s" value))
  (setf (decor.visible? door) t)
  ;;(light-spot! dungeon x y)
  )

(defmethod decor-operation ((variant variant) (trap active-trap) operation &key value)

  (warn "trap operation ~s (~s) not implemented." operation value)

  )


(defun %alt-sel-input (alt-len &key alternative-list)
  "INTERNAL FUNCTION.  Might change!

Reads a character via READ-ONE-CHARACTER and
acts on the result:
  Q     - calls QUIT-GAME&
  S     - returns NIL
  ESC   - Picks random value and returns it (a number)
  *     - As ESC
  ENTER - Returns 'CURRENT (ie the currently selected value)
  SPACE - As ENTER
  [a-z] - Checks if the value is legal, returns number if ok, returns 'BAD-VALUE if not legal
"
  (let ((val (if alternative-list
		 (select-displayed-alternative alternative-list)
		 (read-one-character))))
    #-cmu
    (assert (characterp val))
;;    (warn "Got back ~a ~s ~s" val val (type-of val))
    (cond ((eql val #\Q)
	   (quit-game&))
	  ;; start over
	  ((eql val #\S)
	   nil)
	  ;; pick a random value
	  ((or (eql val +escape+)
	       (eql val #\*))
	   (random alt-len))
	  ;; use highlighted value
	  ((or (eql val #\Return) (eql val #\Newline))
	   'current)
	  (t
	   (let ((its-char-code (char-code val)))
	     ;; legal char-code
	     (cond ((and (>= its-char-code (char-code #\a))
			 (<= its-char-code (char-code #\z)))
		    (let ((r-val (- its-char-code (char-code #\a))))
		      (if (and (>= r-val 0) (< r-val alt-len))
			  r-val
			  (progn
			    (warn (format nil "Invalid value: ~a" val))
			    'bad-value))))

		   ;; an arrow-key I guess
		   ((and (>= its-char-code (char-code #\0))
			 (<= its-char-code (char-code #\9)))
		    (case val
		      (#\8 'up)
		      (#\6 'right)
		      (#\4 'left)
		      (#\2 'down)
		      (t 'bad-value)))

		   (t
		    'bad-value))
	     )))
    ))


(defun interactive-alt-sel (col row alternatives
			    &key (display-fun nil) (mod-value 5) 
			    (ask-for "value") (settings nil))
"Interative selection of alternatives.

The COL, ROW argument specifies where the alternatives should start
ALTERNATIVES is a list of valid alternatives
DISPLAY-FUN is a function to display help for a given option
MOD-VALUE is how much space should be between rows (I think)"
  ;; [add more info]

  
  (let ((alt-len (length alternatives))
	(win *cur-win*))
    
    (labels ((display-alternatives (highlight-num)
	       "also returns coordinates fit for select-displayed-alternative"
	       (let* ((desc (when display-fun
			     (funcall display-fun highlight-num)))
		      (offset-x (setting-lookup settings "offset-x" 0))
		      (offset-y (setting-lookup settings "offset-y" 0))
		      (text-col (+ offset-x (setting-lookup settings "text-x" 2)))
		      (text-row (+ offset-y (setting-lookup settings "text-y" 10)))
		      (text-attr (setting-lookup settings "text-attr" +term-white+))
		      (text-wid (setting-lookup settings "text-w" 75))
		      (alt-colour (setting-lookup settings "altern-attr" +term-white+))
		      (salt-colour (setting-lookup settings "altern-sattr" +term-l-blue+))
		      (alt-b-colour (setting-lookup settings "button-attr" :purple))
		      (salt-b-colour (setting-lookup settings "button-sattr" :green))
		      (row-to-clear (if desc text-row row))
		      ;;(alt-coords '())
		      )		 
		 
		 ;; find a better solution here
		 (loop for i from row-to-clear below (get-frame-height win)
		       do
		       ;;(warn "clear ~s ~s" text-col i)
		       (clear-row win text-col i +max-wincol+ +decor+)
		       (clear-row win text-col i +max-wincol+ +foreground+))
;;		       (put-coloured-line! +term-white+ "" text-col i))
		 ;;(clear-window-from *cur-win* text-row) ;; clears things

		 (when desc
		   (print-text! text-col text-row text-attr desc :end-col (+ text-col text-wid)))
		 
		 (loop for cur-alt in alternatives
		       for i from 0
		       for the-col = (truncate (+ col (* 15 (mod i mod-value))))
		       for the-row = (truncate (+ 1 row (/ i mod-value)))
		       for cur-bg  = (if (= i highlight-num) salt-colour alt-colour)
		       for cur-butt = (if (= i highlight-num) salt-b-colour alt-b-colour)
		       for text = (format nil "~c) ~10a" (i2a i) cur-alt)
		       collecting
		       (make-selectable-ui-object (i2a i) the-col the-row (+ 13 the-col) the-row
							  :text text
							  :text-colour alt-colour
							  :text-colour-hi salt-colour
							  :highlighted? (= i highlight-num)
							  :button-colour alt-b-colour
							  :button-colour-hi salt-b-colour))
		 ))
	     
	     (get-a-value (cur-sel)
	       (let* ((query-colour (setting-lookup settings "query-attr" +term-l-red+))
		      (red-query (setting-lookup settings "query-reduced" nil))
		      (query-str (if red-query
				     "Choose a ~a (~c-~c): "
				     "Choose a ~a (~c-~c, or * for random): "))
		      (coords (display-alternatives cur-sel))
		      (rval nil))

		 (put-coloured-str! query-colour (format nil query-str
							 ask-for (i2a 0) (i2a (- alt-len 1)))
				    col row)

		 (dolist (i coords)
		   (buttonify-selectable-ui-object *cur-win* i))
		 
		 (setf rval (%alt-sel-input alt-len :alternative-list coords))
		 
		 (if (not (symbolp rval))
		     rval
		     (case rval
		       (bad-value (get-a-value cur-sel)) ;; retry
		       (current cur-sel)	     ;; return current
		       (up    (get-a-value (mod (- cur-sel mod-value) alt-len))) ;; move selection
		       (down  (get-a-value (mod (+ cur-sel mod-value) alt-len))) ;; move selection
		       (left  (get-a-value (mod (- cur-sel 1) alt-len))) ;; move selection
		       (right (get-a-value (mod (+ cur-sel 1) alt-len))) ;; move selection
		       (t
			(warn "Unknown symbol returned ~s" rval)))))

	       ))
      
      (get-a-value 0))))

(defun run-in-direction (dungeon player direction)
  "Tries to run in the given direction."
  (let ((next (run-along-corridor dungeon player direction)))
;;    (warn "Tried to run ~s, got next ~s" direction next)
    (cond ((plusp next)
	   ;;(sleep 0.1)
	   (move-player! dungeon player next)
	   (setf (get-information "run-direction") next)
	   t)
	  (t
	   (stop-creature-activity *variant* player :running)
	   ))))

(defmethod stop-creature-activity ((variant variant) (player player) activity)
  "Tries to stop given activity for a creature."
  (case activity
    (:running
     (halt-sound-effects 0)
     (setf (get-information "run-direction") -1
	   (get-information "running") nil))
    (:resting
     (setf (get-information "rest-mode") nil
	   (get-information "resting") nil))
    (otherwise
     (warn "Unknown player-activity ~s" activity)))
  t)

(defmethod disturbance ((variant variant) (player player) source level)
  (declare (ignore source))
  (when (or (eq level :max) (eq level :major))
    (stop-creature-activity variant player :running))
  (when (or (eq level :max) (eq level :major))
    (stop-creature-activity variant player :resting))

  t)

(defun is-resting? (creature)
  "Returns T if the creature is resting."
  (declare (ignore creature))
  (get-information "resting" :default nil))


(defun get-string-input (prompt &key (max-length 20) (x-pos 0) (y-pos 0))
  "Non-efficient code to read input from the user, and return the string
on success.  Returns NIL on failure or user-termination (esc)." 
  (put-coloured-line! +term-white+ prompt x-pos y-pos)

  (let ((xpos (+ x-pos (length prompt)))
	(ypos y-pos)
	(wipe-str (make-string max-length :initial-element #\Space))
	(cnt 0)
	(collected '())
	(return-value nil))

    ;; wipe before we start to enter stuff
    (put-coloured-str! +term-dark+ wipe-str xpos ypos)
    (set-cursor-to *cur-win* :input (+ cnt xpos) ypos)
    
    (block str-input
      (loop
       (let ((val (read-one-character)))
	 ;;(warn "got ~s" val)
	 (cond ((or (eql val +escape+) #|(eql val #\Escape)|#)
		(return-from str-input nil))
	       ((eql val #\Backspace)
		(when collected
		  (setq collected (cdr collected))
		  (decf cnt)))
	       ((or (eql val #\Return) (eql val #\Newline))
		
		(setq return-value (coerce (nreverse collected) 'string))
		     (return-from str-input nil))
	       ((or (alphanumericp val)
		    (eql val #\-)
		    (eql val #\])
		    (eql val #\[)
		    (eql val #\()
		    (eql val #\))
		    (eql val #\<)
		    (eql val #\>)
		    (eql val #\*)
		    (eql val #\&)
		    (eql val #\.))
		(push val collected)
		(incf cnt))
	       (t
		(warn "Got unknown char ~s" val)))
	    
	 ;;	    (warn "print ~s" (coerce (reverse collected) 'string))
	 (put-coloured-str! +term-dark+ wipe-str xpos ypos)
	 (put-coloured-str! +term-l-blue+ (coerce (reverse collected) 'string) xpos ypos)
	 (set-cursor-to *cur-win* :input (+ cnt xpos) ypos))))
    
    (put-coloured-line! +term-white+ "" x-pos y-pos)
    
    return-value))

(defun get-aim-direction ()
  "Interactive! Uses +query-frame+"
  
  (flet ((read-loop ()
	   (let ((xpos 0)
		 (ypos 0)
		 (err-colour +term-yellow+))
	     (loop
	      (put-coloured-line! +term-white+ "Direction: " xpos ypos)
	      (let ((val (read-one-character)))
		(cond ((or (eql val #\.)
			   (eql val #\0)
			   (eql val #\t))
		       ;; only allowed when there is a legal target, 
		       (cond ((is-legal-target? *dungeon* (player.target *player*))
			      (put-coloured-line! +term-white+ "" 0 0)
			      (return-from read-loop 5))
			     (t
			      (let ((msg "No legal target selected!"))
				(setf xpos (1+ (length msg)))
				(put-coloured-line! err-colour msg 0 0))
			      )))
		    
		      ((digit-char-p val)
		       (put-coloured-line! +term-white+ "" 0 0)
		       (return-from read-loop (digit-char-p val)))
		    
		      ((eql val +escape+)
		       (put-coloured-line! +term-white+ "" 0 0)
		       (return-from read-loop nil))
		    
		      (t
		       (let ((msg "Unknown direction!"))
			 (setf xpos (1+ (length msg)))
			 (put-coloured-line! err-colour msg 0 0))
		       ))
		))
	     )))

    
    (with-frame (+query-frame+)
      ;; still needed?
      ;;(flush-messages! :forced t)
      (let ((retval (read-loop)))
	(put-coloured-line! +term-white+ "" 0 0)
	retval))))


#||
 * Desc-type Flags:
 *   0x01 --> Objective (or Reflexive)
 *   0x02 --> Possessive (or Reflexive)
 *   0x04 --> Use indefinites for hidden monsters ("something")
 *   0x08 --> Use indefinites for visible monsters ("a kobold")
 *   0x10 --> Pronominalize hidden monsters
 *   0x20 --> Pronominalize visible monsters
 *   0x40 --> Assume the monster is hidden
 *   0x80 --> Assume the monster is visible
 *
 * Useful desc-types:
 *   0x00 --> Full nominative name ("the kobold") or "it"
 *   0x04 --> Full nominative name ("the kobold") or "something"
 *   0x80 --> Banishment resistance name ("the kobold")
 *   0x88 --> Killing name ("a kobold")
 *   0x22 --> Possessive, genderized if visable ("his") or "its"
 *   0x23 --> Reflexive, genderized if visable ("himself") or "itself"
||#

(defmethod get-creature-desc (creature desc-type)
  "Returns a string with monster-desc."
  (let* ((name (get-creature-name creature))
	 (seen (or (bit-flag-and desc-type #x80)
		   (is-player? creature)
		   (and (not (bit-flag-and desc-type #x40))
			(amon.seen-by-player? creature))))
	 (pronoun (or (and seen
			   (bit-flag-and desc-type #x20))
		      (and (not seen)
			   (bit-flag-and desc-type #x10))))
	 (retval name))
   
    (cond ((or (not seen) pronoun)
	   ;; not seen monsters and pronouns

	   (let ((kind (cond ((is-player? creature)
			      #x30)
			     ((is-female? creature)
			      #x20)
			     ((is-male? creature)
			      #x10)
			     (t
			      #x00))))
	     
	     (unless pronoun
	       (setf kind #x00))

	     (setf retval
		   (case (+ kind (logand desc-type #x07))
		     ;; neuter/unknown
		     (#x00 "it")
		     (#x01 "it")
		     (#x02 "its")
		     (#x03 "itself")
		     (#x04 "something")
		     (#x05 "something")
		     (#x06 "something's")
		     (#x07 "itself")
		     ;; male
		     (#x10 "he")
		     (#x11 "him")
		     (#x12 "his")
		     (#x13 "himself")
		     (#x14 "someone")
		     (#x15 "someone")
		     (#x16 "someone's")
		     (#x17 "himself")
		     ;; female
		     (#x20 "she")
		     (#x21 "her")
		     (#x22 "her")
		     (#x23 "herself")
		     (#x24 "someone")
		     (#x25 "someone")
		     (#x26 "someone's")
		     (#x27 "herself")
		     ;; player
		     (#x30 "you")
		     (#x31 "you")
		     (#x32 "your")
		     (#x33 "yourself")
		     (#x34 "yourself")
		     (#x35 "yourself")
		     (#x36 "your")
		     (#x37 "yourself")
		     (otherwise "it")))
	     ))

	  ;; visible monster, reflexive request
	  ((and (bit-flag-and desc-type #x02)
		(bit-flag-and desc-type #x01))
	   (setf retval (cond ((is-player? creature)
			       "yourself")
			      ((is-male? creature)
			       "himself")
			      ((is-female? creature)
				"herself")
			      (t
			       "itself"))))

	  ;; all other visible monsters
	  (t
	   
	   (cond ((or (is-unique-monster? creature)
		      (is-player? creature))
		  (setf retval name))
		 ((bit-flag-and desc-type #x08)
		  (let ((article (if (is-vowel? (aref name 0))
				     "an " "a ")))
		    (setf retval (concatenate 'string article name))))
		 (t
		  ;; a definite monster
		  (setf retval (concatenate 'string "the " name))))


	   ;; possesive
	   (when (bit-flag-and desc-type #x02)
	     ;; check last char for safety
	     (setf retval (concatenate 'string retval "'s")))

	   
	   ;; check offscreen
	   
	   ))
    (assert (stringp retval))
    
    retval))


(defun load-setting-background (variant win settings-obj &key (layout :centred))
  "Loads a setting background into the window at +background+ layer. Returns adjustments to x, and y"

  (declare (ignore layout)) ;; centres it
  (when-bind (bg (setting-lookup settings-obj "background"))
    (let ((book-idx (find-image variant bg)))
      (unless (non-negative-integer? book-idx)
	(setf book-idx (load-image& variant bg 1 0)))
      (unless (non-negative-integer? book-idx)
	(warn "Unable to load image ~s and get an index other than ~s."
	      bg book-idx)
	(return-from load-setting-background nil))
		     
      (let* ((book-wid (setting-lookup settings-obj "background-width" 800))
	     (book-hgt (setting-lookup settings-obj "background-height" 800))
	     (book-x (truncate (int-/ (- (window.pixel-width win) book-wid) 2) (window.tile-width win)))
	     (book-y (truncate (int-/ (- (window.pixel-height win) book-hgt) 2) (window.tile-height win))))
	
	(paint-gfx-image win book-idx book-x book-y +background+)
     
	(values book-x book-y))
      )))

