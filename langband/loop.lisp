;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: loop.lisp - the game loop(s) (most relevant code)
Copyright (c) 2000-2004 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify ;
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or ;
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun %load-window-textures (variant)
  (let ((idx 50))
    (loop for win across *windows*
	  for i from 0
	  do
	  (when win
	    (when-bind (bgfile (window.backgroundfile win))
	      (cond ((stringp bgfile)
		     (incf idx)
		     (org.langband.ffi:c-load-texture& idx
						       (concatenate 'string *engine-data-dir*
								    "graphics/" bgfile)
						       (window.pixel-width win)
						       (window.pixel-height win) 0)
		     (setf (window.background win) idx)
		     ;; c-side needs negative value for bad values
		     (org.langband.ffi:c-add-frame-bg! i idx) 
		     (register-image& variant bgfile idx))
		    
		    ;; colour with background from background tilefile
		    ((non-negative-integer? bgfile)
		     (colour-window win bgfile))
		    (t
		     (warn "Don't know how to handle background ~s for window ~s"
			   bgfile i)))
	      ))
	  )))


(defmethod redraw-stuff ((variant variant) (dungeon dungeon) (player player))
  "Redraws stuff according to redraws requested previously."

  (when (not (any-redraws? player)) (return-from redraw-stuff nil))

  (let ((retval nil))

    (when (want-redraw? player '[map])
      (reset-redraw! player '[map])
      (print-map dungeon player *map-frame*)
      (setf retval t))

    
    (when (want-redraw? player '[basic])
      (reset-redraw! player '[basic])
      (reset-redraw! player '[misc])
      (reset-redraw! player '[stats])
      (reset-redraw! player '[level])
      (reset-redraw! player '[xp])
      (reset-redraw! player '[gold])
      (reset-redraw! player '[armour])
      (reset-redraw! player '[hp])
      (reset-redraw! player '[depth])
      (reset-redraw! player '[health])
      (print-basic-frame variant dungeon player)
      (setf retval t))
    

    
    (when (want-redraw? player '[misc])
      (reset-redraw! player '[misc])
      (trigger-printfield-hooks& variant dungeon player '[race])
      (trigger-printfield-hooks& variant dungeon player '[class])
      (setf retval t))
    

    (when (want-redraw? player '[level])
      (reset-redraw! player '[level])
      (trigger-printfield-hooks& variant dungeon player '[level])
      (setf retval t))

    (when (want-redraw? player '[xp])
      (reset-redraw! player '[xp])
      (trigger-printfield-hooks& variant dungeon player '[xp])
      (setf retval t))

    (when (want-redraw? player '[stats])
      (reset-redraw! player '[stats])
      (trigger-printfield-hooks& variant dungeon player '[stats])
      (setf retval t))
    
    (when (want-redraw? player '[armour])
      (reset-redraw! player '[armour])
      (trigger-printfield-hooks& variant dungeon player '[armour])
      (setf retval t))
    
    (when (want-redraw? player '[hp])
      (reset-redraw! player '[hp])
      ;;(warn "trigger hp")
      (trigger-printfield-hooks& variant dungeon player '[hp])
      (setf retval t))

    (when (want-redraw? player '[gold])
      (reset-redraw! player '[gold])
      (trigger-printfield-hooks& variant dungeon player '[gold])
      (setf retval t))

    (when (want-redraw? player '[depth])
      (reset-redraw! player '[depth])
      (print-depth variant (dungeon.depth dungeon) nil)
      (trigger-printfield-hooks& variant dungeon player '[depth])
      (setf retval t))

    (when (want-redraw? player '[health])
      (reset-redraw! player '[health])
      ;; fix
      (trigger-printfield-hooks& variant dungeon player '[health])
      (setf retval t))

    ;; extra-printing moved to variant
    ;; cut moved to variant
    ;; stun moved to variant
    ;; moved hunger to variant
    ;; moved blind to variant
    ;; moved confused to variant
    ;; moved afraid to variant
    ;; poisoned moved to variant
    ;; old 'state' moved to variant

    (when (want-redraw? player '[speed])
      (reset-redraw! player '[speed])
      (trigger-printfield-hooks& variant dungeon player '[speed])
      (setf retval t))

;;    )
    ;; moved study to variant

    (when (want-redraw? player '[equipment])
      (reset-redraw! player '[equipment])
      (update-inventory-row variant player)
      (setf retval t))

    ;; hackish, fix later
    (when (any-redraws? player)
      (warn "Unhandled redraw flags ~s" (get-list-of-redraws player)))

    retval))


(defmethod update-stuff ((variant variant) dungeon player)
  "Updates stuff according to previously requested updates."
  
  (unless (any-updates? player) (return-from update-stuff nil))
  
  (let ((retval nil))
    
    (when (want-update? player '[bonuses])
      (reset-update! player '[bonuses])
      (calculate-creature-bonuses! variant player)
      ;; move later
      (update-inventory-row variant player)
      (setf retval t))
    
    (when (want-update? player '[torch])
      (reset-update! player '[torch])
      (calculate-creature-light-radius! variant player)
      (setf retval t))
    
    (when (want-update? player '[hp])
      (reset-update! player '[hp])
      (calculate-creature-hit-points! variant player)
      (setf retval t))
            
    (when (want-update? player '[forget-view])
      (reset-update! player '[forget-view])
      (forget-view! dungeon player)
      (setf retval t))

    (when (want-update? player '[update-view])
      (reset-update! player '[update-view])
      (update-view! dungeon player)
      (setf retval t))

    (when (want-update? player '[forget-flow])
      (reset-update! player '[forget-flow])
;;      (forget-view! dungeon player)
      (setf retval t))

    (when (want-update? player '[update-flow])
      (reset-update! player '[update-flow])
;;      (update-view! dungeon player)
      (setf retval t))

    (when (want-update? player '[distance])
      (reset-update! player '[distance])
      (reset-update! player '[monsters])
      (update-monsters! variant dungeon t)
      (setf retval t))
    
    (when (want-update? player '[monsters])
      (reset-update! player '[monsters])
      (update-monsters! variant dungeon nil)
      (setf retval t))
    
    (when (want-update? player '[viewport])
      (reset-update! player '[viewport])
      (verify-viewport dungeon player)
      (setf retval t))


    (when (any-updates? player)
      (warn "Unhandled upd-flags ~s" (get-list-of-updates player)))

    
    retval))

(defun handle-stuff (variant dungeon player)
  (let ((retval nil))
    (when (any-updates? player)
      (setf retval (update-stuff variant dungeon player)))
    (when (any-redraws? player)
      (setf retval (or (redraw-stuff variant dungeon player) retval)))
    ;; add window-stuff
    retval))

(defun %locate-mouse-click-window (x y)
  "x and y are absolute coordinates. returns multiple values
window, relative x and relative y."
  
  (when (eq (get-system-type) 'sdl)
    ;; let us figure out that window:
    (let ((win nil))
      (loop for w across *windows*
	    for x-off = (window.x-offset w)
	    for y-off = (window.y-offset w)
	    do
	    (when (and (not (window.disabled? w))
		       (window.visible? w)
		       (>= x x-off) (< x (+ x-off (window.pixel-width w)))
		       (>= y y-off) (< y (+ y-off (window.pixel-height w))))
	      (setf win w)))

      (cond (win
	     (values win (- x (window.x-offset win))
		     (- y (window.y-offset win))))
	    (t
	     (warn "Totally unable to figure out which window the button was clicked!")
	     nil))
      )))


(defun %mouse-clicked (button x y)
  "Figures out what window it happened in and calls handle-mouse-click"
  (when (eq (get-system-type) 'sdl)

    (multiple-value-bind (win loc-x loc-y)
	(%locate-mouse-click-window x y)
      (handle-mouse-click *variant* win button loc-x loc-y))

      t))

(defvar *poll-time* 0)

(defun get-and-process-command! (dungeon player table &optional (poll nil))
  "remove me later"

  (let ((loc-table (gethash table *current-key-table*))
	(pre-time 0)
	(event nil))

    (loop
     (setf pre-time (org.langband.ffi:c-get-internal-time))
     (setf event (fetch-event *input-event* poll))
     (setf *poll-time* (- (org.langband.ffi:c-get-internal-time) pre-time))
     (assert (>= *poll-time* 0))
     (cond ((and poll (not event))
	    (return-from get-and-process-command! (values nil nil)))
	   (event
	    (cond ((eq (input-event.type event) :mouse)
		   (let ((m-ev (input-event.mouseclick event)))
		     ;; don't return now, maybe later
		     (%mouse-clicked (mouse-event.button m-ev)
				     (mouse-event.x m-ev)
				     (mouse-event.y m-ev))))
		  
		  ((eq (input-event.type event) :key)
		   (let* ((kev (input-event.keypress event))
			  (ch (kbd-event.key kev))
			  (check nil)
			  (fun nil))

		     ;; this is ultra-ugly!  check for -more- to be flushed
		     (when (eq (msghandler.state *message-handler*) :pause)
		       (advance-message-sys! *message-handler*)
		       (try-printing-messages! *message-handler*)
		       (return-from get-and-process-command! (values nil nil)))
		     
		     ;; add ALT-key later as well.. 
		     (when (kbd-event.shift kev)
		       (setf check (list 'shift ch)) ;; can avoid consing later if it is a problem.
		       ;;(warn "Looking for ~s" check)
		       (setf fun (check-keypress loc-table check)))
		     
		     (unless (functionp fun)
		       (setf check (kbd-event.key kev))
		       (setf fun (check-keypress loc-table check)))
		     
		     (cond ((functionp fun)
			    (return-from get-and-process-command!
			      (values (funcall fun dungeon player) t)))
			   (t
			    (warn "fell through key with ~s" kev)))
		     ))
		  (t
		   (warn "Unknown event ~s" (input-event.type event))))
	    ))
       )))


(defmethod handle-turn ((variant variant) (player player) (activity (eql :resting)))

  (let ((mode (get-information "rest-mode"))
	(dungeon *dungeon*))
    (move-player! dungeon player 5)
    (when (integerp mode)
      (decf mode)
      (if (plusp mode)
	  (setf (get-information "rest-mode") mode)
	  (setf (get-information "rest-mode") nil
		(get-information "resting") nil)))
    ;; hack
    (when (or (eq mode :full-rest)
	      (eq mode :normal-rest))
      (when (and (= (current-hp player) (maximum-hp player))
		 (= (current-mana player) (maximum-mana player)))
	(setf (get-information "rest-mode") nil
	      (get-information "resting") nil)))
    t))


(defun process-player! (variant dungeon player)
  "processes the player in a given turn"

 
  (let ((temp-attrs (player.temp-attrs player))
	(run-status nil)
	(run-dir nil)
	(rest-status nil))

    (block waste-energy
;;      (loop
       (setf run-status  (get-information "running" :default nil)
	     run-dir     (get-information "run-direction" :default 0)
	     rest-status (get-information "resting" :default nil))
       
       
       (when (any-updates? player) (update-stuff variant dungeon player))
       (when (any-redraws? player) (redraw-stuff variant dungeon player))
       
       ;;(put-cursor-relative! dungeon (location-x player) (location-y player))

       ;; assume no energy is used
       (setf (player.energy-use player) 0)
       	
       (cond ((and temp-attrs
		   (or (get-attribute-value '<paralysed> temp-attrs)
		       (>= (get-attribute-value '<cut> temp-attrs) 100))) ;; move to variant
	      (setf (player.energy-use player)  +energy-normal-action+))
	     
	     (rest-status
	      ;; this behaviour is in the wrong place.. it should be done in the regeneration phase
	      (handle-turn variant player :resting))
	     
	     ;; skip resting
	     ((and run-status (>= run-dir 0))
	      ;;(warn "from loop")
	      (unless (run-in-direction dungeon player run-dir)
		;;(warn "turning off running")
		(setf (get-information "run-direction") -1
		      (get-information "running") nil)))
	     
	     
	     ;; skip repeat
	     (t
	      ;; do normal command
	      (multiple-value-bind (retval did-event?)
		  (get-and-process-command! dungeon player :global +event-poll-mode+)
		(declare (ignore retval))
		;;(warn "get-command returned ~s ~s" retval did-event?)
		(unless did-event?
		  (return-from process-player! nil)))
	      ))
       
       (when (plusp (player.energy-use player))
	 (decf (get-creature-energy player) (player.energy-use player)))


       (when (plusp (player.energy-use player))
	 (return-from waste-energy t))

       (when (player.leaving? player)
	 (return-from waste-energy t))
       ;;)
      )

    t))


(defun regenerate-hp! (crt percent)
  "Tries to regenerate the creature, and includes percent-chance."
  
  (let* ((regen-base 1442)
	 (old-hp (current-hp crt))
	 (new-hp (+ (* (maximum-hp crt) percent) regen-base))
	 (max-short 32767)
	 (increase (int-/ new-hp (expt 2 16)))
	 (new-frac (+ (player.fraction-hp crt)
		      (logand new-hp #xffff)))
	 )

    (incf (current-hp crt) increase)

    (when (and (minusp (current-hp crt))
	       (plusp old-hp))
      (setf (current-hp crt) max-short))

    (if (> new-frac #x10000)
	(progn
	  (setf (player.fraction-hp crt) (- new-frac #x10000))
	  (incf (current-hp crt)))
	(setf (player.fraction-hp crt) new-frac))

    (when (>= (current-hp crt)
	      (maximum-hp crt))
      (setf (current-hp crt) (maximum-hp crt)
	    (player.fraction-hp crt) 0))

    (when (/= old-hp (current-hp crt))
;;      (warn "Regenerated..")
      (ask-for-redraw! crt '[hp]))
      
    (current-hp crt)))

;;(trace regenerate-hp!)

(defmethod process-world& ((variant variant) (dungeon dungeon) (player player))
  "tries to process important world-stuff every 10 turns."

  (let ((the-turn (variant.turn variant)))

    (unless (= 0 (mod the-turn 10)) ;; every 10 turns only
      (return-from process-world& nil))

    ;; see variant for real code!

    t))
 
(defun energy-for-speed (crt)
  (aref *energy-table* (get-creature-speed crt)))
			 
#||
(defun energise-creatures! (variant dungeon player)

  (incf (get-creature-energy player) (energy-for-speed player))

  ;; boost all monsters
  (with-dungeon-monsters (dungeon mon)
    (declare (ignore dungeon))
    (incf (get-creature-energy mon) (energy-for-speed mon)))
  
  ;; can our dear player do anything?

  (loop named player-fun
	while (and (>= (get-creature-energy player) +energy-normal-action+) ;; better solution?
		   (not (player.leaving? player)))
	do
	(progn
	  (process-monsters& dungeon player (1+ (get-creature-energy player)))
	  
	  (unless (player.leaving? player)
	    (process-player! variant dungeon player))))
  )
||#

(defun do-possible-repaints! (variant)
  "Checks windows and does any wanted repaints."

  ;;(warn "Check for repaints")
  ;; ensure that everyone gets served
  (loop for win across *windows*
	do
	(when (and win
		   (window.repaint? win))
	  (when (is-frame-shown? variant win)
	    ;;(warn "repainted ~s" win)
	    ;;(refresh-window win +winflag-delay-paint+)
	    (flush-window win)
	    )
	  (setf (window.repaint? win) nil))))
  
(defvar *action-priority-queue* nil)

(defun run-level! (level player)
  "a loop which runs a dungeon level"

  (unless (level.dungeon level)
    (warn "No legal DUNGEON object, did you load a dead savefile?")
    (setf (creature-alive? player) nil
	  (player.dead-from player) "fatal failure"
	  (player.leaving? player) :quit)
    
    (return-from run-level! nil))
  
  (let* ((dungeon (level.dungeon level))
	 (var-obj *variant*)
	 (variant *variant*)
	 (*dungeon* dungeon)
	 (dungeon-height (dungeon.height dungeon))
	 (dungeon-width  (dungeon.width dungeon))
	 )

    ;; we're not leaving
    (setf (player.leaving? player) nil
	  (player.target player) nil)
      
    ;; setting these to illegal values
    (setf (player.view-x player) dungeon-width
	  (player.view-y player) dungeon-height)
  
    ;; no stairs from town
    ;;    (setf (dungeon.up-stairs-p dungeon) nil
    ;;	  (dungeon.down-stairs-p dungeon) nil)
    

    ;; create stairs.. (postponed)

    ;; postpone verify of viewport
    (verify-viewport dungeon player)
    
    ;;(print-message! nil) ;; flushes

    ;;; == this section needs serious rework.. see angband
    ;; postpone flush


    (ask-for-update! player '[bonuses])
    (ask-for-update! player '[torch])
    (ask-for-update! player '[hp])

    (ask-for-update! player '[forget-view])
    (ask-for-update! player '[update-view])
    (ask-for-update! player '[distance])
    
    
    (ask-for-redraw! player '[map])
    (ask-for-redraw! player '[basic])
    (ask-for-redraw! player '[extra])
    

    (update-stuff var-obj dungeon player)

    (redraw-stuff var-obj dungeon player)

    (org.langband.ffi:c-flip-framebuffer)

    ;; can be moved inside loop below for _very_ frequent checking
    #+langband-extra-checks
    (progn
      (assert (eq player *player*))
      (assert (eq dungeon *dungeon*))
      (assert (eq var-obj *variant*))
      (assert (ok-object? player :context :in-game :warn-on-failure t))
      (assert (ok-object? dungeon :context :in-game :warn-on-failure t))
      (assert (ok-object? var-obj :context :in-game :warn-on-failure t))
      )

    
    (let* ((pq (dungeon.action-queue dungeon)) 
	   (pq-arr (make-array 200 :initial-element nil)) ;; hack, fix to a better value later
	   (last-world-process -1)
	   (cur-action nil)
	   
	   ;;(flip-time 0)
	   ;; #+timer-code
	   #||
	   (targetfps 60.0) ;; should be good
	   (framerate 0) ;; for our pleasure
	   (framecount 0)
	   (framefactor 0)
	   (first-timer 0)
	   (cur-timer 0)
	   (prev-timer 0)
	   
	   (timediff 0)
	   (ticks-pr-second 1000.0) ;; msecs
	   ||#
	   )


      ;; we let the player get some randomness too before we stuff him in
      (incf (get-creature-energy player) (random 20))
      (lb-ds:pq.put pq player (get-creature-energy player))

      #+timer-code
      (setf first-timer (org.langband.ffi:c-get-internal-time)
	    cur-timer first-timer
	    prev-timer first-timer
	    framefactor (/ ticks-pr-second targetfps))
      
      (block main-dungeon-loop

	(loop
	 ;;(delay 10)
	 (let ((skip-rest-of-loop nil)
	       (front nil))

	   #+timer-code
	   (progn
	     (setf cur-timer (org.langband.ffi:c-get-internal-time)
		   timediff (- cur-timer prev-timer)
		   *poll-time* 0
		   *current-speed-factor* (/ timediff framefactor))
	     
	     (when (<= *current-speed-factor* 0) ;; never??
	       (setf *current-speed-factor* 0.1))
	     
	     (setf framerate (/ targetfps *current-speed-factor*))
	     )
	   
	   (setf cur-action 'nothing)

	   (try-printing-messages! *message-handler*) ;; maybe a loop here or in the call?

	   #+timer-code
	   (when (consp *visevents*)
	     (setf cur-action 'visevents)
	     (let ((visevts '())
		   (blocking? nil))

	       (dolist (evt *visevents*)
		 (assert (typep evt 'visual-event))

		 (when (eq (visevent.mode evt) :fresh)
		   (init-visual-event evt dungeon player cur-timer))
		 
		 (when (eq (visevent.mode evt) :active)
		   (trigger-visual-event evt dungeon player cur-timer))

		 (when (eq (visevent.mode evt) :done)
		   (finalise-visual-event evt dungeon player cur-timer))

		 (unless (eq (visevent.mode evt) :dead)
		   (push evt visevts))
		 )
	   
	       (setf *visevents* (nreverse visevts))
	   
	       (dolist (i *visevents*)
		 (when (visevent.blocking? i)
		   (setf blocking? t)))

	       (when blocking?
		 (setf skip-rest-of-loop t))
	       ))

	   (unless skip-rest-of-loop

	     (setf front (lb-ds:pq.top pq))
	   
	     ;;(warn "doing ~s at ~s vs ~s" front (get-creature-energy front) +energy-normal-action+)
	     (refresh-window *map-frame*)

	     ;; fix!
	     (when (basic-frame-shown? var-obj)
	       (when-bind (win (get-window +charinfo-frame+))
		 (refresh-window win +winflag-delay-paint+)))
	     
	     (when (is-frame-shown? var-obj +tiledfields-frame+)
	       (when-bind (win (get-window +tiledfields-frame+))
		 (refresh-window win +winflag-delay-paint+)))

	     (cond ((> (get-creature-energy front) +energy-normal-action+) ;; he or she can do an action
		    (etypecase front
		      (player
		       (setf cur-action 'player)
		       (lb-ds:pq.get pq)
		       ;;(warn "<process player>")
		       (process-player! variant dungeon player)
		       ;;(warn "</process player>")
		       (verify-viewport dungeon player) ;; hack
		       (lb-ds:pq.put pq player (get-creature-energy player)))

		      (active-monster
		       (setf cur-action 'monster)
		       (let* ((mon (lb-ds:pq.get pq))
			      (mx (location-x mon))
			      (my (location-y mon)))
			 ;;(check-type mon active-monster)
			 (when (creature-alive? mon)
			   ;; NOTE! TAPPING ENERGY HERE!
			   (decf (get-creature-energy mon) +energy-normal-action+)
			   ;; skip the 'sensing' of player
			   ;; only do something if there is clear sight
			   (when (player-has-los-bold? dungeon mx my)
			     (process-single-monster! variant dungeon player mon)))

			 (when (creature-alive? mon)
			   (lb-ds:pq.put pq mon (get-creature-energy mon)))
		       
			 ))))
		 
		   ;; not even the front one has enough energy, boost them all
		   (t
		    (setf cur-action 'boost)

		    (let ((count (lb-ds:pq.count pq)))
		      ;; resize when we have to
		      (when (<= (length pq-arr) count)
			(setf pq-arr (make-array (+ 50 count) :initial-element nil)))
		      
		      (loop for cnt from 0 below count
			    do
			    (setf (aref pq-arr cnt) (lb-ds:pq.get pq))))
		  
		    (loop for i from 0
			  for x across pq-arr
			  do
			  (progn
			    (when (and x (creature-alive? x))
			      (incf (get-creature-energy x) (energy-for-speed x))
			      (lb-ds:pq.put pq x (get-creature-energy x)))
			    (setf (aref pq-arr i) nil)))
		
		  
		    ;; this should be a diff vs the last entry
		    (incf (variant.turn var-obj))

		    ;;(warn "Boosted")
		    #||

		    (loop for i from 1 to (lb-ds::heap-size pq)
		    do (format t "~&~d ~a~%" (lb-ds::pq-elem-priority (aref (lb-ds::heap-array pq) i))
		    (get-creature-name (lb-ds::pq-elem-value (aref (lb-ds::heap-array pq) i)))))

		    (warn "---")
		    ||#
		    ))
	   
	     (let ((leave-sym (player.leaving? player)))
	       (when leave-sym
		 (return-from run-level! leave-sym)))
	   
	     (when (any-updates? player) (update-stuff var-obj dungeon player))
	   
	     (when (/= (variant.turn variant) last-world-process)
	       (process-world& var-obj dungeon player)
	       (setf last-world-process (variant.turn variant)))
	   
	     ;; do other stuff
	     ;; hack
	     (when (any-updates? player) (update-stuff var-obj dungeon player))
	     (when (any-redraws? player) (redraw-stuff var-obj dungeon player))
	     )

	   ;; right place?
	   (try-printing-messages! *message-handler*) ;; maybe a loop here or in the call?

	   (do-possible-repaints! var-obj)
	   
	   ;; not 100% sure on the following ones
	   (unless *turn-mode*
	     (clear-old-tiles! *map-frame* dungeon player)
	     )

	   #+flip-mode
	   (progn
	   ;; maybe draw all tiles, check speed for that later
	     (draw-active-objects *map-frame* dungeon player)

	     (draw-creatures *map-frame* dungeon player)
		   
	     ;;(let ((prev-timer (org.langband.ffi:c-get-internal-time)))
	       (org.langband.ffi:c-flip-framebuffer)
	     ;;  (setf flip-time (- (org.langband.ffi:c-get-internal-time) prev-timer)))
	     )
	   
	   #||
	   ;; add delay to keep framerate
	   (block delay-loop
	     (let ((new-diff 0))
	       (loop
		(setf new-diff (- (org.langband.ffi:c-get-internal-time) prev-timer))
		;;(warn "Diff is ~s, delay is ~s" new-diff (- framefactor new-diff 3))
		(when (> new-diff framefactor)
		  ;;(warn "Return")
		  (return-from delay-loop t))

		;;(if (> (- framefactor new-diff 2) 10)
		;;    (delay (floor (- framefactor new-diff 2)))
		;;    (= new-diff 20)) ;; dummy
		)))
	   ||#

	   
	   #+timer-code
	   (progn
	     (incf framecount)
	     (setf prev-timer cur-timer))

	   #||
	   (let ((*print-case* :downcase))
	     (warn "Frametime: ~s (~s) (~s)"
		   (- (org.langband.ffi:c-get-internal-time) prev-timer flip-time *poll-time*)
		   flip-time cur-action))
	   ||#
	   
;;	   (warn "~10a: factor: ~5f    diff: ~3d    rate: ~4f   speed: ~4f"
;;		 cur-action framefactor timediff framerate
;;		 *current-speed-factor*)
	   
	   #||
	   ;;(warn "end loop")
	   ;; hack
	   (when (= (mod framecount (floor targetfps)) 0)
	     (warn "Rate: ~s - ~s frames took ~s msecs"
		   (/ ticks-pr-second
		      (/ (- cur-timer first-timer)
			 framecount))
		   framecount (- cur-timer first-timer) ))
	   ||#
	   
	   )) ;; end of main loop

	))
    ))


(defun remove-monster-from-dungeon! (dungeon monster)
  "Tries to remove the monster from the dungeon."
  
  (let ((mx (location-x monster))
	(my (location-y monster)))
    (setf (creature-alive? monster) nil
	  (dungeon.monsters dungeon) (delete monster (dungeon.monsters dungeon))
	  (cave-monsters dungeon mx my) nil)
    (light-spot! dungeon mx my)
    
    (when (is-targeting? *player* monster)
      (setf (player.target *player*) nil))
    
    monster))


(defun game-loop& ()
  "This is the main game-loop.  and this function looks _ugly_."
  (multiple-value-setq (*player* *variant* *level*)
    (load-old-environment&))
  ;;(update-term-sizes!)
  (loop
   ;; clean up to prevent too many delays while running the dungeon
   ;; it may take quite some time
   (lbsys/garbage-collect :global t)
     
   ;; let's run this dungeon
     
   (let ((how-level-was-left nil))
       
     (setq how-level-was-left (run-level! *level* *player*))
       
     ;;(lbsys/tricky-profile
     ;;(setq how-level-was-left (run-level! *level* *player*))
     ;;:space)
       
     ;; return if we're toast
     (when (or (not (creature-alive? *player*))
	       (eq (player.leaving? *player*) :quit))
       ;;(warn "->End game and level is ~s" *level*)
       (return-from game-loop&))
       
     ;; generate new cave
     (setq *level* (create-appropriate-level *variant* *level*
					     *player* (player.depth *player*)))
       
     (activate-object *level* :player *player*
		      :leave-method how-level-was-left)

     ;; do it again?
     (lbsys/garbage-collect :global t)
     ;; safety? we will reload in less than a second :-)
     (save-current-environment&))
   ))


(defun save-current-environment& ()
  "Attempts to save the environment."
  (setf (get '*player* 'last-value) *player*
	(get '*variant* 'last-value) *variant*
	(get '*level* 'last-value) *level*)
  'last-value)

(defun load-old-environment& ()
  "Returns three values with an old environment."
  (values (get '*player* 'last-value)
	  (get '*variant* 'last-value)
	  (get '*level* 'last-value)))

(defun %load-saved-game (fname)
  "Will assign values to *variant*, *player* and *level if it can."

  ;; use default loader
  (let ((loaded nil)
	(pname fname)
	(format :binary))

    (when (stringp pname)
      (setf pname (pathname fname)))

    (when (equal (pathname-type fname) "lisp")
      (setf format :readable))
    
    (handler-case
	(setf loaded (load-a-saved-game nil fname format))
      (savefile-problem (sp)
	(pause-last-line! :msg (saveproblem.desc sp) :attr +term-l-red+)
	(pause-last-line! :msg "[Creating new character instead.]" :attr +term-l-red+)
	)
      (end-of-file (co)
	(pause-last-line! :attr +term-l-red+
			  :msg (format nil "Error when loading a savegame (~s), creating new character." co))))
    
    ;; we're lenient about the order things are returned in
    (dolist (i loaded)
      (typecase i
	(player (setf *player* i))
	(level (setf *level* i))
	(variant (setf *variant* i))
	(null ;; a nil value is always total miss
	 (return-from %load-saved-game nil))
	(otherwise
	 (warn "Loading gave weird value back: ~s" i))))

    (%load-window-textures *variant*)
    
    nil))

(defun interactive-variant-select ()
  "Returns id to a selected variant, or NIL on failure."
  ;; move this to variant-selection
  (let ((vars (get-registered-variants)))
   
    (cond ((> (length vars) 1)
	   (let ((hgt (get-frame-height))
		 (result nil))
	     (clear-window-from *cur-win* (- hgt 4))
	     (setf result  (interactive-alt-sel 5 (- hgt 3) vars :ask-for "variant to play"))
	     
	     (when (and (integerp result) (>= result 0))
	       (return-from interactive-variant-select (elt vars result)))
	     
	     (warn "Got odd variant selection result ~s" result)
	     ;; just using the first one
	     (first vars)))
	  
	  ((= (length vars) 0)
	   (error "No variant plugin found."))
	  
	  (t
	   (first vars)))))
 

(defun interactive-savefile-select (variant-id)
  "Returns 'new-game for new game or a string with filename to wanted savegame to load."
  
  ;;; This function is ugly.. be careful
  
  (unless (stringp variant-id)
    (return-from interactive-savefile-select nil))
  
  ;; now we should find savegames, or select new game
  (let* ((files (lbsys/directory (variant-save-directory variant-id))))
    
    (unless files ;; we have no files, assume new game
      (return-from interactive-savefile-select 'new-game))
    ;;(warn "files ~s" files)
    ;; only select binaries
    (let ((real-files '()))
      (dolist (i files)
	(let ((type (pathname-type i)))
	  (cond ((equal type nil)
		 nil) ;; do nothing
		
		((equal type "bin") ;; we want this one
		 ;;(warn "found ~s" i)
		 (when-bind (header (load-saveheader i))
		   (check-type header saveheader)
		   ;;(warn "Header is ~s" header)
		   (when (plusp (saveheader.status header))
		     (push (cons i (saveheader.desc header)) real-files))))
		
		((equal type "lisp") ;; we ignore this one
		 #||
		 #-langband-release
		 (progn
		   (push (cons i "unknown lisp-file") real-files))
		 ||#
		 nil)
		
		(t ;; check for badness
		 (warn "Unknown filetype ~s for file ~s.  Ignoring." type i)))))
      
    
      ;;(warn "files ~s" real-files)
    
      (flet ((show-desc (num)
	       (cond ((plusp num)
		      (let ((elm (elt real-files (1- num))))
			;;(warn "Want desc of ~s -> ~s" num elm)
			(cdr elm)))
		     (t
		      "                     "))))
      
	(cond ((= (length real-files) 0)
	       (return-from interactive-savefile-select nil))
	      ((> (length real-files) 0) ;; if more files we need choices
	     
	       (let ((hgt (get-frame-height))
		     (to-show (loop for i in real-files
				    collecting (pathname-name (car i))))
		     (result nil))
		 (clear-window-from *cur-win* (- hgt 6))
		 (let ((sets (get-settings-obj "savefile-selection")))
		   (setf (gethash "text-y" sets) (- hgt 5))
		   (setf result  (interactive-alt-sel 5 (- hgt 3) (cons "<<NEW>>" to-show)
						      :ask-for "savefile to use"
						      :display-fun #'show-desc
						      :mod-value 5
						      ;; hack
						    :settings sets)))
		 
		 (when (integerp result)
		   (cond ((= result 0)
			  (return-from interactive-savefile-select 'new-game)) ;; new game
			 ((> result 0)
			  (return-from interactive-savefile-select (elt real-files (1- result))))))
		 ))))
	     
	     
      nil)))

(defun %create-new-character (wanted-variant)
  "Handles creation of a new character in given variant."

  (unless wanted-variant
    (error "Tried to created new character but no variant is specified."))

  ;; so far we just load vanilla
  (let ((var-obj (load-variant& wanted-variant :verbose t)))
    (cond ((not var-obj)
	   (warn "Unable to find variant ~s" wanted-variant)
	   (return-from %create-new-character nil))
	  (t
	   (setf *variant* var-obj)
	   (activate-object var-obj))))
  (when (eq (get-system-type) 'sdl)
    (%load-window-textures *variant*))

  ;; then it's time to actually create our player (with a dummy level)
  (let* ((*level* (make-instance 'level)) ;; evil hack
	 (pl (interactive-creation-of-player *variant*)))
    ;; call event
    (on-new-player *variant* pl)
    
    pl))

;;; This one is a mess!!! please divide in more fitting functions!
(defun play-game& ()
  "Should not be called directly."

  (setf *screen-height* (lb-ffi:c-get-window-height)
	*screen-width* (lb-ffi:c-get-window-width))
  
  ;;(warn "back in lisp!! ~s ~s" (lb-ffi:c-get-window-width) (lb-ffi:c-get-window-height))
  
  
  (unless (update-term-sizes!)
    (return-from play-game& nil))
  
  (let ((*player* nil)
	(*level* nil)
;;	#+allegro
;;	(old-spread (sys:gsgc-parameter :generation-spread))
	)

    (when (eq (get-system-type) 'gcu)
      (setf *map-frame* +asciimap-frame+)
      (loop for x across *windows*
	    when x
	    do (setf (window.gfx-tiles? x) nil)))

    (loop for x across *windows*
	  when x 
	  do (establish-data-in-window x))

    (activate-window +full-frame+)
    (setf *cur-win* (get-window +full-frame+))
    
    (when (eq (get-system-type) 'gcu)
      (fetch-event *input-event* t)) ;; hack to wake up the gcu side
    
    ;; !!check if we use gfx first!!
    (cond ((eq (get-system-type) 'sdl)
	   (let ((splash-wid 640)
		 (splash-hgt 480)
		 (win-wid (window.width *cur-win*))
		 (win-hgt (window.height *cur-win*))
		 (splash-idx (load-image& *variant* '(engine-gfx "other/langtitle.png") 1 0)))
	     
	     (when (plusp splash-idx)
	       ;; make all white first
	       (fill-area *cur-win* splash-idx 0 0 0
			  (1- win-wid)
			  (1- win-hgt)) ;; hack
	       
	       ;; paint splash in middle of screen
	       (let* ((win-pwid (window.pixel-width *cur-win*))
		      (win-phgt (window.pixel-height *cur-win*))
		      (tile-wid (window.tile-width *cur-win*))
		      (tile-hgt (window.tile-height *cur-win*))
		      (x (truncate (int-/ (- win-pwid splash-wid) 2) tile-wid))
		      (y (truncate (int-/ (- win-phgt splash-hgt) 2) tile-hgt)))

		 (paint-gfx-image *cur-win* splash-idx x y))
	       )))
	  
	  ((eq (get-system-type) 'gcu)
	   (with-open-file (s (game-data-path "text-splash.txt")
			      :direction :input)
	     (loop for x = (read-line s nil 'eof)
		   for i from 0
		   until (eq x 'eof)
		   do
		   (put-coloured-str! +term-white+ x 0 i)))
	   ))


    ;;(read-one-character)
    ;;(pause-last-line!)

    (print-note! "[Initing sound-system]")

    #+use-sound
    (progn
    (let ((sstatus (init-sound-system& 40))) ;; fix this later
      (when sstatus
	(print-note! "[Activating sound-system]")
	(org.langband.ffi:c-activate-sound-system&)))

    ;; disabled this until things are working properly
    #||
    ;;(warn "load")
    (load-music "opening_01.ogg")
    (load-music "opening_02.ogg")
    (load-music "langband_tune01.ogg")
    (load-music "langband_tune02.ogg")
    ;;(warn "play")
    (play-music 3)
    ||#
    (load-music "langband_tune04.ogg")
    (play-music 0 1)
    )
    
    (print-note! "[Initialization complete]")

    ;;(pause-last-line!)
     ;; end init_ang
     
    (let ((*load-verbose* nil))
      (load-game-data "prefs.lisp")
      (load-game-data "settings.lisp"))
    
    ;; hack to remove cursor
    (set-cursor-visibility nil)
    ;;(flush-messages! :forced t)

    (pause-last-line!)

    (let* ((wanted-variant (interactive-variant-select))
	   (start-action nil))

      (unless (and wanted-variant (stringp wanted-variant))
	(error "Unable to find any variant ~s" wanted-variant))

      ;;(warn "*variant* here is ~s ~s" *variant* wanted-variant)
      
      (setf start-action (interactive-savefile-select wanted-variant))

      (when (consp start-action)
	(setf start-action (car start-action)))
      
      ;;(warn "start ~s ~s" wanted-variant start-action)

      (when (or (pathnamep start-action)
		(stringp start-action))
	;; we found a savefile we want
	(print-note! "[Trying to load savefile ..]")
	(%load-saved-game start-action))
      
      (when (or (eq start-action nil)
		(eq start-action 'new-game)
		(eq *player* nil)) ;; in case we screwed up loading a game
	;; start a new game, no savefiles with player found
	(setf *player* (%create-new-character wanted-variant)))
      )
    
    (load-user-variant-prefs& *variant*)
    
    ;; at this point *player* and *variant* must be correct.. *level* can be fixed
    
    (unless *current-key-table*
      (setf *current-key-table* *angband-keys*))

    ;; we must make sure that our player has proper symbol
    ;; this sets symbol to the (animated) hobbit
    ;;(setf (slot-value *player* 'gfx-sym) (tile-paint-value 43 30))
    
    (multiple-value-bind (file tile)
	(get-class-tile *variant* *player*)
      (setf (slot-value *player* 'gfx-sym) (tile-paint-value file tile)))
    
    
;;    (unless *level*
;;      (put-coloured-line! +term-white+ "Please wait..." 0 0)  
;;      (pause-last-line!))

    ;; check our map-mode
    (cond ((eq (default-setting "map") :ascii)
	   (setf *map-frame* +asciimap-frame+
		 *current-map-mode* :ascii))
	  ((eq (default-setting "map") :gfx-tiles)
	   (setf *map-frame* +gfxmap-frame+
		 *current-map-mode* :gfx-tiles))
	  (t
	   ;; use defaults, whatever they are
	   ))
	  
    
    ;; now we want normal layout!
    (switch-to-regular-frameset&)

    ;; construct (wanted) message-handler and init it
    (let ((default-msg-handler 'message-handler-flow)
	  (defhandler (default-setting "message-handler")))
      
      (when (or (stringp defhandler) (nonboolsym? defhandler))
	(cond ((string-equal (string defhandler) "flow")
	       (setf default-msg-handler 'message-handler-flow))
	      ((string-equal (string defhandler) "more")
	       (setf default-msg-handler 'message-handler-more))
	      (t nil)))
      
      ;; produce wanted message-handler
      (setf *message-handler* (make-instance default-msg-handler))
      (init-message-system& *message-handler*))

;;    #+sbcl
;;    (sb-pcl::precompile-random-code-segments)
    
    #||
    (loop for x across *windows*
	  do
	  (warn "Win ~s, has ~s,~s offset and ~s,~s coords ~s"
		(window.id x) (window.x-offset x) (window.y-offset x)
		(window.pixel-width x) (window.pixel-height x)
		(window.visible? x)))
    ||#
    
    
    (setf *cur-win* (aref *windows* *map-frame*))
    ;;(warn "Curwin is ~s" *cur-win*)

    ;; here the game starts
    (on-game-start *variant* *player*)
    
    (block dungeon-running
      (unless *level* 
	(setf *level* (create-appropriate-level *variant* *level*
						*player* (player.depth *player*))))
      (unless (activated? *level*)
	(activate-object *level* :player *player*
			 :leave-method nil))
      
      (save-current-environment&)
      (game-loop&))

    ;;(warn "End game and level is ~s" *level*)
    (cond ((and (is-player? *player*))
	   (texture-background! +full-frame+ "" -1)
	   (flush-messages! :forced t)
	   (arrange-game-exit& *variant* *player*))
	   
	  (t
	   (put-coloured-line! +term-white+ "Quitting..." 0 0)

	   ))
    
    ;;(pause-last-line!)
    (quit-game&)
    t))

;; low-level definitions, move it somewhere else later..
#+allegro
(ff:defun-foreign-callable c-callable-play ()
  (play-game&))

#+allegro
(ff:defun-foreign-callable c-callable-resize (w h)
  (%adjust-screen-size w h))

#+allegro
(ff:defun-foreign-callable c-callable-mouseclick (button x y)
  (%mouse-clicked button x y))

#+lispworks
(fli:define-foreign-callable ("LB_PlayGame" :result-type :void) ()
;;  (warn "callback play")
  (play-game&))

#+lispworks
(fli:define-foreign-callable ("LB_AdjustSize" :result-type :void)
    ((w :int) (h :int))
;;  (warn "Resize to ~s ~s" w h)
  (%adjust-screen-size w h))

#+lispworks
(fli:define-foreign-callable ("LB_MouseClicked" :result-type :void)
    ((button :int) (x :int) (y :int))
  (%mouse-clicked button x y))
