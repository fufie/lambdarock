;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/keys.lisp - key-definitions - keyassignments in config
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

(define-key-operation 'toggle-run-mode
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let ((run-status (get-information "running" :default nil)))
	  (cond (run-status
		 (stop-creature-activity *variant* player :running))
		(t
		 ;;(warn "toggle to ~s" run-status)
		 (setf (get-information "running") (not run-status)
		       (get-information "run-direction") -1
		       ))))
	))

(defun van-move-player! (dungeon player direction)
;;  (warn "move -> ~s" direction)
  (cond ((get-information "running" :default nil)
	 (unless (run-in-direction dungeon player direction)
	   (stop-creature-activity *variant* player :running)
	   ))
	(t
	 (move-player! dungeon player direction))))

(defun %run-direction (dungeon player dir)
  ;;(warn "Start walking-sound")
  (play-sound "walking" :channel 0 :loops 200)
  (setf (get-information "running") t)
  (van-move-player! dungeon player dir))



(define-key-operation 'move-up
    #'(lambda (dungeon player) (van-move-player! dungeon player 8)))

(define-key-operation 'move-up-left
    #'(lambda (dungeon player) (van-move-player! dungeon player 7)))

(define-key-operation 'move-up-right
    #'(lambda (dungeon player) (van-move-player! dungeon player 9)))

(define-key-operation 'move-right
    #'(lambda (dungeon player) (van-move-player! dungeon player 6)))

(define-key-operation 'move-left
    #'(lambda (dungeon player) (van-move-player! dungeon player 4)))

(define-key-operation 'move-down
    #'(lambda (dungeon player) (van-move-player! dungeon player 2)))

(define-key-operation 'move-down-left
    #'(lambda (dungeon player) (van-move-player! dungeon player 1)))

(define-key-operation 'move-down-right
    #'(lambda (dungeon player) (van-move-player! dungeon player 3)))

(define-key-operation 'run-up
    #'(lambda (dungeon player) (%run-direction dungeon player 8)))

(define-key-operation 'run-up-left
    #'(lambda (dungeon player) (%run-direction dungeon player 7)))

(define-key-operation 'run-up-right
    #'(lambda (dungeon player) (%run-direction dungeon player 9)))

(define-key-operation 'run-right
    #'(lambda (dungeon player) (%run-direction dungeon player 6)))

(define-key-operation 'run-left
    #'(lambda (dungeon player) (%run-direction dungeon player 4)))

(define-key-operation 'run-down
    #'(lambda (dungeon player) (%run-direction dungeon player 2)))

(define-key-operation 'run-down-left
    #'(lambda (dungeon player) (%run-direction dungeon player 1)))

(define-key-operation 'run-down-right
    #'(lambda (dungeon player) (%run-direction dungeon player 3)))

(define-key-operation 'stand-still
    #'(lambda (dungeon player) (van-move-player! dungeon player 5)))

(define-key-operation 'rest
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-frame (+message-frame+)
	  (flush-messages! :forced t)
	  (let ((how-long (get-string-input "Rest (0-9999, '*' for HP/SP, '&' as needed): " :max-length 4))
		(mode nil))
	    (when how-long
	      (cond ((equal how-long "&")
		     (setf mode :full-rest))
		    
		    ((or (equal how-long "*")
			 (= (length how-long) 0))
		     (setf mode :normal-rest))
		    
		    ((every #'digit-char-p how-long)
		     (setf mode (parse-integer how-long)))
		    
		    (t
		     (format-message! "Unknown argument '~a'" how-long)))

	      (when mode
		(setf (get-information "rest-mode") mode
		      (get-information "resting") t))

	      t)))
	))

(define-key-operation 'show-equipment
     #'(lambda (dungeon player)
	 (declare (ignore dungeon))
	 (with-dialogue ()
	   (clear-window *cur-win*) ;; hack
	   (let ((table (player.equipment player)))
	     (item-table-print table :show-pause t :start-x 3))
	   (refresh-window +dialogue-frame+))
	 ))

(define-key-operation 'show-inventory
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(with-dialogue ()
	  (clear-window *cur-win*) ;; hack
	  (let* ((backpack (get-creature-inventory player))
		 (inventory (aobj.contains backpack)))
	    (item-table-print inventory :show-pause t :start-x 3))
	  (refresh-window +dialogue-frame+))
	))
  
(define-key-operation 'show-character
    #'(lambda (dungeon player)
	(flush-messages! :forced t)


	(with-full-frame ()
		       
	  (clear-window *cur-win*)
	    
	  (block display-input 
	    (let ((loc-table (gethash :display *current-key-table*))
		  (alts (make-note-buttons +full-frame+
					   '((#\C "'C' to show combat-info")
					     (#\R "'R' to show resists")
					     (#\Escape "ESC to continue"))
					   :text-colour +term-white+
					   :button-colour :matte-brown)))
	      (loop
	       (clear-window +full-frame+ +foreground+)
	       (load-setting-background *variant* *cur-win* (get-setting *variant* :char-display))
	       (display-creature *variant* player)
	       (paint-window +full-frame+)
	       (flush-window +full-frame+)

	       (loop for i in alts do
		     (buttonify-selectable-ui-object *cur-win* i))
	       
	       (let* ((ch (select-displayed-alternative alts)) ;;(read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(funcall fun dungeon player))
		       ((eql ch +escape+)
			(return-from display-input t))
		       (t
			;; nil
			)))
	       )))
	  ;;(texture-background! +full-frame+ "" -1)
	  (clear-window +full-frame+)

	  )))


(define-key-operation 'go-downstairs
    #'(lambda (dungeon player) (use-stair! dungeon player :down)))

(define-key-operation 'go-upstairs
    #'(lambda (dungeon player) (use-stair! dungeon player :up)))

(define-key-operation 'quit-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
;;	(warn "Quitting")
	(with-frame (+query-frame+)
	  (put-coloured-line! +term-white+ "Are you sure you wish to quit [will kill character]? " 0 0)
	  (let ((chr (read-one-character)))
	    (when (or (equal chr #\y)
		      (equal chr #\Y))
	      (setf (creature-alive? player) nil
		    (player.dead-from player) "quitting"
		    (player.leaving? player) :quit)
	      
	    )))
	))

(define-key-operation 'get-item
    #'(lambda (dungeon player)
;;	(with-new-screen ()
	(pick-up-from-floor! dungeon player)))
  

(define-key-operation 'drop-item
    #'(lambda (dungeon player)
	(interactive-drop-item! dungeon player)))

(define-key-operation 'take-off-item
    #'(lambda (dungeon player)
	(interactive-take-off-item! dungeon player)))

(define-key-operation 'destroy-item
    #'(lambda (dungeon player)
	(interactive-destroy-item! dungeon player)))

(define-key-operation 'wear-item
    #'(lambda (dungeon player)
	(interactive-wear-item! dungeon player
				:selection-function
				#'(lambda (table key obj)
				    (declare (ignore table key))
				    (or (typep obj 'active-object/weapon)
					(typep obj 'active-object/armour)
					(typep obj 'active-object/ring)
					(typep obj 'active-object/light-source)
					(typep obj 'active-object/neckwear))
				    ;;(warn "Checking ~s ~s ~s" (lbsys/class-name table) key (get-id obj))
				    ))))

(define-key-operation 'use-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player)))

(define-key-operation 'throw-item
    #'(lambda (dungeon player)
	(interactive-throw-item! dungeon player)))

(define-key-operation 'quaff-potion
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:quaff)
			       :which-use :quaff
			       :limit-from '(:backpack :floor) ;; only place with potions
			       :prompt "Quaff which potion")))

(define-key-operation 'zap-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:zap)
			       :which-use :zap
			       :limit-from '(:backpack :floor) ;; only place with zappers I think
			       :prompt "Zap which stick")))
	


(define-key-operation 'read-text
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:read)
			       :limit-from '(:backpack :floor) ;; only place with scrolls
			       :which-use :read
			       :prompt "Read which scroll?")))

(define-key-operation 'eat-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:eat)
;;			       :selection-function #'(lambda (x)
;;						       (typep x 'active-object/food))
			       :limit-from '(:backpack :floor) ;; only place with food
			       :which-use :eat
			       :sound "eat-something"
			       :prompt "Eat what")))

(define-key-operation 'refill-item
    #'(lambda (dungeon player)
	(interactive-refill-item! dungeon player)))


(define-key-operation 'invoke-spell
    #'(lambda (dungeon player)
	(van-invoke-spell! dungeon player)))


(define-key-operation 'open-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :open)))

(define-key-operation 'close-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :close)))

(define-key-operation 'bash-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :bash)))

(define-key-operation 'jam-door
    #'(lambda (dungeon player)
	(interactive-door-operation! dungeon player :jam)))

(define-key-operation 'search-area
    #'(lambda (dungeon player) (search-area! dungeon player)))

(define-key-operation 'disarm-trap
    #'(lambda (dungeon player)
	(interactive-trap-operation! dungeon player :disarm)))

(define-key-operation 'select-target
    #'(lambda (dungeon player)
	(interactive-targeting! dungeon player)))


;; unused
(define-key-operation 'print-mapper
    #'(lambda (dungeon player)
	(print-map dungeon player *map-frame*)))



(define-key-operation 'save-game
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(when-bind (func (get-late-bind-function 'langband 'save-the-game))
	  (let ((save-path (variant-save-directory *variant*)))
	    (lbsys/make-sure-dirs-exist& save-path)
	    #-langband-release
	    (funcall func *variant* player *level* :format :readable)
	    (funcall func *variant* player *level* :format :binary))
	  (print-message! "Your game was saved [binary+source]")
	  )))

(define-key-operation 'save-and-exit
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(when-bind (func (get-late-bind-function 'langband 'save-the-game))
	  ;; actual save is done in death.lisp
	  (setf (creature-alive? player) t
		(player.dead-from player) "quitting"
		(player.leaving? player) :quit)	  
	  (print-message! "Your game was saved [binary+source].  Press Enter/Return to continue.")
	  (read-one-character)
	  )))



(define-key-operation 'show-help
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-dialogue ()
	  (clear-window *cur-win*)
	  (display-help-topics *variant* "LAangband help (Vanilla)" 3)

	  )))


(define-key-operation 'identify-symbol
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(with-dialogue ()
	  (clear-window *cur-win*)
	  (interactive-identify-symbol *variant* player)

	  )))


(define-key-operation 'learn-spell
    #'(lambda (dungeon player)
	(van-learn-spell! dungeon player)))

(define-key-operation 'browse-spells
    #'(lambda (dungeon player)
	(browse-spells dungeon player)))


(define-key-operation 'fire-missile
    #'(lambda (dungeon player)
	(interactive-fire-a-missile dungeon player)))

(define-key-operation 'previous-messages
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-dialogue ()
	  (show-messages :offset 0))))


(define-key-operation 'print-attack-table
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
;;	(with-new-screen ()
	  (print-attack-table *variant* player)
	  (print-attack-graph *variant* player)
	))

(define-key-operation 'print-resists
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let ((var *variant*))
	  (print-resistance-table var player (get-setting var :resists-display)))
	))

(define-key-operation 'print-misc
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(print-misc-info *variant* player (get-setting *variant* :misc-display))
	))

(define-key-operation 'redraw-all
    #'(lambda (dungeon player)

	;; skip flushes
	;; do xtra react
	;;(org.langband.ffi:c-term-xtra& 10 0) ;; xtra_react
	;; skip combine & reorder

	(ask-for-update! player '[torch])
	(ask-for-update! player '[forget-view])
	(ask-for-update! player '[update-view])
	(ask-for-update! player '[monsters])
	
	(ask-for-redraw! player '[extra])
	(ask-for-redraw! player '[basic])
	(ask-for-redraw! player '[map])
	;; skip equippy
	
	;; fix clear in the right areas!
	;; do clear
	;;(c-term-clear!)
	 
	;; call handle-stuff
	(handle-stuff *variant* dungeon player)
	
	;; do all windows, we might need this
	(loop for x across *windows*
	      do
	      (when (window.visible? x)
		(refresh-window x)))
	
	))

(define-key-operation 'swap-map
    #'(lambda (dungeon player)
	(when (eq (get-system-type) 'sdl)
	  (switch-map-mode *variant* dungeon player))))

(define-key-operation 'play-music
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(warn "play music again..")
	(play-music 1 0)))
