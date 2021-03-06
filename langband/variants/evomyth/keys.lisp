;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/keys.lisp - key-definitions - keyassignments in config
Copyright (c) 2000-2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-key-operation 'toggle-run-mode
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(let ((run-status (get-information "running" :default nil)))
	  ;;(warn "toggle to ~s" run-status)
	  (setf (get-information "running") (not run-status)
		(get-information "run-direction") -1
		))))


(defun evo/player-run! (dungeon player direction)
  (let ((next (run-along-corridor dungeon player direction)))
;;    (warn "Tried to run ~s, got next ~s" direction next)
    (when (plusp next)
      (move-player! dungeon player next)
      (setf (get-information "run-direction") next)
      t)))

(defun evo/move-player! (dungeon player direction)
;;  (warn "move -> ~s" direction)
  (cond ((get-information "running" :default nil)
	 (unless (evo/player-run! dungeon player direction)
	   (setf (get-information "run-direction") -1
		 (get-information "running") nil)))
	(t
	 (move-player! dungeon player direction))))

(defun evo/run-direction (dungeon player dir)
  (setf (get-information "running") t)
  (evo/move-player! dungeon player dir))



(define-key-operation 'move-up
    #'(lambda (dungeon player) (evo/move-player! dungeon player 8)))

(define-key-operation 'move-up-left
    #'(lambda (dungeon player) (evo/move-player! dungeon player 7)))

(define-key-operation 'move-up-right
    #'(lambda (dungeon player) (evo/move-player! dungeon player 9)))

(define-key-operation 'move-right
    #'(lambda (dungeon player) (evo/move-player! dungeon player 6)))

(define-key-operation 'move-left
    #'(lambda (dungeon player) (evo/move-player! dungeon player 4)))

(define-key-operation 'move-down
    #'(lambda (dungeon player) (evo/move-player! dungeon player 2)))

(define-key-operation 'move-down-left
    #'(lambda (dungeon player) (evo/move-player! dungeon player 1)))

(define-key-operation 'move-down-right
    #'(lambda (dungeon player) (evo/move-player! dungeon player 3)))

(define-key-operation 'run-up
    #'(lambda (dungeon player) (evo/run-direction dungeon player 8)))

(define-key-operation 'run-up-left
    #'(lambda (dungeon player) (evo/run-direction dungeon player 7)))

(define-key-operation 'run-up-right
    #'(lambda (dungeon player) (evo/run-direction dungeon player 9)))

(define-key-operation 'run-right
    #'(lambda (dungeon player) (evo/run-direction dungeon player 6)))

(define-key-operation 'run-left
    #'(lambda (dungeon player) (evo/run-direction dungeon player 4)))

(define-key-operation 'run-down
    #'(lambda (dungeon player) (evo/run-direction dungeon player 2)))

(define-key-operation 'run-down-left
    #'(lambda (dungeon player) (evo/run-direction dungeon player 1)))

(define-key-operation 'run-down-right
    #'(lambda (dungeon player) (evo/run-direction dungeon player 3)))

(define-key-operation 'stand-still
    #'(lambda (dungeon player) (evo/move-player! dungeon player 5)))

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
	  (texture-background! +full-frame+ "textures/plainbook.png" -1)
	  (clear-window *cur-win*)
	  (block display-input 
	    (let ((loc-table (gethash :display *current-key-table*)))
	      ;;(warn "~s ~s" loc-table *current-key-table*)
	      (loop
	       (clear-window +full-frame+)
	       (display-creature *variant* player)
	       ;;(put-coloured-line! +term-white+ "['C' to show combat-info, 'R' to show resists,  ESC to continue]"
	       ;;5 (get-last-console-line))
	       (print-note! "['Q' to show quest-list, 'R' to show resists,  ESC to continue]")
	       
	       (let* ((ch (read-one-character))
		      (fun (check-keypress loc-table ch)))
		 (cond ((and fun (functionp fun))
			(funcall fun dungeon player))
		       ((eql ch +escape+)
			(return-from display-input t))
		       (t
			;; nil
			)))
	       )))
	  (texture-background! +full-frame+ "" -1)
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

(define-key-operation 'destroy-item
    #'(lambda (dungeon player)
	(interactive-destroy-item! dungeon player)))

(define-key-operation 'take-off-item
    #'(lambda (dungeon player)
	(interactive-take-off-item! dungeon player)))

(define-key-operation 'wear-item
    #'(lambda (dungeon player)
	(interactive-wear-item! dungeon player)))

(define-key-operation 'use-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player)))

(define-key-operation 'quaff-potion
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:quaff)
			       :which-use :quaff
			       :limit-from '(:backpack :floor) ;; only place with potions
			       :prompt "Quaff which potion?")))

(define-key-operation 'zap-item
    #'(lambda (dungeon player)
	(interactive-use-item! dungeon player :need-effect '(:zap)
			       :which-use :zap
			       :limit-from '(:backpack :floor) ;; only place with zappers I think
			       :prompt "Zap which stick?")))
	


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
			       :prompt "Eat what?")))
#||
(define-key-operation 'invoke-spell
    #'(lambda (dungeon player)
	(van-invoke-spell! dungeon player)))
||#

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

#||
;; unused
(define-key-operation 'print-mapper
    #'(lambda (dungeon player)
	(print-map dungeon player)))



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
	  (setf (player.dead? player) nil
		(player.dead-from player) "quitting"
		(player.leaving? player) :quit)	  
	  (print-message! "Your game was saved [binary+source].  Press Enter/Return to continue.")
	  (read-one-character)
	  )))




(define-key-operation 'learn-spell
    #'(lambda (dungeon player)
	(van-learn-spell! dungeon player)))

(define-key-operation 'browse-spells
    #'(lambda (dungeon player)
	(browse-spells dungeon player)))
||#


(define-key-operation 'show-help
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-dialogue ()
	  (clear-window *cur-win*)
	  (display-help-topics *variant* "LAangband help (Evomyth)" 3)

	  )))

#||
(define-key-operation 'fire-missile
    #'(lambda (dungeon player)
	(interactive-fire-a-missile dungeon player)))
||#

(define-key-operation 'previous-messages
    #'(lambda (dungeon player)
	(declare (ignore dungeon player))
	(with-dialogue ()
	  (show-messages :offset 0))))

(define-key-operation 'show-quests
    #'(lambda (dungeon player)
	(declare (ignore dungeon))
	(let ((var *variant*))
	  (quest:print-quests var player (get-setting var :quests-display)))
	))


#||
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
||#

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
	(play-music 1 1)))

;; update this later
(defun %will-talk (player other)
  (declare (ignore player))
  (and (typep other 'npc)
       (attitude>= other +attitude-hated+) ;; no talk, just attack!
       ))

(defun %can-talk (player other)
  (declare (ignore player))
  (typep other 'npc))



(define-key-operation 'start-conversation
    #'(lambda (dungeon player)
	(lb-dialogue:interactive-start-conversation dungeon player
						    :will-talk-test #'%will-talk
						    :can-talk-test #'%can-talk)))
