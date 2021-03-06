;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/traps.lisp - trap-types for vanilla variant
Copyright (c) 2002-2003 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

;; traps should be better at finding out who the victim is.. right now the player is hurt automagically

(define-trap-type "trapdoor" "trap-door"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You fall through a trap door!");
	    (play-sound "trapdoor")
	    (let ((variant *variant*)
		  (player *player*) ;; fix later
		  (level-num (randint 3)))
	      
	      (cond ((eq t (get-creature-state player '<feather-fall>))
		     (setf level-num 1)
		     (print-message! "You float gently down to the next level."))
		    (t
		     (deliver-damage! variant the-trap player (roll-dice 2 8))))

	      ;; increase depth and set leave to true
	      (move-creature-to-depth! dungeon player :direction :down :amount level-num :type :trapdoor)
	      ))
  :min-depth 1
  :max-depth nil
  :text-sym (text-paint-value +term-white+ #\^)
  :gfx-sym (tile-paint-value 10 38)
  :rarity 1)

(define-trap-type "pit" "pit"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You fall into a pit!")
	    
	    (let ((variant *variant*)
		  (player *player*))
	      (cond ((eq t (get-creature-state player '<feather-fall>))
		     (print-message! "You float gently down to the bottom of the pit."))
		    (t
		       (deliver-damage! variant the-trap player (roll-dice 2 6))))
	      )) 
  :text-sym (text-paint-value +term-slate+ #\^)
  :gfx-sym (tile-paint-value 10 36)
  )


(define-trap-type "spiked-pit" "spiked pit"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You fall into a spiked pit!")
	    (let ((variant *variant*)
		  (player *player*))
	      (cond ((eq t (get-creature-state player '<feather-fall>))
		     (print-message! "You float gently to the floor of the pit.")
		     (print-message! "You carefully avoid touching the spikes."))
		    (t
		     (let ((dmg (roll-dice 2 6)))
		       (when (< (randint 100) 50)
			 (print-message! "You are impaled!")
			 (incf dmg dmg) ;; * 2
			 ;; add cut (randint dmg)
			 )
		       (deliver-damage! variant the-trap player dmg))
		     ))
	      )) 
  :text-sym (text-paint-value +term-slate+ #\^)
  :gfx-sym (tile-paint-value 10 37)
  )


(define-trap-type "spiked-pit-poison" "spiked pit"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You fall into a spiked pit!")
	    (let ((variant *variant*)
		  (player *player*))
	      (cond ((eq t (get-creature-state player '<feather-fall>))
		     (print-message! "You float gently to the floor of the pit.")
		     (print-message! "You carefully avoid touching the spikes."))
		    (t
		       (let ((dmg (roll-dice 2 6)))
			 (when (< (randint 100) 50)
			   (print-message! "You are impaled on poisonous spikes!")
			   (incf dmg dmg) ;; * 2
			   ;; add cut (randint dmg)
			   (cond ((resists-element? player '<poison>)
				  (print-message! "The poison does not affect you!"))
				 (t
				  (incf dmg dmg) ;; * 2
				  (modify-creature-state! player '<poisoned> :add (randint dmg))
				  )))
			 (deliver-damage! variant the-trap player dmg))
		       ))
		))
  :text-sym (text-paint-value +term-slate+ #\^)
  :gfx-sym (tile-paint-value 10 67)
  )


(define-trap-type "summon-trap" "summoning trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are enveloped in a cloud of smoke!")
	      ;;; ...
	    )
  :text-sym (text-paint-value +term-orange+ #\^)
  :gfx-sym (tile-paint-value 10 39)
  )

(define-trap-type "teleport-trap" "teleport trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You hit a teleport trap!")
	    (let ((player *player*))
	      (teleport-creature! dungeon player player 100)))
  :text-sym (text-paint-value +term-orange+ #\^)
  :gfx-sym (tile-paint-value 10 40)
  )

(define-trap-type "fire-trap" "fire trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are enveloped in flames!")
	    (play-sound "burning-fire")
	    (deliver-elemental-damage! *variant* the-trap *player* '<fire> (roll-dice 4 6)))
  :text-sym (text-paint-value +term-umber+ #\^)
  :gfx-sym (tile-paint-value 10 35)
  )


(define-trap-type "acid-trap" "acid trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are splashed with acid!")
	    (play-sound "acid-splash")
	    (deliver-elemental-damage! *variant* the-trap *player* '<acid> (roll-dice 4 6)))
  :text-sym (text-paint-value +term-umber+ #\^)
  :gfx-sym (tile-paint-value 10 35)
  )


(define-trap-type "dart-trap-slow" "dart trap (slow)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (modify-creature-state! target '<slowed> :add (+ 20 (randint 20)))
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
		))
  
  :text-sym (text-paint-value +term-red+ #\^)
  :gfx-sym (tile-paint-value 10 33)
  )

(define-trap-type "dart-trap-red-str" "dart trap (red. str)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (play-sound "dart-trap")
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (update-player-stat! target '<str> '<reduce>)
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
	      ))
  :text-sym (text-paint-value +term-red+ #\^)
  :gfx-sym (tile-paint-value 10 57)
  )


(define-trap-type "dart-trap-red-dex" "dart trap (red. dex)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (play-sound "dart-trap")
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (update-player-stat! target '<dex> '<reduce>)
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
	      ))
  :text-sym (text-paint-value +term-red+ #\^)
  :gfx-sym (tile-paint-value 10 58)
  )


(define-trap-type "dart-trap-red-con" "dart trap (red. con)"
  :effect (trap-effect (the-trap dungeon x y)
	    (let ((target *player*))
	      (play-sound "dart-trap")
	      (cond ((melee-hit-ac? target 125 (get-creature-ac target) t)
		     (print-message! "A small dart hits you!")
		     (deliver-damage! *variant* the-trap target (roll-dice 1 4))
		     (update-player-stat! target '<con> '<reduce>)
		     )
		    (t
		     (print-message! "A small dart barely misses you.")))
	      ))
  :text-sym (text-paint-value +term-red+ #\^)
  :gfx-sym (tile-paint-value 10 60)
  )


(define-trap-type "blind-trap" "blindness trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a black gas!")
	    (unless (resists-element? *player* '<blindness>)
	      (modify-creature-state! *player* '<blindness> :add (+ 25 (randint 50)))
	      ))
  :text-sym (text-paint-value +term-green+ #\^)
  :gfx-sym (tile-paint-value 10 61)
  )



(define-trap-type "confusion-trap" "confusion trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a gas of scintillating colors!")
	    (unless (resists-element? *player* '<confusion>)
	      (modify-creature-state! *player* '<confusion> :add (+ 10 (random 20)))
	      ))
  :text-sym (text-paint-value +term-green+ #\^)
  :gfx-sym (tile-paint-value 10 59)
  )


(define-trap-type "poison-trap" "poison-gas trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a pungent green gas!")
	    (unless (resists-element? *player* '<poison>)
	      (modify-creature-state! *player* '<poisoned> :add (+ 20 (randint 20)))

	      ))
  :text-sym (text-paint-value +term-green+ #\^)
  :gfx-sym (tile-paint-value 10 56)
  )


(define-trap-type "paralysis-trap" "paralysis trap"
  :effect (trap-effect (the-trap dungeon x y)
	    (print-message! "You are surrounded by a strange white mist!")
	    (unless (eq t (get-creature-state *player* '<free-action>))
	      (modify-creature-state! *player* '<paralysed> :add (+ 5 (randint 10)))
	      ))
  :text-sym (text-paint-value +term-green+ #\^)
  :gfx-sym (tile-paint-value 10 61)
  )
