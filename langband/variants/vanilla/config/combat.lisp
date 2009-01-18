;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/combat.lisp - combat factors
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(define-attack-description '<hit> "hits you")
(define-attack-description '<beg> "begs you for money")
(define-attack-description '<touch> "touches you")
(define-attack-description '<claw> "claws you")
(define-attack-description '<bite> "bites you")
(define-attack-description '<wail> "wails at you")
(define-attack-description '<gaze> "gazes at you")
(define-attack-description '<butt> "butts you")
(define-attack-description '<kick> "kicks you")
(define-attack-description '<spore> "releases spores at you")
(define-attack-description '<engulf> "engulfs you")
(define-attack-description '<insult> "insults you") ;; randomise
(define-attack-description '<moan> "moans at you") ;; randomise
(define-attack-description '<spit> "spits on you")
(define-attack-description '<crush> "crushes you")
(define-attack-description '<crawl> "crawls on you")
(define-attack-description '<sting> "stings you")
(define-attack-description '<drool> "drools on you")

;; move this
(defun %get-power-level (attacker)
  (min-cap (get-power-lvl attacker) 1))

(define-monster-attack '<hurt>
    :power 60
    :hit-effect (attack-effect (attacker target the-attack damage)
		    ;; alter for ac
		    (deduct-hp! target damage)
		    damage))

(define-monster-attack '<shatter>
    :power 60
    :hit-effect (attack-effect (attacker target attack dmg)
		  ;; alter for ac
		  (deduct-hp! target dmg)
		  ;; earthquake?
		  dmg))

(define-monster-attack '<paralyse>
    :power 2
    :hit-effect (attack-effect (attacker target attack dmg)
		  ;; add hack for always dmg
		  (deduct-hp! target dmg)
		  (cond ((eq t (get-creature-state *player* '<free-action>))
			 (print-message! "You are unaffected!"))
			;; dummy save
			((< (random 100) 50)
			 (print-message! "You resist the effects!"))
			(t
			 (modify-creature-state! target '<paralysed>
						 :add (+ 3 (randint (%get-power-level attacker))))))

		    ;; add paralyse knowledge to attacker
		    dmg))

(define-monster-attack '<terrify>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (cond ((or (resists-element? target '<fear>)
			     ;; dummy save
			     (< (random 100) 50))
			 (print-message! "You stand your ground!"))
			(t
			 (modify-creature-state! target '<fear>
						 :add (+ 3 (randint (%get-power-level attacker))))))

		    ;; add fear knowledge to attacker
		    dmg))

(define-monster-attack '<blind>
    :power 2
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (unless (resists-element? target '<blindness>)
		    (modify-creature-state! target '<blindness>
					    :add (+ 10 (randint (%get-power-level attacker)))))
		  ;; add blind knowledge to attacker
		  dmg))


(define-monster-attack '<poison>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (unless (resists-element? target '<poison>)
		    (modify-creature-state! target '<poisoned>
					    :add (+ 5 (randint (%get-power-level attacker)))))
		  ;; add poison knowledge to attacker
		  dmg))

(define-monster-attack '<confusion>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  (unless (resists-element? target '<confusion>)
		    (modify-creature-state! target '<confusion>
					    :add (+ 3 (randint (%get-power-level attacker)))))
		  ;; add poison knowledge to attacker
		  dmg))

(define-monster-attack '<exp-10>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))

(define-monster-attack '<exp-20>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))


(define-monster-attack '<exp-40>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))

(define-monster-attack '<exp-80>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  ;; check for hold-life
		  (modify-xp! target -400) ;; hack
		  dmg))

(define-monster-attack '<eat-gold>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; expand to monsters too later
		  (when (typep target 'player)
		    ;; saving throw
		    (cond ((< (random 100) 50) ;; base on dex and lvl later
			   (print-message! "You quickly protect your money pouch!")
			   ;; possible blink
			   )
			  (t
			   ;; fix this to also apply to monsters
			   (let* ((cur-gold (player.gold target))
				  (amount (+ 25 (int-/ cur-gold 10))))
			     (when (< amount 2)
			       (setf amount 2))
			     ;; skip 5000 limit
			     (when (> amount cur-gold)
			       (setf amount cur-gold))

			     (decf (player.gold target) amount)

			     (cond ((<= amount 0)
				    (print-message! "Nothing was stolen!"))
				   ((plusp (player.gold target))
				    (print-message! "Your purse feels lighter.")
				    (format-message! "~d coins were stolen!" amount))
				   (t
				    (print-message! "Your purse feels lighter.")
				    (print-message! "All your coins were stolen.")))

			     ;; repaint
			     (ask-for-redraw! target '[gold])
			     ;; add blink-away
			     ))))

		  dmg))

(define-monster-attack '<eat-light>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<eat-item>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<eat-food>
    :power 5
    :hit-effect (attack-effect (attacker target attack dmg)
		  (when (plusp dmg)
		    (deduct-hp! target dmg))
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<lose-str>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<str> '<reduce>))
		  damage))

(define-monster-attack '<lose-dex>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<dex> '<reduce>))
		  damage))

(define-monster-attack '<lose-con>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<con> '<reduce>))
		  damage))

(define-monster-attack '<lose-int>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<int> '<reduce>))
		  damage))

(define-monster-attack '<lose-wis>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<wis> '<reduce>))
		  damage))

(define-monster-attack '<lose-chr>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<chr> '<reduce>))
		  damage))

(define-monster-attack '<lose-all>
    :power 0
    :hit-effect (attack-effect (attacker target the-attack damage)
		  (when damage
		    (deduct-hp! target damage))
		  (when (typep target 'player)
		    (update-player-stat! target '<str> '<reduce>)
		    (update-player-stat! target '<dex> '<reduce>)
		    (update-player-stat! target '<con> '<reduce>)
		    (update-player-stat! target '<int> '<reduce>)
		    (update-player-stat! target '<wis> '<reduce>)
		    (update-player-stat! target '<chr> '<reduce>)
		    )
		  damage))

(define-monster-attack '<acid>
    :power 0
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are covered in acid!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<acid> dmg))
		  ;; add acid knowledge to attacker
		  dmg))

(define-monster-attack '<cold>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are covered with frost!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<cold> dmg))
		  ;; add cold knowledge to attacker
		  dmg))

(define-monster-attack '<fire>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are enveloped in flames!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<fire> dmg))
		  ;; add fire knowledge to attacker
		  dmg))

(define-monster-attack '<electricity>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are struck by electricity!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<electricity> dmg))
		  ;; add elec knowledge to attacker
		  dmg))

(define-monster-attack '<water>
    :power 10
    :hit-effect (attack-effect (attacker target attack dmg)
		  (print-message! "You are splashed by water!")
		  (when (plusp dmg)
		    (deliver-elemental-damage! *variant* attacker target '<water> dmg))
		  ;; add water knowledge to attacker
		  dmg))



(define-monster-attack '<un-power>
    :power 15
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  dmg))

(define-monster-attack '<un-bonus>
    :power 20
    :hit-effect (attack-effect (attacker target attack dmg)
		  (deduct-hp! target dmg)
		  ;; NOT IMPLEMENTED
		  dmg))

;;; Monster special abilities

(define-monster-spab '<breath> '<time>
  :breath-type '<time>
  :desc "time"
  :damage #'(lambda (chp)
	      (max-cap 150 (int-/ chp 3))))

(define-monster-spab '<breath> '<fire>
  :breath-type '<fire>
  :desc "fire"
  :visual "fire"
  :damage #'(lambda (chp)
	      (max-cap 1600 (int-/ chp 3))))

(define-monster-spab '<breath> '<cold>
  :breath-type '<cold>
  :desc "frost"
  :visual "cold"
  :damage #'(lambda (chp)
	      (max-cap 1600 (int-/ chp 3))))

(define-monster-spab '<breath> '<electricity>
  :breath-type '<electricity>
  :desc "lightning"
  :visual "electricity"
  :damage #'(lambda (chp)
	      (max-cap 1600 (int-/ chp 3))))

(define-monster-spab '<breath> '<acid>
  :breath-type '<acid>
  :desc "acid"
  :visual "acid"
  :damage #'(lambda (chp)
	      (max-cap 1600 (int-/ chp 3))))
    
(define-monster-spab '<breath> '<poison>
  :breath-type '<poison>
  :desc "gas"
  :visual "poison"
  :damage #'(lambda (chp)
	      (max-cap 1600 (int-/ chp 3))))

(define-monster-spab '<breath> '<plasma>
  :breath-type '<plasma>
  :desc "plasma"
  :visual "fire"
  :damage #'(lambda (chp)
	      (max-cap 150 (int-/ chp 6))))
    
(define-monster-spab '<breath> '<light>
  :breath-type '<light>
  :desc "light"
  :visual "light"
  :damage #'(lambda (chp)
	      (max-cap 400 (int-/ chp 6))))
    
(define-monster-spab '<breath> '<darkness>
  :breath-type '<darkness>
  :desc "darkness"
  :visual "darkness"
  :damage #'(lambda (chp)
	      (max-cap 400 (int-/ chp 6))))

    
(define-monster-spab '<breath> '<nether>
  :breath-type '<nether>
  :desc "nether"
  :visual "darkness"
  :damage #'(lambda (chp)
	      (max-cap 550 (int-/ chp 6))))

    
(define-monster-spab '<breath> '<nexus>
  :breath-type '<nexus>
  :desc "nexus"
  :damage #'(lambda (chp)
	      (max-cap 400 (int-/ chp 6))))

(define-monster-spab '<breath> '<shards>
  :breath-type '<shards>
  :desc "shards"
  :damage #'(lambda (chp)
	      (max-cap 500 (int-/ chp 6))))

(define-monster-spab '<breath> '<inertia>
  :breath-type '<inertia>
  :desc "inertia"
  :damage #'(lambda (chp)
	      (max-cap 200 (int-/ chp 6))))

(define-monster-spab '<breath> '<force>
  :breath-type '<force>
  :desc "force"
  :damage #'(lambda (chp)
	      (max-cap 200 (int-/ chp 6))))

(define-monster-spab '<breath> '<chaos>
  :breath-type '<chaos>
  :desc "chaos"
  :damage #'(lambda (chp)
	      (max-cap 500 (int-/ chp 6))))


(define-monster-spab '<breath> '<water>
  :breath-type '<water>
  :desc "water"
  :damage #'(lambda (chp)
	      (max-cap 250 (int-/ chp 3))))


(define-monster-spab '<breath> '<gravity>
  :breath-type '<gravity>
  :desc "gravity"
  :damage #'(lambda (chp)
	      (max-cap 200 (int-/ chp 3))))


(define-monster-spab '<breath> '<sound>
  :breath-type '<sound>
  :desc "sound"
  :visual "electricity"
  :damage #'(lambda (chp)
	      (max-cap 500 (int-/ chp 6))))
    
(define-monster-spab '<breath> '<disenchant>
  :breath-type '<disenchant>
  :desc "disenchantment"
  :damage #'(lambda (chp)
	      (max-cap 150 (int-/ chp 3))))

(define-monster-spab '<breath> '<confusion>
  :breath-type '<confusion>
  :desc "confusion"
  :damage #'(lambda (chp)
	      (max-cap 400 (int-/ chp 6))))


(define-monster-spab '<arrow> 1
  :power 1
  :range 6
  :desc "an arrow"
  :missile "arrow")

(define-monster-spab '<arrow> 2
  :power 3
  :range 8
  :desc "a bolt"
  :missile "bolt")

(define-monster-spab '<arrow> 3
  :power 5
  :range 10
  :desc "a missile"
  :missile "arrow")

(define-monster-spab '<arrow> 4
  :power 7
  :range 12
  :desc "a missile"
  :missile "arrow")

(define-monster-spab '<summon> '<angel>)
(define-monster-spab '<summon> '<demon>)
(define-monster-spab '<summon> '<wraith>)
(define-monster-spab '<summon> '<dragon>)
(define-monster-spab '<summon> '<kin>)
(define-monster-spab '<summon> '<ant>)
(define-monster-spab '<summon> '<hound>)
(define-monster-spab '<summon> '<unique>)
(define-monster-spab '<summon> '<monsters>)
(define-monster-spab '<summon> '<monster>)
(define-monster-spab '<summon> '<undead>)
(define-monster-spab '<summon> '<hydra>)
(define-monster-spab '<summon> '<spider>)
(define-monster-spab '<summon> '<high-undead>)
(define-monster-spab '<summon> '<high-dragon>)
(define-monster-spab '<summon> '<high-demon>)

(define-monster-spab '<spell> '<confusion>
  :effect (spab-effect (creature ability target dungeon)
	    (unless (amon.seen-by-player? creature)
	      (return-from spab-effect nil)) ;; need los
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (if (is-blind? *variant* target)
		(format-message! "~@(~A~) mumbles, you hear puzzling noises."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) creates mesmerising illusions."
				 (get-creature-desc creature #x00)))
		  
	    (cond ((or (resists-element? target '<confusion>)
		       (roll-saving-throw target (get-power-lvl creature)))
		   (print-message! "You disbelieve the feeble spell."))
		  (t
		   (modify-creature-state! target '<confusion> :add (+ 4 (randint 4)))))
	    t))

(define-monster-spab '<spell> '<blindness>
  :effect (spab-effect (creature ability target dungeon)
	    (unless (amon.seen-by-player? creature)
	      (return-from spab-effect nil)) ;; need los
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (if (is-blind? *variant* target)
		(format-message! "~@(~A~) mumbles."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) casts a spell, burning your eyes."
				 (get-creature-desc creature #x00)))
		  
	    (cond ((resists-element? target '<blindness>)
		   (print-message! "You are unaffected."))
		  ((roll-saving-throw target (get-power-lvl creature))
		   (print-message! "You resist the effects!"))
		  (t
		   (modify-creature-state! target '<blindness> :add (+ 4 (randint 4)))))
	    t))

(define-monster-spab '<spell> '<paralysis>
  :effect (spab-effect (creature ability target dungeon)
	    (unless (amon.seen-by-player? creature)
	      (return-from spab-effect nil)) ;; need los
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (if (is-blind? *variant* target)
		(format-message! "~@(~A~) mumbles."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) stares deep into your eyes."
				 (get-creature-desc creature #x00)))
		  
	    (cond ((eq t (get-creature-state target '<free-action>))
		   (print-message! "You are unaffected."))
		  ((roll-saving-throw target (get-power-lvl creature))
		   (print-message! "You resist the effects!"))
		  (t
		   (modify-creature-state! target '<paralysed> :add (+ 4 (randint 4)))))
	    t))

(define-monster-spab '<spell> '<scare>
  :effect (spab-effect (creature ability target dungeon)
	    (unless (amon.seen-by-player? creature)
	      (return-from spab-effect nil)) ;; need los
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (if (is-blind? *variant* target)
		(format-message! "~@(~A~) mumbles, and you hear scary noises."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) casts a fearful illusion."
				 (get-creature-desc creature #x00)))
		  
	    (cond ((or (resists-element? target '<fear>)
		       (roll-saving-throw target (get-power-lvl creature)))
		   (print-message! "You refuse to be frightened."))
		  (t
		   (modify-creature-state! target '<fear> :add (+ 4 (randint 4)))))
	    t))

(define-monster-spab '<spell> '<haste>
  :effect (spab-effect (creature ability target dungeon)
	    (disturbance *variant* *player* creature :major)
	    (setf target creature) ;; hack, fix 
	    (if (is-blind? *variant* *player*)
		(format-message! "~@(~A~) mumbles."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) concentrates on ~A body!"
				 (get-creature-desc creature #x00)
				 (get-creature-desc creature #x22)))
		     
	    (format-message! "~@(~A~) starts moving faster."
			     (get-creature-desc target #x00))
	    (haste-creature! target +10 20)
	    t))

(define-monster-spab '<spell> '<slow>
  :effect (spab-effect (creature ability target dungeon)
	    (unless (amon.seen-by-player? creature)
	      (return-from spab-effect nil)) ;; need los
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (format-message! "~@(~A~) drains power from your muscles."
			     (get-creature-desc creature #x00))

	    (cond ((eq t (get-creature-state target '<free-action>))
		   (print-message! "You are unaffected."))
		  ((roll-saving-throw target (get-power-lvl creature))
		   (print-message! "You resist the effects!"))
		  (t
		   (modify-creature-state! target '<slowed> :add (+ 4 (randint 4)))))
		  
	    t))

(define-monster-spab '<spell> '<blink>
  :effect (spab-effect (creature ability target dungeon)
	    (disturbance *variant* *player* creature :major)
	    (format-message! "~@(~A~) blinks away."
			     (get-creature-desc creature #x00))
	    (teleport-creature! dungeon *player* creature 10)
	    t))

(define-monster-spab '<spell> '<teleport>
  :effect (spab-effect (creature ability target dungeon)
	    (disturbance *variant* *player* creature :major)
	    (format-message! "~@(~A~) teleports away."
			     (get-creature-desc creature #x00))
	    (teleport-creature! dungeon *player* creature (+ 5 (* +max-sight+ 2)))
	    t))

(define-monster-spab '<spell> '<teleport-away>
  :effect (spab-effect (creature ability target dungeon)
	    (disturbance *variant* *player* creature :major)
	    (format-message! "~@(~A~) teleports ~A away."
			     (get-creature-desc creature #x00)
			     (get-creature-desc target #x20))
	    (teleport-creature! dungeon *player* target 100)
	    t))

(define-monster-spab '<spell> '<teleport-level>)
(define-monster-spab '<spell> '<teleport-player>)

(define-monster-spab '<spell> '<missile>
  :visual "magic-missile"
  :effect (spab-effect (creature ability target dungeon)
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (if (is-blind? *variant* target)
		(format-message! "~@(~A~) mumbles."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) casts a magic missile."
				 (get-creature-desc creature #x00)
				 ;;(get-creature-desc target #x20)
				 ))
	    (van-fire-bolt! creature target (get-spell-effect '<magic-missile>)
			    (+ (roll-dice 2 6) (int-/ (get-power-lvl creature) 3))
			    :projected-object ability)
	    t)
  :offensive #'(lambda (creature)
		 (+ (roll-dice 2 6) (int-/ (get-power-lvl creature) 3)))
  :mana-cost 2)


(define-monster-spab '<spell> '<darkness>
  :visual "darkness"
  :effect (spab-effect (creature ability target dungeon)
	    (unless (amon.seen-by-player? creature)
	      (return-from spab-effect nil)) ;; need los
	    (disturbance *variant* *player* creature :major)
	    ;; check blindness
	    (if (is-blind? *variant* *player*)
		(format-message! "~@(~A~) mumbles."
				 (get-creature-desc creature #x00))
		(format-message! "~@(~A~) conjures up shadows."
				 (get-creature-desc creature #x00)))
	    (light-area! dungeon creature (location-x target)
			 (location-y target) 0 3 :type '<darkness>
			 :projected-object ability))
  :offensive #'(lambda (creature)
		 (block offensive-check
		   ;;(warn "Checking offensiveness")
		   (let ((target *player*))
		     ;; absolute demand that it's not glowing where the player is
		     (unless (bit-flag-set? (cave-flags *dungeon* (location-x target) (location-y target))
					    +cave-glow+)
		       ;;(warn "no glow")
		       (return-from offensive-check 0))

		     ;; will monster use this inside light-radius
		     (when (and (<= (distance (location-x creature) (location-y creature)
					     (location-y target) (location-y target))
				    (get-light-radius target))
				(or (is-smart? creature) ;; smart creature will not
				    (< (random 100) 50))) ;; 50% chance for normal monsters
		       ;;(warn "inside light radius")
		       (return-from offensive-check 0))

		     ;; ok, then we give it a rating of 20
		     20)))

  :mana-cost 4)

	    


(define-monster-spab '<spell> '<brain-smash>)
(define-monster-spab '<spell> '<mind-blast>)
(define-monster-spab '<spell> '<drain-mana>)
(define-monster-spab '<spell> '<forget>)
(define-monster-spab '<spell> '<heal>)
(define-monster-spab '<spell> '<traps>)
(define-monster-spab '<spell> '<shriek>)

(define-monster-spab '<bolt-spell> '<fire>
  :desc "fire bolt"
  :visual "fire"
  :damage #'(lambda (lvl)
	      (+ (roll-dice 9 8) (int-/ lvl 3))))

(define-monster-spab '<bolt-spell> '<cold>
  :desc "frost bolt"
  :visual "cold"
  :damage #'(lambda (lvl)
	      (+ (roll-dice 6 8) (int-/ lvl 3))))

(define-monster-spab '<bolt-spell> '<acid>
  :desc "acid bolt"
  :visual "acid" 
  :damage #'(lambda (lvl)
	      (+ (roll-dice 7 8) (int-/ lvl 3))))

(define-monster-spab '<bolt-spell> '<electricity>
  :desc "lightning bolt"
  :visual "electricity"
  :damage #'(lambda (lvl)
	      (+ (roll-dice 4 8) (int-/ lvl 3))))

(define-monster-spab '<bolt-spell> '<poison>
  :desc "poison bolt"
  :visual "poison"
  :damage #'(lambda (lvl)
	      (+ (roll-dice 9 8) (int-/ lvl 3))))

(define-monster-spab '<bolt-spell> '<mana>
  :desc "mana bolt"
  :visual "magic-missile"
  :damage #'(lambda (lvl)
	      (+ 50 (randint (int-/ (* lvl 7) 2)))))

(define-monster-spab '<bolt-spell> '<plasma>
  :desc "plasma bolt"
  :visual "fire"
  :damage #'(lambda (lvl)
	      (+ 10 (roll-dice 8 7) lvl)))

(define-monster-spab '<bolt-spell> '<nether>
  :desc "nether bolt"
  :visual "darkness"
  :damage #'(lambda (lvl)
	      (+ 30 (roll-dice 5 5) (int-/ (* 3 lvl) 2))))

(define-monster-spab '<bolt-spell> '<water>
  :desc "water bolt"
  :damage #'(lambda (lvl)
	      (+ (roll-dice 10 10) lvl)))

(define-monster-spab '<ball-spell> '<fire>
  :desc "fire ball"
  :visual "fire"
  :damage #'(lambda (lvl)
	      (+ 10 (randint (int-/ (* lvl 7) 2)))))

(define-monster-spab '<ball-spell> '<cold>
  :desc "cold ball"
  :visual "cold"
  :damage #'(lambda (lvl)
	      (+ 10 (randint (int-/ (* lvl 3) 2)))))

(define-monster-spab '<ball-spell> '<acid>
  :desc "acid ball"
  :visual "acid"
  :damage #'(lambda (lvl)
	      (+ 15 (randint (* lvl 3)))))

(define-monster-spab '<ball-spell> '<electricity>
  :desc "lightning ball"
  :visual "electricity"
  :damage #'(lambda (lvl)
	      (+ 8 (randint (int-/ (* lvl 3) 2)))))

(define-monster-spab '<ball-spell> '<poison>
  :desc "stinking cloud"
  :visual "poison" ;; not right, but ok
  :damage #'(lambda (lvl)
	      (declare (ignore lvl))
	      (roll-dice 12 2)))

(define-monster-spab '<ball-spell> '<mana>
  :desc "mana storm"
  :visual "magic-missile"
  :damage #'(lambda (lvl)
	      (+ (* 5 lvl) (roll-dice 10 10))))

(define-monster-spab '<ball-spell> '<darkness>
  :desc "darkness storm"
  :visual "darkness"
  :damage #'(lambda (lvl)
	      (+ (* 5 lvl) (roll-dice 10 10))))

(define-monster-spab '<ball-spell> '<water>
  :desc "whirlpool"
  :damage #'(lambda (lvl)
	      (+ 50 (randint (int-/ (* lvl 5) 2)))))

(define-monster-spab '<ball-spell> '<nether>
  :desc "nether"
  :visual "darkness"
  :damage #'(lambda (lvl)
	      (+ 50 lvl (roll-dice 10 10))))

(define-monster-spab '<dmg-spell> 1 :power 1)
(define-monster-spab '<dmg-spell> 2 :power 2)
(define-monster-spab '<dmg-spell> 3 :power 3)
(define-monster-spab '<dmg-spell> 4 :power 4)
