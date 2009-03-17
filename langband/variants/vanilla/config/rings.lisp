;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/rings.lisp - rings for vanilla variant
Copyright (c) 2002,2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

(define-object-kind "ring-str" "strength"
  :numeric-id 132
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4424
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (1+ (magic-bonus-for-level 5 depth))))
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (boost-stats! item (- 0 bonus)))
			    (t
			     (boost-stats! item bonus)))))
  :stat-modifiers '((<str> +1)))
				 
				 

(define-object-kind "ring-dex" "dexterity"
  :numeric-id 133
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4426
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (1+ (magic-bonus-for-level 5 depth))))
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (boost-stats! item (- 0 bonus)))
			    (t
			     (boost-stats! item bonus)))))
  :stat-modifiers '((<dex> +1)))

(define-object-kind "ring-con" "constitution"
  :numeric-id 134
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4427
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (1+ (magic-bonus-for-level 5 depth))))
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (boost-stats! item (- 0 bonus)))
			    (t
			     (boost-stats! item bonus)))))
  :stat-modifiers '((<con> +1)))

(define-object-kind "ring-int" "intelligence"
  :numeric-id 135
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 500
  :flags '(<hide-type>)
  :sort-value 4425
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (1+ (magic-bonus-for-level 5 depth))))
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (boost-stats! item (- 0 bonus)))
			    (t
			     (boost-stats! item bonus)))))
  :stat-modifiers '((<int> +1)))

(define-object-kind "ring-speed" "speed"
  :numeric-id 136
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 80
  :locations '((80 . 1))
  :weight 2
  :cost 100000
  :flags '(<hide-type>)
  :sort-value 4431
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (+ (randint 5) (magic-bonus-for-level 5 depth))))
		      (when (< (random 100) 50) (incf bonus)) ;; might get lucky, eh?
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (decf (aobj.speed-modifier item) bonus))
			    (t
			     (incf (aobj.speed-modifier item) bonus)))
		      )))

(define-object-kind "ring-searching" "searching"
  :numeric-id 137
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 250
  :flags '(<hide-type>)
  :sort-value 4423
  :the-kind '<ring>
  :skill-modifiers '(<search>))

(define-object-kind "ring-teleport" "teleportation"
  :numeric-id 138
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 0
  :flags '(<easy-know> <curse>)
  :sort-value 4404
  :the-kind '<ring>
  :abilities '(<random-teleport>))

(define-object-kind "ring-slow-digest" "slow digestion"
  :numeric-id 139
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 250
  :flags '(<easy-know>)
  :sort-value 4406
  :the-kind '<ring>
  :abilities '(<slow-digestion>))

(define-object-kind "ring-resist-fire" "resist fire"
  :numeric-id 140
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 2
  :cost 250
  :flags '(<easy-know>)
  :sort-value 4408
  :the-kind '<ring>
  :ignores '(<fire>)
  :resists '(<fire>))

(define-object-kind "ring-resist-cold" "resist cold"
  :numeric-id 141
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 2
  :cost 250
  :flags '(<easy-know>)
  :sort-value 4409
  :the-kind '<ring>
  :ignores '(<cold>)
  :resists '(<cold>))

(define-object-kind "ring-feather-fall" "feather falling"
  :numeric-id 142
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 200
  :flags '(<easy-know>)
  :sort-value 4407
  :the-kind '<ring>
  :abilities '(<feather-fall>))

(define-object-kind "ring-resist-poison" "poison resistance"
  :numeric-id 143
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 40
  :locations '((40 . 2))
  :weight 2
  :cost 16000
  :flags '(<easy-know>)
  :sort-value 4420
  :the-kind '<ring>
  :resists '(<poison>))

(define-object-kind "ring-free-action" "free action"
  :numeric-id 144
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 2
  :cost 1500
  :flags '(<easy-know>)
  :sort-value 4421
  :the-kind '<ring>
  :abilities '(<free-action>))

(define-object-kind "ring-weakness" "weakness"
  :numeric-id 145
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 0
  :flags '(<hide-type> <curse>)
  :sort-value 4402
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		   (curse-object! item :light)
		   (boost-stats! item (- -1 (magic-bonus-for-level 5 depth))))

  :stat-modifiers '((<str> -1)))

(define-object-kind "ring-flames" "flames"
  :numeric-id 146
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 2
  :cost 3000
  :sort-value 4418
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (setf (get-armour-modifier item)
			  (+ 5 (randint 5) (magic-bonus-for-level 10 depth))))
  :ignores '(<fire>)
  :ac-modifier 15
  :resists '(<fire>))

(define-object-kind "ring-acid" "acid"
  :numeric-id 147
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 2
  :cost 3000
  :sort-value 4417
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (setf (get-armour-modifier item)
			  (+ 5 (randint 5) (magic-bonus-for-level 10 depth))))
    
  :ignores '(<acid>)
  :ac-modifier 15
  :resists '(<acid>))

(define-object-kind "ring-ice" "ice"
  :numeric-id 148
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 2
  :cost 3000
  :sort-value 4419
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (setf (get-armour-modifier item)
			  (+ 5 (randint 5) (magic-bonus-for-level 10 depth))))
  :ignores '(<cold>)
  :ac-modifier 15
  :resists '(<cold>))

(define-object-kind "ring-woe" "woe"
  :numeric-id 149
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 50
  :locations '((50 . 1))
  :weight 2
  :cost 0
  :flags '(<hide-type> <curse>)
  :sort-value 4400
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		   (curse-object! item :light)
		   (boost-stats! item (- -1 (magic-bonus-for-level 5 depth)))
		   (setf (get-armour-modifier item) (- -5 (magic-bonus-for-level 10 depth))))

  :abilities '(<random-teleport>)
  :stat-modifiers '((<chr> -1) (<wis> -1)))


(define-object-kind "ring-stupid" "stupidity"
  :numeric-id 150
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 0
  :flags '(<hide-type> <curse>)
  :sort-value 4403
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		   (curse-object! item :light)
		   (boost-stats! item (- -1 (magic-bonus-for-level 5 depth))))

  :stat-modifiers '((<int> -1)))

(define-object-kind "ring-dmg" "damage"
  :numeric-id 151
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 2
  :cost 500
  :sort-value 4429
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (+ 5 (randint 3) (magic-bonus-for-level 7 depth))))
		      (when (or (eq status :bad) (eq status :horrible))
			(setf bonus (- 0 bonus))
			(curse-object! item :light))

		      ;;(warn "bonus is ~s" bonus)
		      (setf (get-damage-modifier item) bonus)))
  :the-kind '<ring>) 

(define-object-kind "ring-to-hit" "accuracy"
  :numeric-id 152
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 20
  :locations '((20 . 1))
  :weight 2
  :cost 500
  :sort-value 4428
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (+ 5 (randint 3) (magic-bonus-for-level 7 depth))))
		      (when (or (eq status :bad) (eq status :horrible))
			(curse-object! item :light)
			(setf bonus (- 0 bonus)))
		      
		      ;;(warn "bonus is ~s" bonus)
		      (setf (get-tohit-modifier item) bonus)))
  :the-kind '<ring>) 

(define-object-kind "ring-protection" "protection"
  :numeric-id 153
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 10
  :locations '((10 . 1))
  :weight 2
  :cost 500
  :sort-value 4416
  :on-add-magic (magic-add (item depth status)
		    (let ((bonus (+ 5 (randint 5) (magic-bonus-for-level 10 depth))))
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (setf (get-armour-modifier item) (- 0 bonus)))
			    (t
			     (setf (get-armour-modifier item) bonus))
			    )))

  :the-kind '<ring>) 

(define-object-kind "ring-aggr-monster" "aggravate monster"
  :numeric-id 154
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 5
  :locations '((5 . 1))
  :weight 2
  :cost 0
  :flags '(<easy-know> <curse>)
  :sort-value 4401
  :the-kind '<ring>
  :abilities '(<aggravate>))

(define-object-kind "ring-see-inv" "see invisible"
  :numeric-id 155
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 340
  :flags '(<easy-know>)
  :sort-value 4422
  :the-kind '<ring>
  :abilities '(<see-invisible>)) 

(define-object-kind "ring-sust-str" "sustain strength"
  :numeric-id 156
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 750
  :flags '(<easy-know>)
  :sort-value 4410
  :the-kind '<ring>
  :stat-sustains '(<str>))

(define-object-kind "ring-sust-int" "sustain intelligence"
  :numeric-id 157
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 600
  :flags '(<easy-know>)
  :sort-value 4411
  :the-kind '<ring>
  :stat-sustains '(<int>))

(define-object-kind "ring-sust-wis" "sustain wisdom"
  :numeric-id 158
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 600
  :flags '(<easy-know>)
  :sort-value 4412
  :the-kind '<ring>
  :stat-sustains '(<wis>))

(define-object-kind "ring-sust-con" "sustain constitution"
  :numeric-id 159
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((10 . 1))
  :weight 2
  :cost 750
  :flags '(<easy-know>)
  :sort-value 4413
  :the-kind '<ring>
  :stat-sustains '(<con>))

(define-object-kind "ring-sust-dex" "sustain dexterity"
  :numeric-id 160
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 750
  :flags '(<easy-know>)
  :sort-value 4414
  :the-kind '<ring>
  :stat-sustains '(<dex>))

(define-object-kind "ring-sust-chr" "sustain charisma"
  :numeric-id 161
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 30
  :locations '((30 . 1))
  :weight 2
  :cost 500
  :flags '(<easy-know>)
  :sort-value 4415
  :the-kind '<ring>
  :stat-sustains '(<chr>))

(define-object-kind "ring-slaying" "slaying"
  :numeric-id 162
  :gfx-sym (text-paint-value +term-dark+ #\=)
  :power-lvl 40
  :locations '((40 . 1))
  :weight 2
  :cost 1000
  :flags '(<show-modififers>)
  :sort-value 4430
  :the-kind '<ring>
  :on-add-magic (magic-add (item depth status)
		    (let ((hit-bonus (+ (randint 5) (magic-bonus-for-level 5 depth)))
			  (dmg-bonus (+ (randint 5) (magic-bonus-for-level 5 depth))))
		      
		      (cond ((or (eq status :bad) (eq status :horrible))
			     (curse-object! item :light)
			     (setf (get-tohit-modifier item) (- 0 hit-bonus)
				   (get-damage-modifier item) (- 0 dmg-bonus)))
			    (t
			     (setf (get-tohit-modifier item) hit-bonus
				   (get-damage-modifier item) dmg-bonus))
			    )))
  )
