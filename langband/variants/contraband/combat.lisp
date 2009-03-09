;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/combat.lisp - contraband combat
Copyright (c) 2003 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defvar *defence-strength-help* "
DEFENCE-STRENGTH

Formula is
full-armour-rating     ((2 * evasion) + agility)
------------------ +  ------------------------------
      25               6 + armour-bulk + shield-bulk

The full-armour-rating is calculated by going through all equipped
armour, including shield, and multiplying with the skill for that
armour.  Clothes and light armours use the light-armour skill,
metal armours use the heavy-armour skill and shields use the
shield-skill. 

Each piece of body-armour makes up a percentage of the total
armour-rating of the body. 
")

(defun con/get-ac-from-obj (obj)
  (unless obj
    (return-from con/get-ac-from-obj 0))

  (let ((skill-score (gethash (ecase (object.armour-skill (aobj.kind obj))
				(<light> 'light-armour)
				(<heavy> 'heavy-armour)
				(<shield> 'shield))
			      (player.skills *player*))))

    (* (get-armour-rating obj) skill-score)))

(defun con/calculate-body-armour-rating (player)
  (let ((sum 0)
	(light-skill (gethash 'light-armour (player.skills player)))
	(heavy-skill (gethash 'heavy-armour (player.skills player)))
	(the-eq (player.equipment player)))

    (check-type light-skill integer)
    (check-type heavy-skill integer)
    (check-type the-eq item-table)

    
    ;; all calculations are boosted by 100 to let even small armours have accumulated effect
    
    (incf sum (*  15 (con/get-ac-from-obj (item-table-find the-eq 'eq.head))))
    (incf sum (*  50 (con/get-ac-from-obj (item-table-find the-eq 'eq.armour))))
    (incf sum (*   5 (con/get-ac-from-obj (item-table-find the-eq 'eq.hands))))
    (incf sum (*  20 (con/get-ac-from-obj (item-table-find the-eq 'eq.legs))))
    (incf sum (*  10 (con/get-ac-from-obj (item-table-find the-eq 'eq.feet))))
    (incf sum (* 100 (con/get-ac-from-obj (item-table-find the-eq 'eq.cloak))))

    (round-/ sum 100) ;; we normalise the armour-rating at the end
    ))

(defun con/check-for-shield (player)
  (let ((the-eq (player.equipment player)))
    (when-bind (obj (item-table-find the-eq 'eq.lefthand))
      (when (typep obj 'active-object/shield)
	(return-from con/check-for-shield obj)))
    (when-bind (obj (item-table-find the-eq 'eq.righthand))
      (when (typep obj 'active-object/shield)
	(return-from con/check-for-shield obj)))
    nil))
        

(defun con/calculate-shield-rating (player)
  (let ((shield (con/check-for-shield player))
	(shield-skill (gethash 'shield (player.skills player))))

    (check-type shield-skill integer)

    (when (= 0 shield-skill)
      (return-from con/calculate-shield-rating 0))

    (unless shield
      (return-from con/calculate-shield-rating 0))

    ;; return value
    (con/get-ac-from-obj shield)))

(defmethod get-melee-attack-skill ((variant contraband) (player player))
  (let ((wpn (get-melee-weapon player)))
    (etypecase wpn
      (null (gethash 'unarmed (player.skills player))) ;; no weapons is unarmed
      (active-object/short-blade (gethash 'short-blades (player.skills player)))
      (active-object/long-blade  (gethash 'long-blades  (player.skills player)))
      )))


(defmethod get-melee-weapon ((crt player))
  (let ((the-eq (player.equipment crt)))
    (check-type the-eq item-table)

    (when-bind (obj (item-table-find the-eq 'eq.lefthand))
      (when (typep obj 'active-object/melee-weapon)
	(return-from get-melee-weapon obj)))

    (when-bind (obj (item-table-find the-eq 'eq.righthand))
      (when (typep obj 'active-object/melee-weapon)
	(return-from get-melee-weapon obj)))

    nil))


(defun con/get-tohit-ability (player object)
  (declare (ignore object))
  ;; agility now, hack
  (aref (player.active-stats player) 1))
  

(defun con/calculate-attack-strength (player)
  "Formula is
2 * ((weapon-skill * 5) + (ability * 2) + (to-hit bonus * 5) + modifiers)
-------------------------------------------------------------------------
                             25
"
  (let ((sum 0)
	(weapon (get-melee-weapon player)))

    (incf sum (* 5 (get-melee-attack-skill *variant* player)))
    (incf sum (* 2 (con/get-tohit-ability player weapon)))
    ;; no bonus supported yet
    ;; no modifiers yet

    (incf sum sum)

    ;;(warn "Top sum is ~s" sum)
    
    (setf sum (round-/ sum 25))
    
    sum))

(defun con/get-armour-rating (player where)
  (ecase where
    (:body   (con/calculate-body-armour-rating player))
    (:shield (con/calculate-shield-rating player))
    ))

  
(defun con/get-armour-bulk (player where)
  (declare (ignore player where))
  0)

(defun con/calculate-defence-strength (player)
  
  (let ((sum 0)
	(armour-sum 0)
	(evasion-sum 0)
	;;(armour-rating (con/get-armour-rating player :body))
	(armour-bulk (con/get-armour-bulk player :body))
	(shield-rating (con/get-armour-rating player :shield))
	(shield-bulk (con/get-armour-bulk player :shield))
	(agility-value (aref (player.active-stats player) 1)) ;; hack
	(evasion-value (gethash 'evasion (player.skills player)))
	;; we fake this:
	(armour-value (gethash 'light-armour (player.skills player)))
	(shield-value (gethash 'shield (player.skills player)))
	)

;;    (check-type armour-rating integer)
    (check-type armour-bulk   integer)
    (check-type shield-rating integer)
    (check-type shield-bulk   integer)
    (check-type agility-value integer)
    (check-type evasion-value integer)
    (check-type armour-value integer)
    (check-type shield-value integer)

    (setf armour-sum (round-/ (+ (con/get-armour-rating player :body)
				 (con/get-armour-rating player :shield))
				 25))

    (setf evasion-sum (round-/ (+ evasion-value evasion-value agility-value)
			       (+ 6 shield-bulk armour-bulk)))

    (setf sum (+ armour-sum evasion-sum))

    ;; hack, remove later
    (let ((actual (player.actual-abilities player))
	  (perc (player.perceived-abilities player)))
      (setf (get-armour-rating actual) sum
	    (get-armour-modifier actual) 0
	    (get-armour-rating perc) sum
	    (get-armour-modifier perc) 0))

    
    sum))

(defmethod melee-inflict-damage! ((player player) target the-attack)
  (declare (ignorable the-attack))

  (let ((wpn (get-melee-weapon player))
	(skill (get-melee-attack-skill *variant* player))
	(str-value (aref (player.active-stats player) 0))
	(roll 1)
	(dmg 1)) ;; hack

    (when wpn
      (setf roll (roll-dice (get-number-of-damage-dice wpn) (get-damage-dice wpn))))

    (when (< roll 1) (setf roll 1))

    (setf dmg (round-/ (* roll (+ skill skill str-value))
		       #.(* 3 25)))
    
    (when (< dmg 1) (setf dmg 1))

    (deduct-hp! target dmg)
    ;;(warn "Damage is ~s for ~s" dmg the-attack)

    ;; soak here!
    ;; damage armour here
    
    dmg))


(defmethod melee-hit-creature? ((attacker player) (target active-monster) the-attack)
  
  (declare (ignore the-attack))
  
  (let* ((attack (con/calculate-attack-strength attacker))
	 (defence (get-creature-ac target))
	 (chance (+ 50 (* 3 (- attack defence)))))
    
    (check-type attack integer)
    (check-type defence integer)

    (when (< chance 5) (setf chance 5))
    (when (> chance 95) (setf chance 95))

    (warn "Trying to hit ~s with ~s% chance (~s vs ~s)"
	  (get-creature-name target) chance attack defence)
	  
    
    ;; percentage
    (< (random 100) chance)))
