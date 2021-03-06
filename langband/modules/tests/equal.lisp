;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: tests/equal.lisp - equality and various predicates
Copyright (c) 2000-2001 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)


(defmacro report-predicate (pred x y)
  (let ((retval (gensym))
	(x-val (gensym))
	(y-val (gensym)))
    `(let* ((,x-val ,x)
	    (,y-val ,y)
	    (,retval (,pred ,x-val ,y-val)))
      (if ,retval
	  ,retval
	  (progn
	    (format t "~&LANG-EQUAL: Check ~s ~s failed, values: ~s ~s~%" ',pred ',x ,x-val ,y-val)
	    ,retval)))))

(defmacro report-equal (x y)
  `(report-predicate equal ,x ,y))

(defmacro report-equalp (x y)
  `(report-predicate equalp ,x ,y))

(defmethod lang-equal (first-obj second-obj)
  (equal first-obj second-obj))

(defmethod lang-equal :around (x y)
  (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find LANG-EQUAL for types ~a and ~a" (type-of x) (type-of y))
     (return-from lang-equal nil))
  
  (let ((retval (call-next-method)))
    (unless retval
      (typecase x
	(dungeon-coord
	 (lang-warn "Equality failed for ~s and ~s" (type-of x) (type-of y))
	 (describe x)
	 (describe y)
	 )
	(cons
	 (lang-warn "Equality failed for ~s and ~s" x y))
	(t
	 (lang-warn "Equality failed for ~s and ~s" (type-of x) (type-of y)))
	))
    retval))

#||
;; don't give this one anything circular
(defmethod lang-equal ((x cons) (y cons))
  (and (lang-equal (car x) (car y))
       (lang-equal (cdr x) (cdr y))))
||#

(defmethod lang-equal ((first-obj cons) (second-obj cons))
  (every #'lang-equal first-obj second-obj))


(defmethod lang-equal ((x array) (y array))
  (and (= (array-rank x) (array-rank y))
       (dotimes (axis (array-rank x) t)
	 (unless (= (array-dimension x axis)
		    (array-dimension y axis))
	   (return nil)))
       (dotimes (index (array-total-size x) t)
	 (let ((x-el (row-major-aref x index))
	       (y-el (row-major-aref y index)))
	   (unless (or (eq x-el y-el)
		       (lang-equal x-el y-el))
	     (return nil))))))

(defmethod lang-equal ((first-obj dungeon) (second-obj dungeon))

  (and (= (dungeon.depth first-obj)
	  (dungeon.depth second-obj))
       (= (dungeon.height first-obj)
	  (dungeon.height second-obj))
       (= (dungeon.width first-obj)
	  (dungeon.width second-obj))
       (lang-equal (dungeon.table first-obj)
		   (dungeon.table second-obj))
       (lang-equal (dungeon.monsters first-obj)
		   (dungeon.monsters second-obj))
       (lang-equal (dungeon.rooms first-obj)
		   (dungeon.rooms second-obj))
       ;; add later
       ;;(lang-equal (dungeon.triggers first-obj)
	;;	   (dungeon.triggers second-obj))
       ))
       

(defmethod lang-equal ((first-obj dungeon-coord) (second-obj dungeon-coord))
  (and (= (coord.floor first-obj)
	  (coord.floor second-obj))
;;       (= (coord.flags first-obj)
;;	  (coord.flags second-obj))
       (= (logand (coord.flags first-obj) +saved-cave-flags+)
	  (logand (coord.flags second-obj) +saved-cave-flags+))
       (lang-equal (coord.objects first-obj)
		   (coord.objects second-obj))
       (lang-equal (coord.monsters first-obj)
		   (coord.monsters second-obj))))
 
(defmethod lang-equal ((x active-monster) (y active-monster))
  (and (amon.kind x)
       (amon.kind y)
       (equal (get-id (amon.kind x))
	      (get-id (amon.kind y)))
       (= (current-hp x)
	  (current-hp y))
       (= (maximum-hp x)
	  (maximum-hp y))
       (= (get-creature-speed x)
	  (get-creature-speed y))
       (= (get-creature-energy x)
	  (get-creature-energy y))
       (= (get-creature-mana x)
	  (get-creature-mana y))
       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))
       (equal (creature-alive? x)
	      (creature-alive? y))))
	  
(defmethod lang-equal ((x active-object) (y active-object))
  (and (aobj.kind x)
       (aobj.kind y)
       (equal (get-id (aobj.kind x))
	      (get-id (aobj.kind y)))
       (equal (aobj.inscr x)
	      (aobj.inscr y))
       (equal (aobj.number x)
	      (aobj.number y))
       (lang-equal (aobj.contains x)
		   (aobj.contains y))
       (lang-equal (aobj.events x)
		   (aobj.events y))
       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))
       (= (aobj.identify x)
	  (aobj.identify y))
       (report-equal (aobj.speed-modifier x)
		     (aobj.speed-modifier y))

       ))

(defmethod lang-equal ((x items-on-floor) (y items-on-floor))

  (and (= (items.cur-size x)
	  (items.cur-size y))
       (lang-equal (items.objs x)
		   (items.objs y))
       ;; skip dungeon as it is just a help-pointer
       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))))

(defmethod lang-equal ((x items-worn) (y items-worn))

  (and (= (items.cur-size x)
	  (items.cur-size y))
       (lang-equal (items.objs x)
		   (items.objs y))
       ))

(defmethod lang-equal ((x items-in-container) (y items-in-container))

  #||
  (unless (= (items.cur-size x)
	     (items.cur-size y))
    (error "CS"))
  (unless (= (items.max-size x)
	     (items.max-size y))
    (lang-warn "MS"))
  ||#
  
  (and (= (items.cur-size x)
	  (items.cur-size y))
       (lang-equal (items.objs x)
		   (items.objs y))
       (= (items.max-size x)
	  (items.max-size y))
       ))

(defmethod lang-equal ((x active-room) (y active-room))

  (and (room.type x)
       (room.type y)
       (equal (room-type.id (room.type x))
	      (room-type.id (room.type y)))

       (= (location-x x)
	  (location-x y))
       (= (location-y x)
	  (location-y y))))

(defmethod lang-equal ((x player) (y player))
  ;; add misc and abilities
  
  (and

   (report-equal (player.name x) 	       (player.name y))
   (report-equal (get-id (player.class x))   (get-id (player.class y)))
   (report-equal (get-id  (player.race x))    (get-id (player.race y)))
   (report-equal (player.gender x) 	       (player.gender y))

   (report-equalp (player.base-stats x)    (player.base-stats y))
   (report-equalp (player.cur-statmods x)  (player.cur-statmods y))
   
   (report-equalp (player.hp-table x)      (player.hp-table y))
   (lang-equal (player.equipment x) (player.equipment y))
   (lang-equal (player.misc x)      (player.misc y))

   (report-equal (player.dead-from x) 	       (player.dead-from y))

   (report-equal (location-x x)         (location-x y))
   (report-equal (location-y x)         (location-y y))
   (report-equal (player.view-x x)        (player.view-x y))
   (report-equal (player.view-y x)        (player.view-y y))

   (report-equal (player.depth x)         (player.depth y))
   (report-equal (player.max-depth x)     (player.max-depth y))

   (report-equal (player.maximum-xp x)    (player.maximum-xp y))
   (report-equal (player.current-xp x)    (player.current-xp y))
   (report-equal (player.fraction-xp x)   (player.fraction-xp y))
   
   (report-equal (current-hp x)           (current-hp y))
   (report-equal (player.fraction-hp x)   (player.fraction-hp y))

   (report-equal (current-mana x)         (current-mana y))
   (report-equal (player.fraction-mana x) (player.fraction-mana y))

   (report-equal (player.gold x)         (player.gold y))
   (report-equal (player.satiation x)    (player.satiation y))
   (report-equal (player.energy x)       (player.energy y))

   (report-equal (player.power-lvl x)     (player.power-lvl y))
   (report-equal (player.max-level x)     (player.max-level y))   


   (report-equal (maximum-hp x)           (maximum-hp y))
   (report-equal (maximum-mana x)          (maximum-mana y))
   (report-equalp (player.xp-table x)      (player.xp-table y))

   (report-equal (player.energy-use x)   (player.energy-use y))
   (report-equal (player.leaving? x)     (player.leaving? y))
       
   (report-equal (creature-alive? x)        (creeature-alive? y))
   (report-equal (player.speed x)        (player.speed y))

   (report-equal (player.burden x)         (player.burden y))
   (report-equal (get-light-radius x)   (get-light-radius y))
   (report-equal (player.infravision x)    (player.infravision y))

   (lang-equal (get-creature-inventory x) (get-creature-inventory y))
   (lang-equal (player.skills x)    (player.skills y))

   (report-equalp (player.modbase-stats x) (player.modbase-stats y))
   (report-equalp (player.active-stats x)  (player.active-stats y))
       
   (lang-equal (player.actual-abilities x)     (player.actual-abilities y))
   (lang-equal (player.perceived-abilities x)  (player.perceived-abilities y))

   (report-equalp (get-resistance-table x)    (get-resistance-table y))
   ))


(defmethod lang-equal ((x skills) (y skills))
  (let ((var-obj *variant*))
    (when var-obj ;; fix this?
      (dolist (i (variant.skill-translations var-obj))
	(unless (= (slot-value x (cdr i))
		   (slot-value y (cdr i)))
	  (return-from lang-equal nil))))
    t))

(defmethod lang-equal ((x level) (y level))

  (and (equal (level.id x) (level.id y))
       (equal (level.depth x) (level.depth y))
       (equal (level.rating x) (level.rating y))
       (lang-equal (level.dungeon x)
		   (level.dungeon y))
       ))

(defun %print-set-diff (x y)
  (let ((s-a (loop for xk being the hash-keys of x collecting xk))
	(s-b (loop for xk being the hash-keys of y collecting xk)))
    (warn "Set-diff ~s ~s" (set-difference s-a s-b :test #'equal) (set-difference s-b s-a :test #'equal))))

(defmethod lang-equal ((x hash-table) (y hash-table))
  (flet ((compare-tables (xtbl ytbl)
	   (maphash #'(lambda (x-key x-value)
			(multiple-value-bind (y-value foundp)
			    (gethash x-key ytbl)
			  (unless (and foundp (lang-equal x-value y-value))
			    (return-from compare-tables nil)))
			nil)
		    xtbl)
	   t))
    
    (and (report-equal (hash-table-test x) (hash-table-test y))
	 (let ((cnt-eql (report-equal (hash-table-count x) (hash-table-count y))))
	   (unless cnt-eql
	     (%print-set-diff x y))
	   cnt-eql)						 
	 (compare-tables x y)
	 )))

(defmethod lang-equal ((x game-obj-table) (y game-obj-table))

  (and (lang-equal (gobj-table.obj-table x) (gobj-table.obj-table y))
       (lang-equal (gobj-table.alloc-table x) (gobj-table.alloc-table y))
       (lang-equal (gobj-table.obj-table-by-lvl x) (gobj-table.obj-table-by-lvl y))))

(defmethod lang-equal ((x alloc-entry) (y alloc-entry))
  (and (lang-equal (alloc.obj x)   (alloc.obj y))
       (lang-equal (alloc.index x) (alloc.index y))
       (lang-equal (alloc.depth x) (alloc.depth y))
       ;;; ....
       ))
       

(defmethod lang-equal ((x monster-kind) (y monster-kind))
  (and (report-equal (get-id x) (get-id y))
       (report-equal (monster.numeric-id x) (monster.numeric-id y))
       (report-equal (monster.name x) (monster.name y))
       (report-equal (monster.desc x) (monster.desc y))
       (report-equal (gfx-sym x) (gfx-sym y))
       (report-equal (text-sym x) (text-sym y))
       (report-equal (monster.alignment x) (monster.alignment y))
       (report-equal (monster.type x) (monster.type y))
       (report-equalp (alloc-locations x) (alloc-locations y))
       (report-equal (monster.hitpoints x) (monster.hitpoints y))
       (report-equal (monster.armour x) (monster.armour y))
       (report-equal (monster.speed x) (monster.speed y))
       (report-equal (monster.xp x) (monster.xp y))
       (report-equal (monster.gender x) (monster.gender y))

       (report-equal (monster.abilities x) (monster.abilities y))
       (report-equal (get-immunities x) (get-immunities y))
       (report-equal (get-vulnerabilities x) (get-vulnerabilities y))
       (report-equal (monster.alertness x) (monster.alertness y))
       (report-equal (monster.vision x) (monster.vision y))
       (lang-equal   (monster.attacks x) (monster.attacks y))
       (report-equal (monster.treasures x) (monster.treasures y))
       (report-equal (monster.sp-abilities x) (monster.sp-abilities y))
       ))
		   
(defmethod lang-equal ((x attack) (y attack))
  (and (report-equal (attack.kind x) (attack.kind y))
       (report-equal (attack.dmg-type x) (attack.dmg-type y))
       (report-equal (attack.damage x) (attack.damage y))
       ))

(defmethod lang-equal ((x object-kind) (y object-kind))
  (and (report-equal (get-id x) (get-id y))
       (report-equal (object.numeric-id x) (object.numeric-id y))
       (report-equal (object.name x) (object.name y))
       (report-equal (text-sym x) (text-sym y))
       (report-equal (gfx-sym x) (gfx-sym y))
       (report-equalp (alloc-locations x) (alloc-locations y))
       (report-equal (object.weight x) (object.weight y))
       (report-equal (object.cost x) (object.cost y))
       (report-equal (object.flags x) (object.flags y))
       (report-equal (object.easy-know x) (object.easy-know y))
       (report-equal (object.aware x) (object.aware y))
       (report-equal (object.tried x) (object.tried y))
       (report-equal (object.flavour x) (object.flavour y))
       (report-equal (object.sort-value x) (object.sort-value y))
       (report-equal (get-immunities x) (get-immunities y))
       (report-equal (get-light-radius x) (get-light-radius y))
       (report-equal (get-vulnerabilities x) (get-vulnerabilities y))
       (report-equal (get-stat-sustains x) (get-stat-sustains y))
       (report-equal (object.speed-modifier x) (object.speed-modifier y))
       (report-equal (object.abilities x) (object.abilities y))
       (report-equal (get-stat-modifiers x) (get-stat-modifiers y))
       (report-equal (get-resists x) (get-resists y))
       (report-equal (get-ignores x) (get-ignores y))
       (report-equal (get-armour-rating x) (get-armour-rating y))
       (report-equal (get-armour-modifier x) (get-armour-modifier y))
       (report-equal (get-damage-dice x) (get-damage-dice y))
       (report-equal (get-number-of-damage-dice x) (get-number-of-damage-dice y))
       (report-equal (get-tohit-modifier x) (get-tohit-modifier y))
       (report-equal (get-damage-modifier x) (get-damage-modifier y))
       ;; skip events
       ;; skip effects
       ))

(defmethod lang-equal ((x player-abilities) (y player-abilities))
  (and (report-equal (get-armour-rating x)         (get-armour-rating y))
       (report-equal (get-armour-modifier x)     (get-armour-modifier y))
       (report-equal (get-tohit-modifier x) (get-tohit-modifier y))
       (report-equal (get-damage-modifier x) (get-damage-modifier y))
       ))


(defmethod lang-equal ((x misc-player-info) (y misc-player-info))
  (and (report-equal (playermisc.age x)         (playermisc.age y))
       (report-equal (playermisc.status x)      (playermisc.status y))
       (report-equal (playermisc.height x)      (playermisc.height y))
       (report-equal (playermisc.weight x)      (playermisc.weight y))
       ))

(defmethod lang-equal ((x variant) (y variant))
  (and
   (report-equal  (get-id x)       (get-id y))
   (report-equal  (variant.name x)     (variant.name y))
   (report-equalp (variant.genders x)  (variant.genders y))
   (lang-equal    (variant.races x)    (variant.races y))
   ;; add more
   ))
