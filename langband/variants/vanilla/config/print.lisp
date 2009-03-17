;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/config/print.lisp - tweakable printing of fields
Copyright (c) 2003 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)

;; overrides the one in engine
(register-field-order +tiledfields-frame+
		      '((-tiled/stats- :rows 6)
			<nothing> ;; defaults to 1 row
			-tiled/level- ;; defaults to 1 row, too
			-tiled/armour-
			(-tiled/hitpoints- :rows 2)
			(-tiled/mana- :rows 2)))

(define-printfield-handler '-tiled/stats-
    +tiledfields-frame+
    (printfield-handler (win col row variant player dungeon)
	(let ((stat-len (variant.stat-length variant)))
	  (dotimes (i stat-len)
	    (setf (idx-value (+ i  0)) (svref (player.active-stats player) i)
		  (idx-value (+ i 100)) (get-stat-name-from-num i))
	    
	    (setf (window-coord win +foreground+ (+ col 0) (+ row i)) (idx-paint-value +term-l-blue+ (+ 100 i))
		  (window-coord win +foreground+ (+ col 1) (+ row i)) (idx-paint-value +term-white+  (+   0 i)))
	    
	    (paint-coord win (+ col 0) (+ row i) +winflag-normal-paint+)
	    (paint-coord win (+ col 1) (+ row i) +winflag-normal-paint+)
	    )))
  :hook-on-redraw '[stats])

(define-printfield-handler '-tiled/level-
    +tiledfields-frame+
    (printfield-handler (win col row variant player dungeon)
	(setf (idx-value (+ 0 8)) (player.power-lvl player)
	      (idx-value (+ 0 8 100)) "Lvl")
	  
	(setf (window-coord win +foreground+ (+ col 0) (+ row 0)) (idx-paint-value +term-l-blue+ (+ 100 8))
	      (window-coord win +foreground+ (+ col 1) (+ row 0)) (idx-paint-value +term-white+  (+   0 8)))
	(paint-coord win (+ col 0) (+ row 0) +winflag-normal-paint+)
	(paint-coord win (+ col 1) (+ row 0) +winflag-normal-paint+)

	)
  :hook-on-redraw '[level])

(define-printfield-handler '-tiled/armour-
    +tiledfields-frame+
    (printfield-handler (win col row variant player dungeon)
	(setf (idx-value (+ 0 9)) (+ (get-armour-rating (player.perceived-abilities player))
				     (get-armour-modifier (player.perceived-abilities player)))
	      (idx-value (+ 0 9 100)) "AC")
	  
	(setf (window-coord win +foreground+ (+ col 0) (+ row 0)) (idx-paint-value +term-l-blue+ (+ 100 9))
	      (window-coord win +foreground+ (+ col 1) (+ row 0)) (idx-paint-value +term-white+  (+   0 9)))
	
	;;(paint-coord win (+ col 0) (+ row 0) +winflag-normal-paint+)
	;;(paint-coord win (+ col 1) (+ row 0) +winflag-normal-paint+)
	;;(warn "did tiled armour")
	;;(flush-coords win col row 2 1)
	)
  :hook-on-redraw '[armour])

(define-printfield-handler '-tiled/hitpoints-
    +tiledfields-frame+
    (printfield-handler (win col row variant player dungeon)
	(let ((bg-texture (cond ((non-negative-integer? (window.backgroundfile win))
				 (window.backgroundfile win))
				(t 0)))
	      (hptext-colour +term-l-blue+)
	      (hp-colour +term-white+)
	      (max-hp (maximum-hp player))
	      (cur-hp (current-hp player)))

	  (when (< (* 2 cur-hp) max-hp)
	    (setf bg-texture 2))
	  
	  (setf (idx-value (+ 0 10)) cur-hp
		(idx-value (+ 0 10 100)) "HP"
		(idx-value (+ 0 11)) max-hp
		(idx-value (+ 0 11 100)) "Max")
		
	  
	  (setf (window-coord win +foreground+ (+ col 0) (+ row 0)) (idx-paint-value hptext-colour (+ 100 10))
		(window-coord win +foreground+ (+ col 1) (+ row 0)) (idx-paint-value hp-colour     (+   0 10)))
	  
	  (setf (window-coord win +foreground+ (+ col 0) (+ row 1)) (idx-paint-value hptext-colour (+ 100 11))
		(window-coord win +foreground+ (+ col 1) (+ row 1)) (idx-paint-value hp-colour     (+   0 11)))
	  
	  (colour-area win bg-texture col row 2 2) ;; assuming we have a 2x2 area

	  ;;(warn "repainted hp ~s" (mapcar #'car (SB-DEBUG:BACKTRACE-AS-LIST 7)))
	  (setf (window.repaint? win) t)
	  (paint-coord win (+ col 0) (+ row 0) +winflag-normal-paint+)
	  (paint-coord win (+ col 1) (+ row 0) +winflag-normal-paint+)
	  (paint-coord win (+ col 0) (+ row 1) +winflag-normal-paint+)
	  (paint-coord win (+ col 1) (+ row 1) +winflag-normal-paint+)
	  ))
  :hook-on-redraw '[hp])

(define-printfield-handler '-tiled/mana-
    +tiledfields-frame+
    (printfield-handler (win col row variant player dungeon)
	(let ((bg-texture (cond ((non-negative-integer? (window.backgroundfile win))
				 (window.backgroundfile win))
				(t 0)))
	      (hptext-colour +term-l-blue+)
	      (hp-colour +term-white+)
	      (max-mana (maximum-mana player))
	      (cur-mana (current-mana player)))

	  (when (< (* 2 cur-mana) max-mana)
	    (setf bg-texture 2))

	  (setf (idx-value (+ 0 12)) cur-mana
		(idx-value (+ 0 12 100)) "Mana")
	  
	  (setf (window-coord win +foreground+ (+ col 0) (+ row 0)) (idx-paint-value hptext-colour (+ 100 12))
		(window-coord win +foreground+ (+ col 1) (+ row 0)) (idx-paint-value hp-colour  (+   0 12)))

	  (colour-area win bg-texture col row 2 2) ;; assuming we have a 2x2 area
	  
	  ))

  :hook-on-redraw '[mana])

;; basic frame!

(define-printfield-handler '-basic/level-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
	 (let* ((lev (player.power-lvl player))
		(lower-lvl-p (< lev (player.max-level player))))

	   ;;(warn "basic level")
	   (output-string! win 0 row +term-white+ "Level")
	   
	   (print-number win (if lower-lvl-p +term-yellow+ +term-l-green+)
			 lev 6 row 5)))
    :hook-on-redraw '[level])

(define-printfield-handler '-basic/hitpoints-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)

	(let ((cur-hp (current-hp player))
	      (max-hp (maximum-hp player))
	      )
	  ;;(warn "basic hp")
	  (output-string! win 0 row +term-white+ "HP")
	  
	  (print-number win (cond ((>= cur-hp max-hp) +term-l-green+)
				  ((> cur-hp (int-/ (* max-hp *hitpoint-warning*) 10)) +term-yellow+)
				  (t +term-red+))
			cur-hp 5 row 2)
	  (win/format win 7 row +term-l-green+ "/~v" 3 max-hp)
	  t))
    
    :hook-on-redraw '[hp])

(define-printfield-handler '-basic/armour-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
        (let* ((perc (player.perceived-abilities player))
	       (ac (+ (get-armour-rating perc)
		      (get-armour-modifier perc))))
      
      (output-string! win 0 row +term-white+ "Armour")
      
      (print-number win +term-l-green+ ac 5 row 6)
      t))
    :hook-on-redraw '[armour])

(define-printfield-handler '-basic/gold-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
	(let ((gold (player.gold player)))
	  (output-string! win 0 row +term-white+ "Au")
	  (print-number win +term-l-green+ gold 9 row 2)
	  t))
    :hook-on-redraw '[gold])

(define-printfield-handler '-basic/xp-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
	(let* ((xp (player.current-xp player))
	       (lower-xp-p (< xp (player.maximum-xp player))))
      
	  (output-string! win 0 row +term-white+ "Xp")
	  
	  (print-number win (if lower-xp-p +term-yellow+ +term-l-green+) xp 8 row 3)
	  t))
    
    :hook-on-redraw '[xp])

(define-printfield-handler '-basic/race-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
	(output-string! win col row +term-white+  "            ")
	(output-string! win col row +term-l-blue+ (get-race-name player))
	t)
    
    :hook-on-redraw '[race])

(define-printfield-handler '-basic/class-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
	(output-string! win col row +term-white+  "            ")
	(output-string! win col row +term-l-blue+ (get-class-name player))
	t)
    
    :hook-on-redraw '[class])

(define-printfield-handler '-basic/stats-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
	(let ((stat-len (variant.stat-length variant)))
	  (dotimes (num stat-len)
	    (let* ((stat-val (svref (player.active-stats player) num))
		   (max-val (svref (player.modbase-stats player) num))
		   (reduced-stat-p (> max-val stat-val))
		   (name (get-stat-name-from-num num))
		   )

	      ;; maybe add the reduced-stat code later
	      (output-string! win col (+ num row) +term-white+ name)
    
	      (print-stat-value win (if reduced-stat-p +term-yellow+ +term-l-green+)
				stat-val (+ row num) (+ col 5))
	      ))))

  :hook-on-redraw '[stats])

(define-printfield-handler '-basic/speed-
    +charinfo-frame+
    (printfield-handler (win col row variant player dungeon)
        (let ((factor (- (player.speed player) +speed-base+)))
	  (cond ((= factor 0)
		 (output-string! win 0 row +term-white+ "           "))
		(t
		 (let ((colour (if (minusp factor) +term-yellow+ +term-l-green+))
		       (*winformat-forced-numbersign* t))
		   (output-string! win 0 row +term-white+ "Speed")
		   (win/format win 8 row colour "~v" 3 factor) 
		   )))
	  ))

    :hook-on-redraw '[speed])
