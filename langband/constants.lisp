;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: constants.lisp - constants for the game code
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

;; for use with c-code
(defconstant +false+ 0)
(defconstant +true+ 1)

(defconstant +sdl-minimum-window-width+   800 "Minimum window-width.")
(defconstant +sdl-minimum-window-height+  600 "Minimum window-height.")
(defconstant +sdl-maximum-window-width+  4800 "Maximum window-width.")
(defconstant +sdl-maximum-window-height+ 4800 "Maximum window-height.")

(defconstant +text-end+ #xff "The last legal text-value for an attr or char.")  
(defconstant +graphics-start+ #x100 "The first graphics value.")

;;; === The colours that the TERM can display.
(def-exportconst +term-dark+     (charify-number 0) "a colour")
(def-exportconst +term-white+    (charify-number 1) "a colour")
(def-exportconst +term-slate+    (charify-number 2) "a colour")
(def-exportconst +term-orange+   (charify-number 3) "a colour")
(def-exportconst +term-red+      (charify-number 4) "a colour")
(def-exportconst +term-green+    (charify-number 5) "a colour")
(def-exportconst +term-blue+     (charify-number 6) "a colour")
(def-exportconst +term-umber+    (charify-number 7) "a colour")
(def-exportconst +term-l-dark+   (charify-number 8) "a colour")
(def-exportconst +term-l-white+  (charify-number 9) "a colour")
(def-exportconst +term-violet+   (charify-number 10) "a colour")
(def-exportconst +term-yellow+   (charify-number 11) "a colour")
(def-exportconst +term-l-red+    (charify-number 12) "a colour")
(def-exportconst +term-l-green+  (charify-number 13) "a colour")
(def-exportconst +term-l-blue+   (charify-number 14) "a colour")
(def-exportconst +term-l-umber+  (charify-number 15) "a colour")
;;; === End colour-flags

;;; === The cave flags for coordinates in the dungeon.
(def-exportconst +cave-mark+  #x01 "memorized feature")
(def-exportconst +cave-glow+  #x02 "self-illuminating")
(def-exportconst +cave-icky+  #x04 "part of a vault")
(def-exportconst +cave-room+  #x08 "part of a room")
(def-exportconst +cave-seen+  #x10 "seen flag")
(def-exportconst +cave-view+  #x20 "view flag")
(def-exportconst +cave-temp+  #x40 "temp flag")
(def-exportconst +cave-wall+  #x80 "wall flag")
(def-exportconst +cave-no-tunnel+  #x100 "never tunnel here")
;;; === end cave-flags


;;; === Flags for floors

;; these flags are used
(def-exportconst +floor-flag-wall+             #x01 "The floortype is some kind of a wall, can't see/move through.")
(def-exportconst +floor-flag-permanent+        #x02 "The floortype is permanent and can never be changed.")
(def-exportconst +floor-flag-floor+            #x04 "The floortype is some kind of floor.")
(def-exportconst +floor-flag-allow-items+      #x08 "This floor-type allows items to be dropped on it.")
(def-exportconst +floor-flag-allow-creatures+  #x10
  "This floor-type allows creatures to move atop it and be constructed atop it.")
(def-exportconst +floor-flag-exit-upwards+     #x20 "Can we go up here?")
(def-exportconst +floor-flag-exit-downwards+   #x40 "Can we go down here?")
(def-exportconst +floor-flag-use-light-effect+ #x80 "Should we use a light-effect on this floor?")
;;; === end floor flags

;;; === flags that control print/redraw
(define-redraw-key [misc] "...")
(define-redraw-key [level] "...")
(define-redraw-key [race] "...")
(define-redraw-key [class] "...")
(define-redraw-key [xp] "...")
(define-redraw-key [armour] "...")
(define-redraw-key [hp] "...")
(define-redraw-key [gold] "...")
(define-redraw-key [depth] "...")
(define-redraw-key [health] "...")
(define-redraw-key [speed] "...")
(define-redraw-key [satiation] "...")
(define-redraw-key [blind] "...")
(define-redraw-key [map] "...")
(define-redraw-key [extra] "...")
(define-redraw-key [basic] "The panel on the left.")
(define-redraw-key [equipment] "Print equipment/inventory row (if there).")

;;; === end redraw/print flags

;;; === flags for updating the player, the values differ from angband!!
(define-update-key [bonuses] "...")
(define-update-key [torch] "...")
(define-update-key [hp] "...")
(define-update-key [forget-view] "...")
(define-update-key [update-view] "...")
(define-update-key [forget-flow] "...") ;; elsewhere?
(define-update-key [update-flow] "...") ;; elsewhere?
(define-update-key [monsters] "...") ;; elsewhere?
(define-update-key [distance] "...") ;; elsewhere?

(define-update-key [viewport] "Update the viewport/camera, ie what we see.")

;;; === end flags for updating the player

(def-exportconst +ident-sense+  #x01 "Item has been 'sensed'")
(def-exportconst +ident-fixed+  #x02 "Item has been 'haggled'")
(def-exportconst +ident-empty+  #x04 "Item charges are known")
(def-exportconst +ident-known+  #x08 "Item abilities are known")
(def-exportconst +ident-rumour+ #x10 "Item background is known")
(def-exportconst +ident-mental+ #x20 "Item information is known")
;;(def-exportconst +ident-cursed+ #x40 "Item is temporarily cursed [DEPRECATED, use sanctity]")
;;(def-exportconst +ident-broken+ #x80 "Item is permanently worthless [useful?]")

;;; === Sanctity of items

(def-exportconst +sanctity-blessed+        #x01  "Item is blessed.")
(def-exportconst +sanctity-holy+           #x02  "Item is holy.")
(def-exportconst +sanctity-divine+         #x04  "Item is divine.")

(def-exportconst +sanctity-cursed+         #x10 "Item is cursed.")
(def-exportconst +sanctity-heavily-cursed+ #x20 "Item is heavily cursed.")
(def-exportconst +sanctity-perma-cursed+   #x40 "Item is permanently cursed.")
;;; === Various monster-flags

(def-exportconst +monster-flag-view+  #x01 "Monster is in line of sight")
;; ...
(def-exportconst +monster-flag-born+  #x10 "Monster is being born")
(def-exportconst +monster-flag-nice+  #x20 "Monster is being nice")
(def-exportconst +monster-flag-show+  #x40 "Monster is recently memorised")
(def-exportconst +monster-flag-mark+  #x80 "Monster is currently memorised")
  
(defconstant +block-height+ 11)
(defconstant +block-width+ 11)


;;(defconst +escape+ =char-code= (charify-number 27) "escape-key")
(def-exportconst +escape+ #\Escape "Escape key")

(def-exportconst +store-item-limit+ 24 "How many items in a store")
(def-exportconst +store-maximum-items+ 18 "Max items in a store")
(def-exportconst +store-minimum-items+ 6 "Min items in store")
(def-exportconst +store-turnover+ 9 "How often does the content change")
(def-exportconst +max-itemstack-size+ 99 "What is the max items of a type")

;; make these into variables later.. 


(def-exportconst +max-sight+ 20 "maximum distance seen")

(def-exportconst +max-dungeon-width+ 200 "maximum possible width for a dungeon.")
(def-exportconst +max-dungeon-height+ 100 "maximum possible height for a dungeon.")

(defconstant +dungeon-align+ t)


(defconst +tunnel-random+   u-fixnum 10 "chance of random direction")
(defconst +tunnel-change+   u-fixnum 30 "chance of changing direction")
(defconst +tunnel-extra+    u-fixnum 15 "chance of extra tunneling")
(defconst +tunnel-door+     u-fixnum 25 "chance of doors at room entrances")
(defconst +tunnel-junction+ u-fixnum 90 "chance of doors at tunnel junctions")

;; maximum constants
(defconst +tunnel-max+ u-fixnum 900 "maximum tunnel-spaces.")

(defvar *ddd* #1A(2 8 6 4 3 1 9 7 5)
	"Global array for looping through the 'keypad directions'.")

(defvar *ddx* #1A(0 -1 0 1 -1 0 1 -1  0  1)
	"Global array for converting 'keypad direction' into 'offsets'.")

(defvar *ddy* #1A(0  1 1 1  0 0 0 -1 -1 -1)
	"Global array for converting 'keypad direction' into 'offsets'.")

(defvar *ddx-ddd* #1A(0  0 1 -1 1 -1  1 -1 0)
	"Global arrays for optimizing 'ddx[ddd[i]]'")
(defvar *ddy-ddd* #1A(1 -1 0  0 1  1 -1 -1 0)
	"Global arrays for optimizing 'ddx[ddd[i]]'")

(defconstant +simple-direction-number+ 4 "basic nswe directions in ddd arrays")
(defconstant +normal-direction-number+ 8 "basic nswe directions in ddd arrays plus diagonals")


(def-exportconst +project-jump+ #x01
  "Jumps to the target without affecting area in-between.")
(def-exportconst +project-beam+ #x02
  "Works as a beam, affecting all grids it passes.")
(def-exportconst +project-through+ #x04
  "Continues past the target.")
(def-exportconst +project-stop+ #x08
  "Stops as soon as we hit something tangible (monster, door, wall, ..).")
(def-exportconst +project-grid+ #x10
  "If there's an explosion, it can hurt the actual physical environment, ie the grids.")
(def-exportconst +project-item+ #x20
  "If there's an explosion, it can hurt any items in the explosion area.")
(def-exportconst +project-kill+ #x40
  "If there's an explosion, it can hurt any creatures in the explosion area.")
(def-exportconst +project-hide+ #x80
  "Don't show any visual clues to what happens.")

(def-exportconst +energy-normal-action+ 100 "cost of doing a normal action")

(defvar *energy-table*  #200(
    1  1  1  1  1  1  1  1  1  1 ;; Slow
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow     
    1  1  1  1  1  1  1  1  1  1 ;; Slow
    1  1  1  1  1  1  1  1  1  1 ;; S -50
    2  2  2  2  2  2  2  2  2  2 ;; S -40  
    2  2  2  2  2  2  2  3  3  3 ;; S -30
    3  3  3  3  3  4  4  4  4  4 ;; S -20
    5  5  5  5  6  6  7  7  8  9 ;; S -10
   10 11 12 13 14 15 16 17 18 19 ;; Normal
   20 21 22 23 24 25 26 27 28 29 ;; F +10
   30 31 32 33 34 35 36 36 37 37 ;; F +20
   38 38 39 39 40 40 40 41 41 41 ;; F +30
   42 42 42 43 43 43 44 44 44 44 ;; F +40
   45 45 45 45 45 46 46 46 46 46 ;; F +50
   47 47 47 47 47 48 48 48 48 48 ;; F +60
   49 49 49 49 49 49 49 49 49 49 ;; F +70
   49 49 49 49 49 49 49 49 49 49 ;; Fast
   ))


(def-exportconst +speed-base+ 110 "Base for some speed thingie")

(def-exportconst +food-max+      15000 "Bloated")
(def-exportconst +food-full+     10000 "Normal")
(def-exportconst +food-hungry+    2000 "Hungry")
(def-exportconst +food-weak+      1000 "Weak")
(def-exportconst +food-fainting+   500 "Fainting")
(def-exportconst +food-starving+   100 "Starving")

(defconstant +illegal-loc-x+ 7777)
(defconstant +illegal-loc-y+ 7777)
(defconstant +room-size-arg-len+ 5)

(def-exportconst +saved-cave-flags+ (logior +cave-mark+ +cave-glow+ +cave-icky+ +cave-room+) "Which flags to store for a cave")

;; stuff for view.lisp

(defconstant +view-max+ 1536)
(defconstant +vinfo-max-grids+ 161)
(defconstant +vinfo-max-slopes+ 126)


(defvar *vinfo-bit-fields* #8(#xFFFF #xFFFF  ;; 0
				     #xFFFF #xFFFF  ;; 1
				     #xFFFF #xFFFF  ;; 2
				     #x3FFF #xFFFF  ;; 3
				     ))
(declaim (type (simple-vector 8) *vinfo-bit-fields*))
	 
(defconstant +vinfo-bit-field-len+ 8)

(defconstant +vinfo-grid-field-len+ 8)

(defconstant +scale+ 100000)

;; for visual effects
(def-exportconst +draw-delay+ 150 "How long delay when drawing") ;; hackish, remove later
;;(def-exportconst +draw-delay+ 250) ;; hackish, remove later
;;(def-exportconst +draw-delay+ 500) ;; hackish, remove later


;;(def-exportconst +calculated-effect+ #x01)
;;(def-exportconst +temporary-effect+  #x02)

(def-exportconst +max-range+ 18 "How far can we see?")


;; the above need not be the same, but typically is the same

(def-exportconst +full-frame+ 0 "Action affects whole frame")
(def-exportconst +message-frame+ 1 "Action affects message frame")
(def-exportconst +charinfo-frame+ 2 "Action affects charinfo")
(def-exportconst +misc-frame+ 3 "Action affects misc frame")
(def-exportconst +gfxmap-frame+ 4 "Action affects gfxmap")
(def-exportconst +asciimap-frame+ 5 "Action affects gfxmap")
(def-exportconst +inv-frame+ 6 "Action affects inventory frame")
(def-exportconst +dialogue-frame+ 7 "Dialogue frame")
(def-exportconst +infodisp-frame+ 8 "Info display frame")
(def-exportconst +tiledfields-frame+ 9 "Tiledfields frame")

(def-exportconst +frametype-active+ 0 ".")
(def-exportconst +frametype-predefined+ 1 ".")

;; allowed to change between ascii and gfx
(defvar *current-map-mode* :gfx-tiles) ;; or ascii
(defvar *map-frame* +gfxmap-frame+)
;;(defvar *map-frame* +asciimap-frame+)

(defvar *windows* (make-array +predefined-frames+ :initial-element nil)
  "A vector of the available windows.")
(declaim (type (simple-array t (#.+predefined-frames+)) *windows*))

(def-exportconst +max-wincol+ 1024 "What is the maximum expected number of columns in a window.")
(def-exportconst +max-winrow+ 1024 "What is the maximum expected number of rows in a window.")

(def-exportconst +winflag-normal-paint+ #x00 "Paint")
(def-exportconst +winflag-clear-bg+ #x01 "Clear")
(def-exportconst +winflag-delay-paint+ #x02 "Delay")

;; alias!
(def-exportconst +query-frame+ +misc-frame+ "Alias")

(def-exportconst +tilefile-armour+ 3 "Gfxfile ref")
(def-exportconst +tilefile-effects+ 4 "Gfxfile ref")
(def-exportconst +tilefile-food+ 5 "Gfxfile ref")
(def-exportconst +tilefile-classes+ 6 "Gfxfile ref")
(def-exportconst +tilefile-humans+ 7 "Gfxfile ref")
(def-exportconst +tilefile-magic+ 9 "Gfxfile ref")
(def-exportconst +tilefile-misc+ 10 "Gfxfile ref")
(def-exportconst +tilefile-weapons+ 13 "Gfxfile ref")
(def-exportconst +tilefile-people+ 14 "Gfxfile ref")
(def-exportconst +tilefile-undeads+ 28 "Gfxfile ref")
(def-exportconst +tilefile-buttons+ 38 "Gfxfile ref")
(def-exportconst +tilefile-buttons-8x16+ 39 "Gfxfile ref")
(def-exportconst +tilefile-crosshairs+ 40 "Gfxfile ref")
(def-exportconst +tilefile-backgrounds+ 44 "Gfxfile ref") ;; 64x64 tiles!
(def-exportconst +tilefile-town+ 45 "Gfxfile ref")

;;; these are for the gfxtile system:
(def-exportconst +num-gfx-layers+ 4 "gfx layers")
(def-exportconst +background+ 0 "Bg layer")
(def-exportconst +decor+ 1 "Decor layer")
(def-exportconst +foreground+ 2 "Fg layer")
(def-exportconst +effect+ 3 "Fx layer")

(def-exportconst +coord-updated+ 1 "Has coord been updated")

;; can probably be moved to vanilla variant
(def-exportconst +element-vulnerability+ -100 ".")
(def-exportconst +element-calculated-resistance+ +33 ".")
(def-exportconst +element-temporary-resistance+ +66 ".")
(def-exportconst +element-immunity+ +100 ".")

(defvar *dungeon-table* nil "Reusable table of a 2d dungeon filled with coords.")

(def-exportconst +tick-precision+ 1000.0 ".")

;; walk-speed is 500msec for one square
(def-exportconst +default-tilesize+ 32 ".")                ;; do not rely on this!!
(def-exportconst +walk-speed+ (* 2 +default-tilesize+) ".") ;; pixels pr second
(def-exportconst +run-speed+ (* 2 +walk-speed+) ".")       ;; pixels pr second
(def-exportconst +throw-speed+ (* 3 +walk-speed+) ".")     ;; pixels pr second
(def-exportconst +missile-speed+ (* 4 +walk-speed+) ".")   ;; pixels pr second
(def-exportconst +monster-speed+ (* 4 +walk-speed+) ".")   ;; pixels pr second

(def-exportconst +walk-animation-interval+  (floor (/ +tick-precision+ 9.0)) ".")

(defconstant +event-poll-mode+ nil "Set to NIL if you only want to poll for events,
and T if you want to wait for events.  May change.")
