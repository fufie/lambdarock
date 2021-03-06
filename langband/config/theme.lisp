#||
  This is a theme-file for Langband.  A UI theme-file contains a (ui-theme ...) with
  all the theme-information.  Theme-information missing will be substituted with
  defaults.  The legal keys are:
  +full-frame+, +gfxmap-frame+, +asciimap-frame+, +message-frame+, +charinfo-frame+,
  +misc-frame+, +inv-frame+, +infodisp-frame+ and +dialogue-frame+

  Other constants that will be provided by Langband are:
    window.width - the width of the window (example: 800/1024/1280 for SDL, may vary for GCU)
    window.height - the height of the window (example: 600/768/1024 for SDL, may vary for GCU)
    gfxtiles.width - width of one graphical tile (32 typically)
    gfxtiles.height - height of one graphical tile (32 typically) 

  GCU measurements are in characters, while SDL measurements are in pixels.

  You can refer to values in other subwindows to avoid hardcoding locations, this can
  be done like:  (var window value) where 'window' refers to window you want value from
  and 'value' is either 'tile-height', 'tile-width', 'height', 'width', 'x-offset' or
  'y-offset'.

  Fonts are loaded from data/fonts/ and can either be specified by a string or by a list
  of arguments to the font-engine.  To specify bold lettergo.ttf of base size 24, do
  ("lettergo.ttf" bold 24)  Fonts are loaded before window-sizes are calculated so you
  can safely refer to tile-height and tile-width in a window when calculating sizes.

  Backgrounds are loaded from data/graphics/textures/

  Good luck
||#

(ui-theme "default-sdl"
       ;; which system does the theme apply to
       :system "xsdl"
       :default-font "vga8x16.hex" ;; this one is also fallback if other fonts fail, must be safe!
       
       (big      :key +full-frame+
		 :x 0 :y 0
		 :width window.width
		 :height window.height
		 :font "vga8x16.hex"
		 ;;:font "lettergo.ttf"
		 )
       
       (charinfo :key +charinfo-frame+
		 :x 0 :y 12
		 :disabled? true
		 ;;:disabled? false
		 :width (* 12 (var charinfo tile-width)) ;; we need 13 columns
		 :height (- (var msg y-offset) (var charinfo y-offset) (var infodisp height))
		 :background "textures/bumpi.png"
		 :font ("vga8x16.hex")
		 ;;:font "lettergo.ttf"
		 )
       
       (infodisp :key +infodisp-frame+
		 :disabled? false
		 ;;:x 0 :y (+ (var charinfo y-offset) (var charinfo height))
		 :x (var tiledfields x-offset)
		 :y (var tiledfields height)
		 :height (* 3 gfxtiles.height)
		 :width (* 2 gfxtiles.width)
		 :font "vga8x16.hex"
		 :tile-width gfxtiles.width
		 :tile-height gfxtiles.height
		 :background "textures/invbg2.png"
		 :gfx-tiles? true
		 )
       
       (gfxmap   :key +gfxmap-frame+
		 :x (var charinfo width) :y 0
		 :width (- window.width (var tiledfields width) (var charinfo width))
		 :height (- (var msg y-offset) (var gfxmap y-offset))
		 :tile-width gfxtiles.width
		 :tile-height gfxtiles.height
		 :gfx-tiles? true)

       (asciimap :key +asciimap-frame+
		 :x (var charinfo width) :y 0
		 :width (- window.width (var tiledfields width) (var charinfo width)) ;;688
		 :height (- (var msg y-offset) (var asciimap y-offset)) 
		 :font "vga8x16.hex"
		 :gfx-tiles? false)

       
       (msg      :key +message-frame+
		 :x 0 :y (- window.height (+ (var msg height) (var inventory height) (var misc height)))
		 :width (- window.width (var tiledfields width))
		 :height (* (var msg tile-height) 3) ;; how many rows?
		 ;;:background "textures/bumpi.png"
		 :background "textures/blue.png"
		 ;;:background "textures/woodfloor.png"
		 :font "vga8x16.hex"
		 ;; other styles are normal and italic
		 ;;:font #+win32 "vga8x16.hex" #-win32 ("lettergo.ttf" normal 14) 
		 )
       
       (misc     :key +misc-frame+
		 :x 0 :y (- window.height (+ (var inventory height) (var misc height)))
		 :width (- window.width (var tiledfields width))
		 :height (var misc tile-height)
		 ;;:background "textures/bumpi.png"
		 :background "textures/green.png"
		 ;;:font #+win32 "vga8x16.hex" #-win32 "lettergo.ttf"
		 :font "vga8x16.hex"
		 )

       (inventory :key +inv-frame+
		  ;;:disabled? false
		  :disabled? true
		  :x 0 :y (- window.height (* gfxtiles.height 1))
		  :width (- window.width (* 2 gfxtiles.width))
		  :height (* gfxtiles.height 1)
		  :background "textures/invbg2.png"
		  :tile-width gfxtiles.width
		  :tile-height gfxtiles.height
		  :font ("vga8x16.hex")
		  ;;:font "lettergo.ttf"
		  :gfx-tiles? true)
       
       (dialogue :key +dialogue-frame+
		 :x 0 :y 0
		 :width window.width
		 :height (- (var msg y-offset) (var dialogue y-offset))
		 :font "vga8x16.hex"
		 :background "textures/woodfloor.png"
		 :gfx-tiles? false)

       ;; shows poison/blind/etc status
       (tiledfields :key +tiledfields-frame+
		:disabled? false
		;;:disabled? true
		:x (- window.width (var tiledfields width))
		:y 0
		:width (* 2 gfxtiles.width)
		:height (- window.height (var infodisp height))
		;;:background "textures/invbg2.png"
		:tile-width gfxtiles.width
		:tile-height gfxtiles.height
		:font ("vga8x16.hex")
		:background 0 ;; in backgrounds file, only when wid/hgt is like gfxtiles 
		;;:font "lettergo.ttf"
		:gfx-tiles? true)
       
       )

(ui-theme "default-gcu"
       ;; which system does the theme apply to
       :system "gcu"
       :default-font "xxx"
       
       (big      :key +full-frame+
		 :x 0 :y 0
		 :width window.width
		 :height window.height)
       
       (charinfo :key +charinfo-frame+
		 :x 0 :y (var msg height)
		 :width 13 :height (- window.height (var msg height) (var misc height)))
       
       (map      :key +asciimap-frame+
		 :x (var charinfo width) :y (var msg height)
		 :width (- window.width (var charinfo width))
		 :height (- window.height (var msg height) (var misc height))
		 :gfx-tiles? false)
       
       (msg      :key +message-frame+
		 :x 0 :y 0
		 :width window.width
		 :height 1) ;; maybe have an if here
       
       (misc     :key +misc-frame+
		 :x 0 :y (- window.height 1)
		 :width window.width
		 :height 1)

       ;; not used
       (inventory :key +inv-frame+
		  :x 0 :y 0
		  :width window.width
		  :height 1)
       ;; not used
       (infodisp :key +infodisp-frame+
		  :x 0 :y 0
		  :width window.width
		  :height 1)
       
       (dialogue :key +dialogue-frame+
		 :x 0 :y (var msg height)
		 :width window.width
		 :height (- window.height (var msg height) (var misc height)))
       )

(ui-theme "fancy-sdl"
       ;; which system does the theme apply to
       :system "sdl"
       :default-font "vga8x16.hex" ;; this one is also fallback if other fonts fail, must be safe!
       
       (big      :key +full-frame+
		 :x 0 :y 0
		 :width window.width
		 :height window.height
		 :font "vga8x16.hex"
		 :background "textures/mapbgx.png"
		 ;;:font "lettergo.ttf"
		 )

       ;; charinfo is where we print the bloody health
       (charinfo :key +charinfo-frame+
		 :x 0 :y (- window.height (* 2 gfxtiles.height))
		 :width (* 3 gfxtiles.width)
		 :height (* 2 gfxtiles.height)
		 :horizontal-padding 16
		 :vertical-padding 16
		 :background "textures/health.png"
		 :font ("vga8x16.hex")
		 ;;:font "lettergo.ttf"
		 )

       ;; various status symbols
       (infodisp :key +infodisp-frame+
		 :disabled? false
		 ;;:x 0 :y (+ (var charinfo y-offset) (var charinfo height))
		 :x 0 :y (- window.height (* 3 gfxtiles.height))
		 :width (* 3 gfxtiles.width)
		 :height (* 1 gfxtiles.height)
		 :font "vga8x16.hex"
		 :tile-width gfxtiles.width
		 :tile-height gfxtiles.height
		 :background "textures/warning.png"
		 :gfx-tiles? true
		 )
       
       (gfxmap   :key +gfxmap-frame+
		 :x 0 :y 0
		 :horizontal-padding 16
		 :vertical-padding 16
		 :width (- window.width 0)
		 :height (- window.height (* 3 gfxtiles.height) 0)
		 :tile-width gfxtiles.width
		 :tile-height gfxtiles.height
		 :background "textures/mapbg3.png"
		 :gfx-tiles? true)

       (asciimap :key +asciimap-frame+
		 :x 0 :y 0
		 :horizontal-padding 16
		 :vertical-padding 16
		 :width (- window.width 0)
		 :height (- window.height (* 3 gfxtiles.height))
		 :font "vga8x16.hex"
		 :background "textures/mapbg3.png"
		 :gfx-tiles? false)

       
       (msg      :key +message-frame+
		 :x (* 3 gfxtiles.width) :y (- window.height (* 2 gfxtiles.height))
		 :width (- window.width (* 3 gfxtiles.width))
		 :height (* 2 gfxtiles.height)
		 :horizontal-padding 16
		 :vertical-padding 8
		 :background "textures/msgarea.png"
		 ;;:background "textures/woodfloor.png"
		 :font "vga8x16.hex"
		 ;; other styles are normal and italic
		 ;;:font #+win32 "vga8x16.hex" #-win32 ("lettergo.ttf" normal 14) 
		 )
       
       (misc     :key +misc-frame+
		 :x (* 3 gfxtiles.width) :y (- window.height (* 3 gfxtiles.height))
		 :width (- window.width (* 3 gfxtiles.width))
		 :height gfxtiles.height
		 :horizontal-padding 16
		 :vertical-padding 8
		 ;;:background "textures/bumpi.png"
		 :background "textures/inputarea.png"
		 ;;:font #+win32 "vga8x16.hex" #-win32 "lettergo.ttf"
		 :font "vga8x16.hex"
		 :disabled? false
		 )

       (inventory :key +inv-frame+
		  ;;:disabled? false
		  :disabled? true
		  :x 0 :y (- window.height (* gfxtiles.height 1))
		  :width (- window.width (* 2 gfxtiles.width))
		  :height (* gfxtiles.height 1)
		  :background "textures/invbg2.png"
		  :tile-width gfxtiles.width
		  :tile-height gfxtiles.height
		  :font ("vga8x16.hex")
		  ;;:font "lettergo.ttf"
		  :gfx-tiles? true)
       
       (dialogue :key +dialogue-frame+
		 :x 0 :y 0
		 :disabled? true
		 :width window.width
		 :height (- (var msg y-offset) (var dialogue y-offset))
		 :font "vga8x16.hex"
		 :background "textures/woodfloor.png"
		 :gfx-tiles? false)

       ;; shows poison/blind/etc status
       (tiledfields :key +tiledfields-frame+
		:disabled? true
		;;:disabled? true
		:x (- window.width (var tiledfields width))
		:y 0
		:width (* 2 gfxtiles.width)
		:height (- window.height (var infodisp height))
		;;:background "textures/invbg2.png"
		:tile-width gfxtiles.width
		:tile-height gfxtiles.height
		:font ("vga8x16.hex")
		;;:background 0 ;; in backgrounds file, only when wid/hgt is like gfxtiles 
		;;:font "lettergo.ttf"
		:gfx-tiles? true)
       
       )
