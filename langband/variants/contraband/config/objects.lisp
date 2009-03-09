(in-package :org.langband.contraband)

(defconstant +common-backpack-size+ 23)

(define-object-kind "backpack" "backpack"
  :numeric-id 750
  :text-sym (text-paint-value +term-white+ #\&)
  :power-lvl 3
  :weight nil
  :cost 1200
  :on-create #'(lambda (item)
		 (let ((container (make-container +common-backpack-size+)))
		   (setf (aobj.contains item) container)
		   t))
  :the-kind '<container>)

(define-object-kind "sealed-letter-to-junifer" "sealed letter for Mereo Junifer"
  :numeric-id 200
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 200
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (print-message! "You break the seal.")
	     (add-to-inventory player (get-new-object "opened-letter-to-junifer"))
	     :used)
  :the-kind '<letter>) 


(define-object-kind "opened-letter-to-junifer" "letter for Mereo Junifer (seal broken)"
  :numeric-id 201
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 90)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 201
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "read letter")
	     :still-useful)
  :the-kind '<letter>) 

(define-object-kind "sealed-letter-to-ulydes" "sealed letter for Mereo Ulydes"
  :numeric-id 202
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 202
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (print-message! "You break the seal.")
	     (add-to-inventory player (get-new-object "opened-letter-to-ulydes"))
	     :used)
  :the-kind '<letter>) 

(define-object-kind "opened-letter-to-ulydes" "letter for Mereo Ulydes (seal broken)"
  :numeric-id 203
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 203
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "opened letter")
	     :still-useful)
  :the-kind '<letter>) 


(define-object-kind "sealed-letter-to-tepesco" "sealed letter for Consul Tepesco"
  :numeric-id 204
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 204
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (print-message! "You break the seal.")
	     (add-to-inventory player (get-new-object "opened-letter-to-tepesco"))
	     :used)
  :the-kind '<letter>) 

(define-object-kind "opened-letter-to-tepesco" "letter for Consul Tepesco (seal broken)"
  :numeric-id 205
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 205
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "opened letter")
	     :still-useful)
  :the-kind '<letter>) 


(define-object-kind "measurements-junifer" "Junifer's physical measurements"
  :numeric-id 206
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 86)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 206
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "read measurements")
	     :still-useful)
  :the-kind '<letter>) 

(define-object-kind "export-forms" "export forms"
  :numeric-id 207
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 207
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "read export forms")
	     :still-useful)
  :the-kind '<letter>)

(define-object-kind "filled-out-export-forms" "export forms (filled out)"
  :numeric-id 208
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 208
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "read export forms")
	     :still-useful)
  :the-kind '<letter>)

(define-object-kind "signed-export-forms" "export forms (signed)"
  :numeric-id 209
  :text-sym (text-paint-value +term-white+ #\?)
  :gfx-sym (tile-paint-value 10 19)
  :weight 5
  :cost 125
  :power-lvl 1
  :sort-value 209
  :flags '(<never-random>)
  :on-read (object-effect (dungeon player item)
	     (warn "read export forms")
	     :still-useful)
  :the-kind '<letter>)


(define-object-kind "green-silk-dress" "& green dress~ of silk"
  :numeric-id 301
  :gfx-sym (tile-paint-value 42 0)
  :text-sym (text-paint-value +term-green+ #\()
  :weight 5
  :cost 50
  :power-lvl 7
  :sort-value 301
  :the-kind '<body-armour>
  :armour-rating 1
  :stat-modifiers '((<pre> +2)))

(define-object-kind "trader-amulet" "& amulet~ of the atrocitan trader's guild"
  :numeric-id 302
  :gfx-sym (tile-paint-value 8 37)
  :text-sym (text-paint-value +term-blue+ #\")
  :weight 1
  :cost 75
  :power-lvl 2
  :sort-value 302
  :the-kind '<amulet>
  :stat-modifiers '((<pre> +1)))

(define-object-kind "facts-machine" "& atrocitan facts-machine~"
  :numeric-id 701
  :gfx-sym (tile-paint-value 43 0)
  :text-sym (text-paint-value +term-umber+ #\o)
  :weight 5
  :cost 5000
  :power-lvl 25
  :sort-value 701
  :flags '(<never-random>)
  :the-kind '<weird>)
