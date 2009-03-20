;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/variant.lisp - code related to variant object
Copyright (c) 2003, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(defmethod activate-object :before ((var-obj evomyth) &key)
  "Initialises variant-variables that should be there before
the rest of the game is init'ed."

  ;; import stuff from dialogue
  ;;(cl:use-package :org.langband.dialogue :org.langband.evomyth)
  
  (setf (variant.images var-obj) (make-array 100 :initial-element nil))

  ;; get the images init'ed right away
  (loop for i from 0
	for x across *evomyth-images*
	do
	(load-image-spec& var-obj i x))
  
  (pushnew (make-gender :id "male" :symbol '<male> :name "Male" :win-title "King")
	   (variant.genders var-obj) :test #'eql :key #'gender.symbol)
  (pushnew (make-gender :id "female" :symbol '<female> :name "Female" :win-title "Queen")
	   (variant.genders var-obj) :test #'eql :key #'gender.symbol)

  (evo/register-levels! var-obj)
  
  (let ((*load-verbose* nil))
    (load-variant-data& var-obj "config/defines")
    (load-variant-data& var-obj "config/rooms")
    (load-variant-data& var-obj "config/settings")
    (load-variant-data& var-obj "config/character")
    (load-variant-data& var-obj "config/skills")
    (load-variant-data& var-obj "config/abilities")
    (load-variant-data& var-obj "config/keys")
    )

  (evo/init-eq-system var-obj)

  var-obj)

(defmethod activate-object ((var-obj evomyth) &key)
  
  (let ((*load-verbose* nil))
    (initialise-objects& var-obj :file "config/objects")
    (initialise-objects& var-obj :file "config/armour")
    (initialise-objects& var-obj :file "config/weapons")
    (initialise-monsters& var-obj :file "config/family")
    (initialise-monsters& var-obj :file "config/animals")


    ;; hack, done after objects and monsters are ok'ed.. maybe move to :after
    (let ((quest:*variant-class* 'evomyth))
      (load-variant-data& var-obj "tasks/leave-valley")
      (load-variant-data& var-obj "tasks/find-partner"))
    
    (load-variant-data& var-obj "people/grandpa")
    )
  
  ;; more hackish
  (let ((pairs '())
	(table (variant.quests var-obj)))
    (maphash #'(lambda (k v) (push (cons k v) pairs)) table)
    (clrhash table)
    (dolist (i pairs)
      (setf (gethash (car i) table) (make-instance (cdr i))))
    (setf (variant.quests var-obj) table))
    
  
  var-obj)

(defmethod activate-object :after ((var-obj evomyth) &key)
  "Does post-initialisation of the game with variant tweaking."

  (setf (get-setting var-obj :basic-frame-printing)
	(get-settings-obj "evo-basic-frame"))
  
  (cond ((eq (get-system-type) 'sdl)
	 (setf (get-setting var-obj :birth)
	       (get-settings-obj "sdl-evo-birth-settings"))

	 (setf (get-setting var-obj :char-display)
	       (get-settings-obj "sdl-evo-chardisplay"))

	 )
	
	;; otherwise go for basic stuff
	(t
	 (setf (get-setting var-obj :birth)
	       (get-settings-obj "evo-birth-settings"))

	 (setf (get-setting var-obj :char-display)
	       (get-settings-obj "chardisplay-settings"))

	 ))
  
  (register-level-builder! "valley"
			   (get-late-bind-function 'org.langband.evomyth
						   'create-bare-valley-level-obj))
  
  var-obj)


(defmethod redraw-stuff ((variant evomyth) (dungeon dungeon) (player player))
  
  (unless (any-redraws? player) (return-from redraw-stuff nil))

  (let ((retval nil))

    (when (want-redraw? player '[extra])
      (reset-redraw! player '[extra])
      (setf retval t))
    
    (if (call-next-method)
	t
	retval)
    ))

(defun evo/init-eq-system (var-obj)
  "Initialises values dealing with the equipment (sorting, worn slots)."

  (let ((equip-order
	 '((eq.lefthand    "Left Hand"              (active-object/melee-weapon active-object/ranged-weapon active-object/shield))
	   (eq.righthand   "Right Hand"             (active-object/melee-weapon active-object/ranged-weapon active-object/shield))
	   (eq.l-ring      "On left index finger"   active-object/ring)
	   (eq.r-ring      "On right index finger"  active-object/ring)
	   (eq.neck        "Around neck"            active-object/neckwear)
	   (eq.head        "On head"                active-object/headgear) ;; 15%
	   (eq.armour      "On body"                active-object/body-armour) ;; 50%
	   (eq.cloak       "About body"             active-object/cloak) ;; bonus armour
	   (eq.hands       "On hands"               active-object/gloves) ;; 5%
	   (eq.legs        "On legs"                active-object/legwear) ;; 20%
	   (eq.feet        "On feet"                active-object/boots) ;; 10%
	   (eq.backpack    "On back"                active-object/container t)
	   )))
    
    (register-slot-order& var-obj equip-order))
  )

(defvar *current-arrow* 0)

(defmethod handle-mouse-click ((variant evomyth) window button x y)

  (let ((num-id (window.num-id window))
	(player *player*)
	(dungeon *dungeon*))
  
    (cond ((= num-id +inv-frame+)
	   ;; handle normal case

	   (when (eq button :left)
	     (let ((wid (window.pixel-width window))
		   (tile-wid (window.tile-width window)))
	       ;; two button-sets
	       (cond ((> x (- wid tile-wid)) ;; last tile
		      (switch-inventory-view variant player))
		     ((> x (- wid (* 2 tile-wid))) ;; second last tile 
		      (switch-map-mode variant dungeon player)))
	       )))
	  
	  ((and (= num-id *map-frame*)
		(eq button :right))
	   ;; first get panel coords, then translate to real coords
	   (let* ((loc-x (int-/ x (window.tile-width window)))
		  (loc-y (int-/ y (window.tile-height window)))
		  (rx (+ loc-x (player.view-x player)))
		  (ry (+ loc-y (player.view-y player)))
		  (tgt (get-target-at-coordinate dungeon rx ry)))

	     (when (is-legal-target? dungeon tgt)
	       (when (player.target player)
		 (remove-target-display (player.target player)))
	       (display-target dungeon tgt)
	       (setf (player.target player) tgt))

	     
	     ;;(warn "right click in square ~s ~s" loc-x loc-y)
	     ))
	  
	  (t nil))
    
    t))

(defmethod arrange-game-exit& ((variant evomyth) player)
  (declare (ignorable player))
  ;; nothing special happens yet
  t)

(defun define-skill (id slot-name slot-alias &key desc index)
  (let ((skill (make-evo/skill :id id :slot slot-name :alias slot-alias
			       :desc desc :idx index)))
    ;;(warn "defining skill ~s with index ~s" slot-alias index)

    (setf (aref (variant.skills *variant*) index) skill)
    ;;(vector-push skill *skills*)
    
    skill))

(defun evo/update-gobj-table! (variant key o-table alloc-table-creator)
  "Tries to make an allocation table from a table."
  (declare (ignore key))
  (warn "updating on ~a ~a" key o-table)
  
  (let ((okind-table (gobj-table.obj-table o-table)))
    
    (setf (gobj-table.obj-table-by-lvl o-table)
	  (convert-obj okind-table :vector :sort-table-p t
		       :sorted-by-key #'(lambda (x) (slot-value x 'power-lvl))))
    
    (setf (gobj-table.alloc-table o-table)
	  (funcall alloc-table-creator variant (gobj-table.obj-table-by-lvl o-table)))
    ))
