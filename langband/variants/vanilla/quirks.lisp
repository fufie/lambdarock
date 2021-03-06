;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/quirks.lisp - special settings for Vanilla
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.vanilla)


(defmethod activate-object :before ((variant vanilla-variant) &key)
  "Initialises variant-variables that should be there before
the rest of the game is init'ed."

  (setf (variant.images variant) (make-array 100 :initial-element nil))

  ;; hack, we start at 6.50am
  (setf (variant.turn variant) (+ (* 6 +van/turns-in-hour+)
				  (* 50 +van/turns-in-minute+)))
  
  (pushnew (make-gender :id "male" :symbol '<male> :name "Male" :win-title "King")
	   (variant.genders variant) :test #'eql :key #'gender.symbol)
  (pushnew (make-gender :id "female" :symbol '<female> :name "Female" :win-title "Queen")
	   (variant.genders variant) :test #'eql :key #'gender.symbol)


;;  (assert (eq nil (variant.legal-effects var-obj)))
;;  (setf (variant.legal-effects var-obj) '(:quaff :read :eat :create :add-magic :use)) ;; make :use a meta-effect?

  (flet ((help-path (file)
	   (concatenate 'string *engine-source-dir* "docs/help/" file)))
  

    (register-help-topic& variant
			  (make-help-topic :id "keys" :key #\k
					   :name "Show commands/keys"
					   :data (help-path "keys.txt")))
    
    (register-help-topic& variant
			  (make-help-topic :id "general" :key #\g
					   :name "General information"
					   :data (help-path "general.txt")))

    (register-help-topic& variant
			  (make-help-topic :id "dungeon" :key #\d
					   :name "Simple information about the dungeons"
					   :data (help-path "dungeon.txt")))

    (register-help-topic& variant
			  (make-help-topic :id "birth" :key #\b
					   :name "Information about creating a character"
					   :data (help-path "birth.txt")))

    (register-help-topic& variant
			  (make-help-topic :id "playing" :key #\p
					   :name "Tips and hints on how to play langband"
					   :data (help-path "playing.txt")))

    (register-help-topic& variant
			  (make-help-topic :id "thanks" :key #\t
					   :name "Who has helped make Langband possible"
					   :data (help-path "THANKS")))
    (register-help-topic& variant
			  (make-help-topic :id "version" :key #\v
					   :name "Show version information"
					   :data (help-path "version.txt"))))
 

  
  (van/register-levels! variant)

  (van-init-skill-system variant)
  
  (let ((*load-verbose* nil))
    (load-variant-data& variant "defines")
    (load-variant-data& variant "sound")
    (load-variant-data& variant "stats")
    (load-variant-data& variant "flavours")
    (load-variant-data& variant "traps")

    (load-variant-data& variant "settings")

    (load-variant-data& variant "effects")
    (load-variant-data& variant "spells")
    (load-variant-data& variant "races")
    (load-variant-data& variant "classes")
    ;; need races
    (load-variant-data& variant "stores")
    (load-variant-data& variant "rooms")

    (load-variant-data& variant "combat")
    (load-variant-data& variant "print")
    
    (load-variant-data& variant "keys")
    (load-variant-data& variant "wizard")
    )

  ;; we ensure that any elements in variant are sorted
  (setf (variant.elements variant)
	(sort (variant.elements variant) #'< :key #'element.number))
  
  (van-init-equipment-values variant)

  ;; check stuff
  #-langband-release
  (loop for i being the hash-values of (variant.spells variant)
	do (ok-object? i))
  )

(defun van/register-levels! (var-obj)
  "registering the levels this variant will use."
  
  (register-level! var-obj "level"
		   :object-filter
		   #'(lambda (var-obj obj)
		       (let* ((table (get-otype-table var-obj "level"))
			      (id (slot-value obj 'id))
			      (obj-table (gobj-table.obj-table table)))
			 (multiple-value-bind (val f-p)
			     (gethash id obj-table)
			   (declare (ignore val))
			   (if f-p
			     (error "Object-id ~s already exists in system, obviously not a unique id."
				    id)
			     (setf (gethash id obj-table) obj))))

			 t)
		   :ego-filter
		   #'(lambda (var-obj obj)
		       ;;(warn "ego filter for ~s" obj)
		       (let* ((table (get-named-gameobj-table var-obj "level" 'ego-items-by-level))
			      (id (slot-value obj 'id))
			      (obj-table (gobj-table.obj-table table)))
			 (multiple-value-bind (val f-p)
			     (gethash id obj-table)
			   (declare (ignore val))
			   (if f-p
			     (error "Ego-id ~s already exists in system, obviously not a unique id."
				    id)
			     (setf (gethash id obj-table) obj))))

			 t))

  (register-level! var-obj "random-level"
		   :monster-filter
		   #'(lambda (var-obj obj)
		       ;; all below 0
		       (when (> (slot-value obj 'power-lvl) 0)
			 (let* ((which-lvl "random-level")
				(table (get-mtype-table var-obj which-lvl))
				(id (slot-value obj 'id))
				(mon-table (gobj-table.obj-table table)))
			   (multiple-value-bind (val f-p)
			       (gethash id mon-table)
			     (declare (ignore val))
			     (if f-p
				 (error "Monster-id ~s already exist for ~s, not unique id."
					id which-lvl)
				 (setf (gethash id mon-table) obj))))
			 t)))

  (register-level! var-obj "town-level"
		   :monster-filter
		   #'(lambda (var-obj obj)
		       ;; all equal to 0
		       (when (= (slot-value obj 'power-lvl) 0)
			 (let* ((which-lvl "town-level")
				(table (get-mtype-table var-obj which-lvl))
				(id (slot-value obj 'id))
				(mon-table (gobj-table.obj-table table)))
			   (multiple-value-bind (val f-p)
			       (gethash id mon-table)
			     (declare (ignore val))
			     (if f-p
				 (error "Monster-id ~s already exist for ~s, not unique id."
					id which-lvl)
				 (setf (gethash id mon-table) obj))))
			 t)))

  )

(defmethod equip-character! ((variant vanilla-variant) player settings)

  (call-next-method variant player settings)

  (let* ((backpack (get-creature-inventory player))
	 (inventory (aobj.contains backpack)))
    
    (dolist (i '("food-ration" "torch"))
      (let ((obj (create-aobj-from-id i :amount (rand-range 3 7))))
	(item-table-add! inventory obj)))
    
    t))
  

(defmethod activate-object :after ((var-obj vanilla-variant) &key)
  "Does post-initialisation of the game with variant tweaking."

;;  (declare (ignore var-obj))
  ;; YES, this is a hack
;;  (warn "Post-init of vanilla variant..")

  (setf (get-setting var-obj :basic-frame-printing)
	(get-settings-obj "vanilla-basic-frame"))
  
  (setf (get-setting var-obj :random-level)
	(get-settings-obj "vanilla-dungeon-settings"))
  
  (cond ((eq (get-system-type) 'sdl)
	 (setf (get-setting var-obj :birth)
	       (get-settings-obj "sdl-vanilla-birth-settings"))

	 (setf (get-setting var-obj :char-display)
	       (get-settings-obj "sdl-vanilla-chardisplay"))

	 (setf (get-setting var-obj :resists-display)
	       (get-settings-obj "sdl-vanilla-resist")))
	
	;; otherwise go for basic stuff
	(t
	 (setf (get-setting var-obj :birth)
	       (get-settings-obj "vanilla-birth-settings"))

	 (setf (get-setting var-obj :char-display)
	       (get-settings-obj "chardisplay-settings"))

	 (setf (get-setting var-obj :resists-display)
	       (get-settings-obj "resistdisplay-settings"))))
  
  
  ;; register level-constructors
  (register-level-builder! "random-level"
			   (get-late-bind-function 'org.langband.engine
						   'make-random-level-obj))
  
  (register-level-builder! "town-level"
			   (get-late-bind-function 'org.langband.vanilla
						   'van-create-bare-town-level-obj))
  
  )

    
(defun update-gobj-table! (variant key o-table alloc-table-creator)
  "Tries to make an allocation table from a table."
  (declare (ignore key))
;;  (warn "updating on ~a ~a" key o-table)
  
  (let ((okind-table (gobj-table.obj-table o-table)))
    
    (setf (gobj-table.obj-table-by-lvl o-table)
	  (convert-obj okind-table :vector :sort-table-p t
		       :sorted-by-key #'(lambda (x)
					  ;;(slot-value x 'depth)
					  (slot-value x 'power-lvl)
					  ;;(caar (slot-value x 'locations))
					  )))
    
    (setf (gobj-table.alloc-table o-table)
	  (funcall alloc-table-creator variant (gobj-table.obj-table-by-lvl o-table)))
    ))



(defmethod initialise-monsters& ((variant vanilla-variant) &key (file "monsters"))

  (unless file
    (error "No file specified for monster-init."))
  
  (when file
    (let ((*load-verbose* nil))
      (load-variant-data& variant file)))
    
  ;; initialise all tables
  (let ((mon-tables (variant.monsters-by-level variant)))
    (maphash #'(lambda (key obj)
		 (update-gobj-table! variant key obj
			#'create-alloc-table-monsters))
	     mon-tables)))


(defmethod initialise-floors& ((variant vanilla-variant) &key (file "floors"))

  (unless file
    (error "No file specified for floor-init."))

  (when file
    (let ((*load-verbose* nil))
      (load-variant-data& variant file))))


(defmethod initialise-objects& ((variant vanilla-variant) &key (file "objects"))

  (unless file
    (error "No file specified for floor-init."))
  
  (when file
    (let ((*load-verbose* nil))
      (load-variant-data& variant file)))

  (let ((object-tables (variant.objects-by-level variant)))
    
    (maphash #'(lambda (key obj)
		 (update-gobj-table! variant key obj
				     #'create-alloc-table-objects))
	     object-tables)))


(defun initialise-ego-items& (variant &key (file "ego-items"))

  (unless file
    (error "No file specified for ego-init."))

  (when file
    (let ((*load-verbose* nil))
      (load-variant-data& variant file)))
    
  ;; initialise all tables
  (let ((object-tables (variant.ego-items-by-level variant)))
    (maphash #'(lambda (key obj)
		 (update-gobj-table! variant key obj
			#'create-basic-allocation-table))
	     object-tables)))



;; The real McCoy
(defmethod activate-object ((var-obj vanilla-variant) &key)

  ;; the last init-call on each fixes all tables
  (let ((*load-verbose* nil))
    (load-variant-data&  var-obj "objects")
    (load-variant-data&  var-obj "food")
    (load-variant-data&  var-obj "armour")
    (load-variant-data&  var-obj "weapons")
    (load-variant-data&  var-obj "potions")
    (load-variant-data&  var-obj "rings")
    (load-variant-data&  var-obj "neckwear")
    (load-variant-data&  var-obj "scrolls")
    (load-variant-data&  var-obj "sticks")
    (load-variant-data&  var-obj "books")
    (initialise-objects& var-obj :file "gold")
    
    (load-variant-data& var-obj "monsters")
    (load-variant-data& var-obj "town-monsters")
    (initialise-monsters& var-obj :file "uniques")
    
    (initialise-floors& var-obj :file "floors")
    (initialise-ego-items& var-obj :file "ego-items")
    )
    
  
  ;; after all objects are in
  ;;  (init-flavours& (variant.flavour-types var-obj))

  (loop for x being the hash-values of (variant.flavour-types var-obj)
	do
	(unless (flavour-type.generator-fn x) ;; no point if generated
	  ;; turn into array, shuffle and put back
	  (let ((an-arr (coerce (flavour-type.unused-flavours x) 'vector)))
	    (shuffle-array! an-arr (length an-arr))
	    (setf (flavour-type.unused-flavours x) (coerce an-arr 'list))
	    t)))

  ;; ensure that we have a legal gold-table
  (when (eq (variant.gold-table var-obj) nil)
    (let* ((obj-kinds (loop for x being the hash-values of (variant.objects var-obj) ;; hackish
			    when (typep x 'object-kind/money)
			    collecting x))
	   (obj-len (length obj-kinds))
	   (gold-table (make-array obj-len)))

      ;; sort it
      (setf obj-kinds (sort obj-kinds #'< :key #'object.numeric-id))
      
      (loop for i from 0
	    for obj in obj-kinds
	    do
	    (setf (aref gold-table i) obj))
      
      (setf (variant.gold-table var-obj) gold-table)))

  ;; check every object
  #-langband-release
  (loop for x being the hash-values of (variant.objects var-obj)
	do (ok-object? x))
  
  var-obj)


(defun van-init-equipment-values (var-obj)
  "Initialises values dealing with the equipment (sorting, worn slots)."

  (let ((equip-order '(
		       (eq.weapon   "Wielding"      active-object/melee-weapon)
		       (eq.bow      "Shooting"      active-object/missile-weapon)
		       (eq.l-ring   "On left hand"  active-object/ring)
		       (eq.r-ring   "On right hand" active-object/ring)
		       (eq.neck     "Around neck"   active-object/neckwear)
		       (eq.light    "Light source"  active-object/light-source)
		       (eq.armour   "On body"       active-object/body-armour)
		       (eq.cloak    "About body"    active-object/cloak)
		       (eq.shield   "On arm"        active-object/shield)
		       (eq.head     "On head"       active-object/headgear)
		       (eq.glove    "On hands"      active-object/gloves)
		       (eq.feet     "On feet"       active-object/boots)
		       (eq.backpack "On back"       active-object/container t)
		       )))
    
    (register-slot-order& var-obj equip-order))
  )


(defun van-init-skill-system (var-obj)
  "Tries to init all that deals with the skill-system."

  ;; we more or less assume these to be complete as we use
  ;; the CDR-values as a list of slots in several places
  ;; in the code
  (register-skill-translation& var-obj
			       '((<disarming> . disarming)
				 (<device> . device)
				 (<saving-throw> . saving-throw)
				 (<stealth> . stealth)
				 (<search> . searching)
				 (<perception> . perception)
				 (<fighting> . fighting)
				 (<shooting> . shooting))))


;;; the rest of the file has a lot of odd functions

(defun get-stat-row (data val)
  (dolist (j data)
    (cond ((consp j) 
	   (cond ((= val (car j))
		  (return-from get-stat-row j))
		 ((and (integerp (cadr j)) (<= val (cadr j)))
		  (return-from get-stat-row j))))
	  ((stat-field-p j)
	   (let ((lower (stat-field-lower j))
		 (upper (stat-field-upper j)))
	     
	     (cond ((= val lower)
		    (return-from get-stat-row j))
		   ((and (integerp upper) (<= val upper))
		    (return-from get-stat-row j)))))))
  (error "Fell through GET-ROW with ~a val in ~s" val data))

(defun get-stat-info (stat-obj stat-value info-key)
  (let ((the-row (get-stat-row (stat.fields stat-obj) stat-value)))
    (cdr (assoc info-key (stat-field-data the-row)))))

(defun get-stat-info-value (variant player stat info-key)
  (let* ((stats (variant.stats variant))
	 (stat (cond ((symbolp stat)
		      (find stat stats :key #'stat.symbol))
		     (t
		      (error "only syms support for get-stat-info-value"))))
	 (value (svref (player.active-stats player) (stat.number stat))))
    (get-stat-info stat value info-key)))

;;(trace get-stat-info-value)

(defun %van-get-res-level (variant player elm)
  (let ((num (if (integerp elm)
		 elm
		 (get-element-number variant elm))))
    (aref (get-resistance-table player) num)))

(defmethod deliver-elemental-damage! ((variant vanilla-variant) source (target player) element damage)

  ;; skip immunity, add later
  (unless (<= damage 0)
    (let ((percentage (cond ((>= damage 60) 3)
			    ((>= damage 30) 2)
			    (t 1)))
	  (res-level (%van-get-res-level variant target element)))
      (declare (ignorable percentage)) ;; fix!
      (cond ((>= res-level +element-immunity+)
	     (setf damage 0))
	    ((or (= res-level +element-calculated-resistance+)
		 (= res-level +element-temporary-resistance+))
	     (setf damage (int-/ (+ 2 damage) 3)))
	    ((= res-level #.(+ +element-calculated-resistance+ +element-temporary-resistance+))
	     ;; do it twice since we double resist
	     (setf damage (int-/ (+ 2 damage) 3))
	     (setf damage (int-/ (+ 2 damage) 3))))
      (when (plusp damage)
	(deliver-damage! variant source target damage))
      ;; add equipment dmg
      t)))

(defun define-variant-graphics (foo images)
  "first argument ignored, images should be a list of image-specs"
  (declare (ignore foo))
  (let ((var-obj *variant*))
    (loop for i from 0
	  for x in images
	  do
	  (load-image-spec& var-obj i x))))
