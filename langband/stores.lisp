;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: stores.lisp - code which deals with stores and their owners
Copyright (c) 2000-2004 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defun is-store? (obj)
  "Returns T if the object is a store."
  (typep obj 'store))

;;; Current implementation ignores haggling, selling-season, buying-season, etc

(defmethod find-owner-for-house (level (house store)
				       &key
				       (var-obj *variant*)
				       (selection :random))
  "Tries to find an owner for the given house.  The owner is returned."
  (declare (ignore level))
  
  (let ((poss-owners (store.possible-owners house))
	(the-owner nil))
    
    (unless poss-owners
      (warn "Unable to find any possible owners for store ~a" house)
      (return-from find-owner-for-house nil))

    (ecase selection
      (:random
       (setf the-owner (rand-elm poss-owners))))
    
    (assert (not (eq the-owner nil)))

    (get-owner the-owner var-obj)
    ))


(defun add-owner-to-store-type! (owner store-type-id
				 &optional (var-obj *variant*))
  "The OWNER argument should be an owner, the STORE-TYPE-ID
should be an exisiting id."
  
  (let* ((v-obj var-obj)
	 (store-type (get-house store-type-id v-obj)))
    
    (if (and store-type (typep store-type 'store))
	(pushnew (owner.id owner) (store.possible-owners store-type))
	(warn "Unable to find store-type ~a" store-type-id))))
	 
(defun make-store-sales-list (variant store-id sale-list)
  "Returns a list of sale-priorities, given a sale-spec."
  (let ((ret-list '()))
    (dolist (i sale-list)
      (cond ((and (consp i) (eq (car i) 'obj))
	     (destructuring-bind (dummy &key id type (weight 1))
		 i
	       (declare (ignore dummy))
	       (cond (id
		      (let ((k (get-object-kind variant id)))
			(if k
			    (dotimes (j weight)
			      (push k ret-list))
			    (warn "Unable to find kind for obj-id ~s" id))))
		     (type
		      (warn "Type-spec for store-objs not implemented."))
		     (t
		      (warn "Neither type or id is mentioned for obj-spec ~s for store ~s" i store-id)))
	       ))
	    (t
	     (warn "Unknown format for spec ~s for store ~s" i store-id))))
    
    ret-list))


(defun define-store (id &key (type 'store) name number
		     (sells nil) (buys nil) (owner :random) (no-items nil))
  "Creates a store object and adds it to the appropriate table(s)."
;;  (declare (ignore args))
  (let ((var-obj *variant*)
	(store (make-instance type :id id :name name :number number :owner owner)))
    
    (when (and (eq type 'store) sells)
      (setf (store.sells store) sells))
    
    (when (and (eq type 'store) buys)
      (setf (store.will-buy store) buys))
    
    ;; hackish
    (unless no-items
      (setf (house.items store) (make-container (store.item-limit store) 'items-in-store)))

    (establish-house& var-obj store)

    (when (and number (numberp number))
      ;; add to numbered position
      (establish-house& var-obj store :house-key number))
    
    store))

(defun define-store-owner (&key store-type id name purse max-greed
			   min-greed haggle-num tolerance race
			   picture special-owner)
  "creates an owner and adds him or her to appropriate tables"

;;  (warn "Looking for ~s in ~s" race (get-races-as-a-list variant))
  (let* ((race-obj (if (and race (or (symbolp race)
				     (stringp race)))
		       (get-char-race race)
		       race))
	 (owner (make-instance 'store-owner :id id :name name
			      :purse purse :max-greed max-greed
			      :min-greed min-greed :haggle-num haggle-num
			      :picture picture
			      :tolerance tolerance :race race-obj))
	 (var-obj *variant*))

    (when (and picture (not (image-exists? picture)))
      (warn "Unable to find picture ~s for store-owner ~s." picture id)
      (setf (owner.picture owner) nil))

    ;; we add it to the owner-table
    (establish-owner& var-obj owner)
    
    ;; we just want generic owners to the relation table
    (unless special-owner
      (add-owner-to-store-type! owner store-type var-obj))

    owner))

(defmethod get-price ((object active-object) (store store))
  "Returns an appropriate shop-price for an object."
  (let* ((okind (aobj.kind object))
	 (default-price (object.cost okind)))

    ;; skip storekeeper, charisma, ...
    (if (and (numberp default-price) (>= default-price 0))
	(floor (* 1.5 default-price) 1)
	0)))


(defmethod get-offer ((object active-object) (store store))
  "Returns a shopkeeper's offer for a given object."
  (int-/ (get-price object store) 2)) ;; decent value, eh?

(defun %print-shop-message! (str &key (attr +term-yellow+))
  (warn "msg: ~s" str)
  (print-message! str :attr attr)
  (flush-messages! :forced nil)
  ;;(paint-window +query-frame+)
  ;;(flush-window +query-frame+)
  )


(defun select-item-from-store (store low top)
  "Reads input from keyboard for selecting an item from a store."
  (declare (ignore store))
  (let ((the-char (read-one-character)))
    (cond ((eql the-char #\Escape)
	   nil)
	  ((characterp the-char)
	   (let ((char-num (a2i the-char)))
	     (cond ((and (<= low char-num)
			 (>= top char-num))
		    char-num)
		   (t
		    (%print-shop-message! (format nil "Illegal selection '~a'!" the-char) :attr +term-violet+)
		    nil))))
	  (t
	   #-cmu
	   (put-coloured-line! +term-white+ "Odd return-value!" 0 0)
	   nil))))
		 

(defun %store-buy-item (player level store)
  (declare (ignore level))
  (block buying
    (let* ((var-obj *variant*)
	   (items (store.items store))
	   (item-len (items.cur-size items)))

      (output-string! +query-frame+ 0 0 +term-yellow+
		      (format nil "(Items ~a-~a, ESC to exit) Which item are you interested in?"
				 (i2a 0) (i2a (1- item-len))))
      #||
      (put-coloured-str! +term-yellow+
			 (format nil "(Items ~a-~a, ESC to exit) Which item are you interested in?"
				 (i2a 0) (i2a (1- item-len)))
			 0 0)
      ||#
      (let ((selected (select-item-from-store store 0 (1- item-len))))
	(when (and selected (numberp selected))
	  (let* ((retval nil)
		 (act-obj (item-table-find items selected))
		 (the-price (get-price act-obj store))
		 (backpack (aobj.contains (get-creature-inventory player))))
	    ;;(warn "Buying ~s for ~s" act-obj the-price)
	    (unless (<= the-price (player.gold player))
	      (%print-shop-message! "You cannot afford that item!" :attr +term-violet+)
	      (return-from buying nil))

	    (unless (item-table-more-room? backpack)
	      (%print-shop-message! "No room in backpack!" :attr +term-violet+)
	      (return-from buying nil))

	    (cond ((= 1 (aobj.number act-obj))
		   (possible-identify! player act-obj) ;; fix?
		   (item-table-add! backpack act-obj)
		   (item-table-remove! items act-obj)
		   (setf retval act-obj))
		  
		  ((> (aobj.number act-obj) 1)
		   (let ((new-obj (copy-active-object var-obj act-obj)))
		     (decf (aobj.number act-obj))
		     (setf (aobj.number new-obj) 1)
		     (item-table-add! backpack new-obj)
		     (possible-identify! player new-obj)
		     (setf retval new-obj))
		   ))
	    
	    (%print-shop-message! (with-output-to-string (s)
				    (format s "You bought ")
				    (write-obj-description var-obj retval s)
				    (format s " for ~a gold." the-price)))

	    
	    ;; add identify for it
	    (decf (player.gold player) the-price)
	    (ask-for-redraw! player '[gold])
	    
	    
	    retval)
	  )))))

(defun %store-sell-item (player level store)
  
  (let ((dungeon (level.dungeon level)))
    (block selling

      (when-bind (selection (select-item dungeon player '(:backpack :equip)
					 :prompt "Sell which item? "
					 :where :backpack
					 :selection-function #'(lambda (table idx obj)
								 (declare (ignore table idx))
								 (store-buys-item? obj store))
					 :printer-function #'(lambda (obj key x y)
							       (let ((attr (get-text-colour obj))
								     (desc (with-output-to-string (s)
									     (write-obj-description *variant* obj s))))
								 
								 (put-coloured-line! +term-white+ "" (- x 2) y)
								 (put-coloured-str! +term-white+
										    (format nil "~a) " (i2a key)) x y)
								 
								 (put-coloured-str! +term-white+
										    (format nil "[value: ~6d]"
											    (get-offer obj store))
										    (+ x 4) y)

								 (put-coloured-str! attr desc (+ x 21) y)
								 ))))
      

	(let* ((the-table (get-item-table dungeon player (car selection)))
	       (removed-obj (item-table-remove! the-table (cdr selection) :only-single-items t)))

	  (when removed-obj

	    ;; does the shop want to buy that kind of object?
	    (let ((might-buy (store-buys-item? removed-obj store)))
	      (unless might-buy
		(%print-shop-message! "- I don't buy such items." :attr +term-violet+)
		;; put it back.
		(item-table-add! the-table removed-obj)
		(return-from selling nil)))

	    ;; does the shop have any room?
	    (let ((shop-items (store.items store)))
	      (unless (item-table-more-room? shop-items)
		(%print-shop-message! "- I have no more room in the store." :attr +term-violet+)
		;; put it back.
		(item-table-add! the-table removed-obj)
		(return-from selling nil)))
	    
	    ;; can we get a decent price?
	    (let ((price (get-offer removed-obj store))
		  (var-obj *variant*))

	      (cond ((plusp price)
		     (%print-shop-message! (with-output-to-string (s)
					     (format s "You sold ")
					     (write-obj-description var-obj removed-obj s)
					     (format s " for ~a gold." price)))

		     ;;(%print-shop-message! "You sold- It's a deal.")
		     ;; add to shop
		     (item-table-add! (store.items store) removed-obj)
		     (incf (player.gold player) price)
		     (return-from selling t))
		    ;; no decent price
		    (t
		     (%print-shop-message! "- That item is worthless, I don't want it."
					   :attr +term-violet+)
		     (item-table-add! the-table removed-obj)
		     (return-from selling nil))))

	    nil))
	
	))))


(defmethod display-house ((player player) (store store) &key (offset 0))
  "Tries to display the store to the screen."
  (declare (ignore offset))
  
  (let ((store-name (store.name store))
	(store-limit 50000)
	(the-owner (house.owner store))
	(owner-name "Bob")
	(owner-race "Human"))

    (when (and the-owner (typep the-owner 'store-owner))
      (setf owner-name (owner.name the-owner))
      (let ((the-race (owner.race the-owner)))
	
	(when (and the-race (symbolp the-race))
	  (setf the-race (get-char-race the-race)))
	
	(when (and the-race (typep the-race 'character-race))
	  (setf owner-race (race.name the-race))))
      
      (let ((poss-limit (owner.purse the-owner)))
	(when (and poss-limit (plusp poss-limit))
	  (setf store-limit poss-limit))))

    (clear-window *cur-win*) ;; hack

    ;; big empty space when no graphics
    (when (use-images?)
      ;; hackish, improve later

      (let ((owner-picture (owner.picture the-owner)))
	(when (and owner-picture (stringp owner-picture))

	  (paint-gfx-image& owner-picture 1 1)
	  
;;      (load-scaled-image& "./graphics/people/grim-elf.png" -1 6 5)
;;      (paint-image& "./graphics/people/grim-elf.png" 1 1))

	  )))
	  
    (let ((left-col 20)
          (desc-line 7)
	  (last-line (get-last-window-line *cur-win*)))

      
      (put-coloured-str! +term-yellow+ (format nil "~a" store-name) left-col 1)
      (put-coloured-str! +term-white+ (format nil "Owned by: ~a (~a)" owner-name owner-race) left-col 2)
      (put-coloured-str! +term-white+ (format nil "Max purchase value: ~a AU" store-limit) left-col 3)

      (put-coloured-str! +term-white+ "Item Description" 3 desc-line)
      (put-coloured-str! +term-white+ "Weight" 60 desc-line)
      (put-coloured-str! +term-white+ "Price" 72 desc-line)

      (put-coloured-str! +term-white+ "Gold Remaining:" 53 (- last-line 3))
      
      (put-coloured-str! +term-white+
			 (format nil "~9d" (player.gold player))
			 68 (- last-line 3))

      ;; pass last-line as info here
      (item-table-print (house.items store) :store store :start-y (1+ desc-line))


	#||
      (put-coloured-str! +term-yellow+ "ESC" 1 (1- last-line))
      (put-coloured-str! +term-white+ ") Exit from building." 4 (1- last-line))
      (put-coloured-str! +term-yellow+ "g" 31 (1- last-line))
      (put-coloured-str! +term-white+ ") Get/purchase item." 32  (1- last-line))
      (put-coloured-str! +term-yellow+ "d" 31 last-line)
      (put-coloured-str! +term-white+ ") Drop/sell item." 32 last-line)
	||#

      (put-coloured-str! +term-white+ "You may: " 0 (- last-line 2)))

   
    t))

(defun %get-shop-commands (shop win)
  (declare (ignore shop))
  (let ((alts '())
	(text nil)
	(start-col 1)
	(start-row 1)
	(last-line (get-last-window-line win))
	(text-colour +term-dark+)
	(button-colour :yellow))
	
    (setf text "ESC) Exit from building"
	  start-col 1
	  start-row (1- last-line))
    
    
    (push (make-selectable-ui-object #\Escape start-col start-row
				     (+ start-col (length text)) start-row
				     :text text :text-colour text-colour
				     :button-colour button-colour)
	  alts)
    
    (setf text "g) Get/purchase item"
	  start-col 31
	  start-row (1- last-line))
    
    (push (make-selectable-ui-object #\g start-col start-row
				     (+ start-col (length text)) start-row
				     :text text :text-colour text-colour
				     :button-colour button-colour)
	  alts)
    
    (setf text "d) Drop/sell item   "
	  start-col 31
	  start-row last-line)
    
    (push (make-selectable-ui-object #\d start-col start-row
				     (+ start-col (length text)) start-row
				     :text text :text-colour text-colour
				     :button-colour button-colour)
	  alts)
    
    (nreverse alts)))



(defun %shop-input-loop (player level store &key shop-commands)
  
  (block input-loop
    (loop
     ;; this should vary depending on size and stuff
     ;;(set-cursor-to *cur-win* :input 10 21)

     ;; more intelligent?
     (flush-window *cur-win*)
     (flush-window (get-window +message-frame+))
     (clear-window (get-window +query-frame+))

     (when (consp shop-commands)
       (dolist (i shop-commands)
	 (buttonify-selectable-ui-object *cur-win* i)))
     
     ;;(flush-window (get-window +query-frame+))
     
     (let ((val (select-displayed-alternative shop-commands) #|(read-one-character)|# ))
       ;;(flush-messages! :forced t) ;; forced
       ;;(warn "shop-loop got ~s" val) 
       (cond ((or (eql val #\g)
		  (eql val #\p))
	      (when-bind (retval (%store-buy-item player level store))
		(display-house player store)
		(update-inventory-row *variant* player)
		(put-coloured-line! +term-white+ "" 0 0)
		))
     
	     ((or (eql val #\d)
		  (eql val #\s))
	      (%store-sell-item player level store)
	      (display-house player store)
	      (update-inventory-row *variant* player)
	      (put-coloured-line! +term-white+ "" 0 0)
	      )

	    
	     ((or (eql val #\Escape)
		  (eql val #\Q))
	      (return-from input-loop t))
	    
	     (t
	      (warn "Unknown key read: ~s" val)))
     
       ;;     (put-coloured-line! +term-white+ "" 0 0)
       ))))


(defmethod visit-house (level (house store))
  "Visits the given store."

  (unless (activated? house)
    (activate-object house))

  (flush-messages! :forced t :reset-col t)
  (print-message! (format nil "You enter the ~a." (house.name house))
		  :attr +term-yellow+)
  (flush-messages! :forced t :reset-col t)
  (flush-window +query-frame+)
  
  (with-dialogue ()
    (let ((shop-commands (%get-shop-commands house *cur-win*)))
      (clear-window *cur-win*)
      (display-house *player* house :offset 0)
      (%shop-input-loop *player* level house :shop-commands shop-commands)))
  
  (clear-window +query-frame+)
  
  (flush-messages! :forced t :reset-col t)
  (print-message! (format nil "You leave the ~a." (house.name house))
		  :attr +term-yellow+)
  (flush-messages! :forced t :reset-col t))



;; hackish  create/delete/maint
(defmethod store-generate-object ((variant variant) (the-store store))
  "this is just for a regular store, not a black market"

  (when-bind (sells (store.sells the-store))
    ;;(warn "Shop ~s sells ~s" the-store sells)
    (when-bind (kind (rand-elm sells))
      (when (typep kind 'object-kind)
	(let ((aobj (create-aobj-from-kind kind :variant variant)))
	  ;; possibly add magic
	  (apply-magic! variant aobj (store.object-depth the-store) :allow-artifact nil)
	  (store-mass-produce! variant the-store aobj)

	  (when (or (is-cursed? aobj)
		    (is-broken? aobj))
	    (setf aobj nil))
	  
	  (return-from store-generate-object aobj)))))

  (warn "Fell through in obj-generation for store ~s" the-store)
    
  nil)


(defun store-delete-obj! (the-store &optional obj-key)
  "Just wipes an object from the store."
  (let* ((store-items (store.items the-store))
	 (cur-size (items.cur-size store-items))) 
    (when (plusp cur-size)
      (let ((key (if obj-key obj-key (random cur-size))))
	;;(warn "Removing ~s" key)
	(item-table-remove! store-items key)))))

(defun allocate-object-in-store! (variant the-store)
  "Tries to generate and add an object to the given store."
  (when-bind (new-obj (store-generate-object variant the-store))
    ;;(warn "Adding ~s" (object.name new-obj))
    (item-table-add! (store.items the-store) new-obj)))

(defun store-empty? (variant store)
  "Is the store devoid of objects?"
  (declare (ignore variant))
  
  (let* ((items (store.items store))
	 (objs (items.objs items))
	 (count 0))
    (loop for x across objs
	  when x
	  do (incf count))
    
    (zerop count)))
    
(defun fill-up-store! (variant store &optional (attempts 10))
  "Tries several attempts (default 10) to run maintenace on the store."
  (when (integerp attempts)
    (dotimes (j attempts)
      (store-maintenance! variant store))))


(defmethod activate-object ((obj store) &key)

  (let ((res-obj (call-next-method)))
    (unless (eq res-obj obj)
      (error "Something fu with store-obj ~a" res-obj)))
  
  (when-bind (sells (store.sells obj))
    ;; late-init basically
    (setf (store.sells obj) (make-store-sales-list *variant* (store.id obj) sells)))
    
  obj)


(defmethod store-maintenance! ((variant variant) (the-store store))
  "This method does a full maintenance of the shop, restocks and cleans as necessary."

  (let ((min (store.min-items the-store))
	(max (store.max-items the-store))
	(limit (items.max-size (store.items the-store)))
	(turnover (store.turnover the-store))
	(cur-num (items.cur-size (store.items the-store)))
	)

    ;; we have some turnover
    (decf cur-num (randint turnover))

    (when (> cur-num max) (setf cur-num max))
    (when (< cur-num min) (setf cur-num min))
    (when (minusp cur-num) (setf cur-num 0))

    (while (> (items.cur-size (store.items the-store)) cur-num)
      ;;(warn "-Compare ~s ~s" (items.cur-size (store.items the-store)) cur-num)
      (store-delete-obj! the-store))

    ;; now, let us get some new stuff in

    (setf cur-num (items.cur-size (store.items the-store)))

    (incf cur-num (randint turnover))
    
    (when (> cur-num max) (setf cur-num max))
    (when (< cur-num min) (setf cur-num min))
    (when (>= cur-num limit) (setf cur-num (1- limit)))

    (while (< (items.cur-size (store.items the-store)) cur-num)
      ;;(warn "+Compare ~s ~s" (items.cur-size (store.items the-store)) cur-num)
      (allocate-object-in-store! variant the-store))

    ;;(warn "Return")
    the-store))
 
