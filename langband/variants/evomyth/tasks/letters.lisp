(in-package :org.langband.evomyth)



;; crappy way to do things
(defquest deliver-letter-to-junifer ()
  :id "deliver-letter-to-junifer"
  :title "Deliver letter to Mereo Junifer"
  :desc "Deliver a sealed letter of introduction to Imperial Mereo Junifer in Mont Renuo."

  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
	   (call-next-method)	       
	   (add-to-inventory taker (get-new-object "sealed-letter-to-junifer"))
	   ;;(setf (quest.state quest) :active)
	   quest)
  )

(defquest deliver-letter-to-ulydes ()
  :id "deliver-letter-to-ulydes"
  :title "Deliver letter to Mereo Ulydes"
  :desc "Deliver a sealed letter of introduction to Imperial Mereo Ulydes in Bartertown."

  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
	   (call-next-method) 
	   (add-to-inventory taker (get-new-object "sealed-letter-to-ulydes"))
	   ;;(setf (quest.state quest) :active)
	   quest)
  )

(defquest deliver-letter-to-tepesco ()
  :id "deliver-letter-to-tepesco"
  :title "Deliver letter to Consul Tepesco"
  :desc "Deliver a sealed letter of introduction to Atrocitas Consul Tepesco in Bartertown."

  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
	   (call-next-method)
	   (add-to-inventory taker (get-new-object "sealed-letter-to-tepesco"))
	   ;;(setf (quest.state quest) :active)
	   quest)
  )


;; this one is a bit bigger so we'll use a full method
(defmethod finish-quest ((variant evomyth) (quest deliver-letter-to-junifer) quest-taker)

  (call-next-method)
  (let ((sealed (remove-from-inventory quest-taker '(object "sealed-letter-to-junifer"))))
    ;; remove this too
    (remove-from-inventory quest-taker '(object "opened-letter-to-junifer"))

    (cond (sealed
	   (print-message! "The Mereo's letter removed from backpack, you're given 15 florentins.")
	   (modify-gold! *player* 15)
	   (modify-xp! quest-taker 70))
	  (t
	   (print-message! "Letter to Mereo Junifer removed from inventory.")
	   (modify-xp! quest-taker 30))))


  (ask-for-update! quest-taker '[bonuses])
  
  ;;(setf (quest.state quest) :finished)
  
  quest)

;; needs tweaking and dialogue-handling
(defmethod finish-quest ((variant evomyth) (quest deliver-letter-to-ulydes) quest-taker)
  (call-next-method)
  (let ((sealed (remove-from-inventory quest-taker '(object "sealed-letter-to-ulydes"))))
    ;; remove this too
    (remove-from-inventory quest-taker '(object "opened-letter-to-ulydes"))

    (cond (sealed
	   (print-message! "The Mereo's Letter removed from backpack, you're given 5 florentins.")
	   (modify-gold! *player* 5)
	   (modify-xp! quest-taker 50))
	  (t
	   (print-message! "Letter to Mereo Ulydes removed from inventory.")
	   (modify-xp! quest-taker 30))))

  (ask-for-update! quest-taker '[bonuses])
  
  ;;(setf (quest.state quest) :finished)
  
  quest)

(defmethod finish-quest ((variant evomyth) (quest deliver-letter-to-tepesco) quest-taker)
  (call-next-method)
  (let ((sealed (remove-from-inventory quest-taker '(object "sealed-letter-to-tepesco"))))
    ;; remove this too
    (remove-from-inventory quest-taker '(object "opened-letter-to-tepesco"))

    (cond (sealed
	   ;; right thing!
	   (print-message! "The Consul's Letter removed from backpack, you're given 15 florentins.")
	   (modify-gold! *player* 15)
	   (modify-xp! quest-taker 70))

	  (t
	   (print-message! "Letter to Consul Tepesco removed from backpack.")
	   (modify-xp! quest-taker 30))))

  (ask-for-update! quest-taker '[bonuses])
  
  ;;(setf (quest.state quest) :finished)
  
  quest)


#||
;; done with easier :init now, can also use method
(defmethod init-quest ((variant evomyth) (quest deliver-letter-to-junifer) quest-giver quest-taker)
  (add-to-inventory quest-taker (get-new-object "sealed-letter-to-junifer"))
  (setf (quest.state quest) :active)
  quest)
||#

  #||
  ;; adding events
  (add-quest-event quest
		   '(on-move-to-coord 10 10) ;; when moved to 10,10 this should have effect
		   (quest-event (variant quest taker)
		     (when (has-object? taker "sealed-letter-to-junifer")
		       (finish-quest variant quest taker))))
  ||#
