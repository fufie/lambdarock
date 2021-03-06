(in-package :org.langband.contraband)

;; crappy way to do things
(defquest buy-dress-for-junifer ()
  :id "buy-dress-for-junifer"
  :title "Buy dress for Captain Perpetro"
  :desc "Buy a green silk dress for Captain Perpetro (to give to Mereo Junifer) in Mont Renuo."
  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
           (call-next-method)
	   (add-to-inventory taker (get-new-object "measurements-junifer"))
	   ;;(setf (quest.state quest) :active)
	   quest)
  )

;; this one is a bit bigger so we'll use a full method
(defmethod finish-quest ((variant contraband) (quest buy-dress-for-junifer) quest-taker)
  (call-next-method)
  (remove-from-inventory quest-taker '(object "measurements-junifer"))
  (remove-from-inventory quest-taker '(object "green-silk-dress"))

  (modify-xp! quest-taker 100)
  (ask-for-update! quest-taker '[bonuses])
  
  ;;(setf (quest.state quest) :finished)
  (set-flag "bought-dress-for-junifer")
  
  quest)
