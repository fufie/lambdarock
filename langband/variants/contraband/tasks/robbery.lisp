(in-package :org.langband.contraband)

(defquest robbery-quest ()
  :id "robbery-quest"
  :title "Investigate a warehouse robbery"
  :desc "Investigate the robbery of a warehouse and catch the criminal."
  :steps nil)


(defun %visit-warehouse (dungeon x y)
  (let ((player *player*))
    (set-flag "last-town-px" (location-x player))
    (set-flag "last-town-py" (location-y player))
    (setf (player.leaving? player) :warehouse)
    t))

  

(defmethod init-quest ((variant contraband) (quest robbery-quest) giver taker)
  (call-next-method)
  (con/place-person "merchant-lodicus" 13 33)

  (let ((x 67) ;; warehouse door
	(y 27)
	(floor (get-floor-type "broken-town-door"))
	(evt (make-coord-event "warehouse" #'%visit-warehouse nil)))
    
    (setf (cave-floor *dungeon* x y) floor)
    (setf (get-coord-trigger *dungeon* x y) evt)

    ;; hack to simplify dev
    (setf (cave-floor *dungeon* 14 28) floor)
    (setf (get-coord-trigger *dungeon* 14 28) evt))

  quest)
