(in-package :org.langband.evomyth)

;; crappy way to do things
(defquest tepesco-quest-avi ()
  :id "tepesco-quest-avi"
  :title "Help Avi Farethan back to Atrocitas"
  :desc "Help Avi Farethan get past customs and the bridge safely."
  :steps '("tepesco-quest-avi-meet"
	   "tepesco-quest-avi-forms"
	   "tepesco-quest-avi-customs"
	   "tepesco-quest-avi-transport"
	   "tepesco-quest-avi-return")
  )

(defquest tepesco-quest-avi-0 ()
  :id "tepesco-quest-avi-meet"
  :title "Meet Avi Farethan"
  :desc "Meet Avi Farethan about exports.")


(defquest tepesco-quest-avi-forms ()
  :id "tepesco-quest-avi-forms"
  :title "Get export forms for Avi Farethan"
  :desc "Get export forms for elven leatherwork from a Mereo."
  )

(defquest tepesco-quest-avi-2 ()
  :id "tepesco-quest-avi-customs"
  :title "Clear export with customs officers"
  :desc "Talk to customs officers and give them necessary export forms."
  )

(defquest tepesco-quest-avi-transport ()
  :id "tepesco-quest-avi-transport"
  :title "Aid caravan transport for Avi Farethan"
  :desc "Follow Avi Farethan's transport to the bridge and see him through."
  )

(defquest tepesco-quest-avi-4 ()
  :id "tepesco-quest-avi-return"
  :title "Return to the consul"
  :desc "Return to the consul and report that Avi is on his way home.")

;; this one is a bit bigger so we'll use a full method
(defmethod finish-quest ((variant evomyth) (quest tepesco-quest-avi) quest-taker)

  (call-next-method)

  (print-message! "You have completed your first task as diplomat, good work.")
  (modify-xp! quest-taker 100)
  (ask-for-update! quest-taker '[bonuses])
  
  quest)

(defmethod finish-quest ((variant evomyth) (quest tepesco-quest-avi-forms) quest-taker)
  (call-next-method)
  (remove-from-inventory quest-taker '(object "export-forms")))

(defmethod init-quest ((variant evomyth) (quest tepesco-quest-avi-transport) quest-giver quest-taker)
  (call-next-method)
  ;;(warn "QG ~s, QT ~s, Q.G ~s, Q.T ~s" quest-giver quest-taker (quest.giver quest) (quest.taker quest))
  (assert (equal (get-id quest-giver) "trader-farethan"))
  (let ((strategy (make-instance 'peaceful-mover)))
    (setf (strategy.destinations strategy) '((10 35) (10 46)
					     (28 46) (42 45)
					     (50 47) (54 55)
					     (53 66) (54 76)
					     (62 78) (71 79)))

    (setf (strategy.destinations strategy)
	  (nconc (strategy.destinations strategy)
		 (list (list 80 80 #'(lambda (p d m s)
				       (format-message! "Avi: We're safely at the bridge ~A." (player.name p))
				       (print-message! "Avi: Do you have the export forms still?"))))))
        
    (setf (amon.strategies quest-giver) (list strategy)))
  
  quest)

(defmethod finish-quest ((variant evomyth) (quest tepesco-quest-avi-transport) quest-taker)
  (call-next-method)
  ;;(warn "Finish ~s" quest)
  ;;(warn "QG ~s, QT ~s, Q.G ~s, Q.T ~s" quest-giver quest-taker (quest.giver quest) (quest.taker quest))
  (assert (equal (get-id (quest.giver quest)) "trader-farethan"))
  (let ((strategy (first (amon.strategies (quest.giver quest)))))
    (check-type strategy peaceful-mover)
    (setf (strategy.destinations strategy)
	  (nconc (strategy.destinations strategy)
		 (list (list 100 80 #'(lambda (p dungeon mon s)
					(remove-monster-from-dungeon! dungeon mon))
			     )))))
  
  (add-to-inventory *player* (get-new-object "trader-amulet"))

  quest)
