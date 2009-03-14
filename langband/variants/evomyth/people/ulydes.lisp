(in-package :org.langband.evomyth)

(define-conversation (player npc)
    (:id "mereo-ulydes")
  (:text "How can I help you?")
  (:option (:test (and (not (flag "met-ulydes"))
		       (flag "work-for-tepesco")))
	   (:text "My name is ~A, and I work for Consul Tepesco of the Kingdom of Atrocitas.
I will work for the consulate here in Bartertown, and I hope for good cooperation to further
trade between our nations." (player.name player))
	   (:node (:text "Thanks ~A, you're welcome in Bartertown.  In fact I need the
services of an educated atroctian.  When you have time, please visit me again and ask me for work."
			 (player.name player))
		  (:perform (set-flag "met-ulydes")
			    )
		  (:dest :back "I will do so, Mereo.")))
		  
	   
  (:option (:test (and (doing-quest? player "tepesco-quest-avi")
		       (not (flag "met-ulydes"))))
	   (:text "I'm with the atrocitan consul and I need forms for export of elven leatherwork.")
	   (:node (:text "I've run out of forms, please check with the Mereo in Lambda Rock.")
		  (:dest :back "Oh, thanks for your help.")))

  (:option (:test (and (doing-quest? player "tepesco-quest-avi")
		       (flag "met-ulydes")
		       (not (has-object? player "export-forms"))
		       ))
	   (:text "Dear Mereo, it's ~A from the atrocitan consulate.  We seem to've run out
of export forms for elven leatherwork and west copian cotton.  To close a deal and pay
due export-taxes, those forms would be useful." (player.name player))
	   (:node (:text "Here you go.")
		  (:perform (add-to-inventory player (get-new-object "export-forms")))
		  (:dest :back "Thank you Mereo."))
	   )
  
  (:quit-option "I will be back Mereo."))
