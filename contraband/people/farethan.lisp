(in-package :org.langband.contraband)

(define-conversation (player npc)
    (:id "trader-farethan")
  (:text "How can I help you?")
  (:option (:test (not (flag "met-farethan")))
	   (:text "I am ~A, I'm with the atrocitan consulate. I'm here to help." (player.name player))
	   (:node (:text "Great, the customs have been troublesome lately and 
I need to get my wares back to Atrocitas as soon as possible.  A copian 
merchant is trying to steal my market for elven leatherwork in Atrocitas, so it's 
vital that my caravan moves freely.  If you can get the necessary forms for 
elven leatherwork from Mereo Ulydes I'll fill them out.")
		  (:perform (set-flag "met-farethan")
			    (advance-quest *variant* "tepesco-quest-avi" player :giver npc))
		  (:quit-option "Sure, I'll get the necessary forms.")))

  (:option (:test (has-object? player "export-forms"))
	   (:text "I got the export forms.")
	   (:node (:text "Great, I'll fill them out. [Avi fills the forms quickly.] 
There you go, please talk to a customs officer on the southern bridge.")
		  (:perform (advance-quest *variant* "tepesco-quest-avi" player :giver npc)
			    (add-to-inventory player (get-new-object "filled-out-export-forms")))
		  (:quit-option "I'll be back later.")))

  (:option (:test (and (doing-quest? player "tepesco-quest-avi-customs")
		       (has-object? player "signed-export-forms")))
	   (:text "The customs officer signed the forms.")
	   (:node (:text "Great, let's go.")
		  (:perform (advance-quest *variant* "tepesco-quest-avi" player :giver npc))
		  (:dest :quit "Let's")))

  (:option (:test (and (= (location-x npc) 80)
		       (= (location-y npc) 80)
		       (doing-quest? player "tepesco-quest-avi-transport")
		       (has-object? player "signed-export-forms")))
	   (:text "Here, I forgot to give you the signed export-forms.")
	   (:node (:text "Thank you ~A, you've been invaluable help.  Please accept this meager
gift for yourself.  I'll go back to Atrocitas now, report to the consul that everything went ok."
			 (player.name player))
		  (:perform (remove-from-inventory player '(object "signed-export-forms"))
			    (advance-quest *variant* "tepesco-quest-avi" player))
		  (:dest :quit "It's been a privilege to help you.")))
			    
  
  (:quit-option "I might be back."))
