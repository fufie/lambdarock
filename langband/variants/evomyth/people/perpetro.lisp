(in-package :org.langband.evomyth)

(define-conversation (player npc)
    (:id "captain-perpetro")
  
  (:skip-if (not (flag "met-perpetro"))
	    (:node (:text "Who are you?")
		   (:option (:text "I am Imperial Agent ~A, and General Junifer told me to talk to you."
				   (player.name player))
			    (:test (done-quest? player "deliver-letter-to-junifer"))
			    (:perform (set-flag "work-for-perpetro")
				      (set-flag "met-perpetro"))
			    (:dest "captain-perpetro"))
		   
		   (:option (:text "I am ~A, and I want information about Mont Renuo." (player.name player))
			    (:perform (set-flag "met-perpetro"))
			    (:dest "captain-perpetro"))))



  (:text "How can I help you?")

  (:option (:test (and (done-quest? player "deliver-letter-to-junifer") (not (flag "work-for-perpetro"))))
	   (:text "I am Imperial Agent ~A, and General Junifer told me to talk to you." (player.name player))
	   (:perform (set-flag "work-for-perpetro"))
	   (:dest "captain-perpetro"))
	   
  (:option (:test (and (flag "work-for-perpetro")
		       (not (doing-quest? player "buy-dress-for-junifer"))
		       (not (done-quest? player "buy-dress-for-junifer"))))
	   (:text "General Junifer told me you had a job your soldiers were unable to do.")
	   (:node (:text "Oh yes, there might be something you can do for the Empire.  We need you to travel 
to the southwest to the elven village, and meet with an elven tailour, Fossgard.  He is a sneaky bastard and 
hates the Empire, but he is a whiz with clothes and the general loves his dresses.  Buy a green dress of the 
finest silk for the General.  25 florentins to cover your expenses, and make sure you get high quality.")
		  (:perform (set-flag "buying-dress-for-junifer")
			    (modify-gold! player 25)
			    (let ((quest (find-quest *variant* "buy-dress-for-junifer")))
			      (init-quest *variant* quest npc player)))
		  (:quit-option "I will do that.")))
  
  (:option (:test (and (doing-quest? player "buy-dress-for-junifer") (has-object? player "green-silk-dress")))
	   (:text "I got hold of the green silk dress, though it cost me 50 florentins.")
	   (:node (:text "Then you were robbed by that pointy-eared bastard.  Hand over the dress.")
		  (:option (:text "<hand over the dress, but look very unhappy>")
			   (:perform (let ((dress-quest (find-quest *variant* "buy-dress-for-junifer")))
				       (finish-quest *variant* dress-quest player)))
			   (:dest "captain-perpetro"))
		  (:quit-option "I'll keep the dress until you pay the remaining 25 florentins.")))
  
  (:option (:test (and (doing-quest? player "buy-dress-for-junifer") (has-object? player "green-silk-dress")))
	   (:text "I had no problems getting the green silk dress from the elf.")
	   (:perform (let ((dress-quest (find-quest *variant* "buy-dress-for-junifer")))
		       (finish-quest *variant* dress-quest player)))
	   (:dest "captain-perpetro"))

  (:option (:test (doing-quest? player "tepesco-quest-avi"))
	   (:text "I'm witht the atrocitan consulate, and we need more export forms for
elven leatherwork, can you help?")
	   (:node (:text "Of course, here you go.")
		  (:perform (add-to-inventory player (get-new-object "export-forms")))
		  (:dest :back "Thank you Captain.")))
  
  (:include "generic-area-info")
    
  (:quit-option "Thanks for your help Captain."))
