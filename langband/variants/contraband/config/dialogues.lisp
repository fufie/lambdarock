;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/dialogues.lisp - to kill nasty creeps
Copyright (c) 2003 - Stig Erik Sandoe

|#

(in-package :org.langband.contraband)

(define-conversation (player npc)
    (:id "generic-area-info")
  (:text "foo")
  (:option (:text "Who is the Mereo in Mont Renuo?")
	   (:node (:text "The Imperial Mereo of Mont Renuo, or Lambda Rock as the locals call it, is General Junifer. 
She is also high commander of the Fourth and Fifth Imperial Legions.  With the high activity on the border now, she 
is very busy.")
		  (:dest :back "I see.")))

  (:option (:text "Who is the Mereo in Bartertown?")
	   (:node (:text "The Imperial Mereo of Bartertown, is Ulydes.  You can find him in a mansion at 
the south of the town, or at the Bartertown Merchant Guild.")
		  (:dest :back "I see.")))
  )



(define-conversation (player npc)
    (:id "copian-guard")
  (:include "generic-area-info")
  (:text "Whaddya want?")
  (:quit-option "Please excuse me, I have another errand."))

(define-conversation (player npc)
    (:id "copian-customs-officer")
  (:include "generic-area-info")
  (:text "Whaddya want?")
  (:option (:test (has-object? player "export-forms"))
	   (:text "I have some export forms, please have a look.")
	   (:node (:text "Well, these forms are not filled out, and I can't sign
blank export forms.")
		  (:dest :back "I see.")))


  (:option (:test (has-object? player "filled-out-export-forms"))
	   (:text "Please have a look at these export-forms, sir.")
	   (:node (:text "These look all right, but taxes have not been paid.  You need to pay
export tax of 10 florentins for such a large shipment, and 3 florentins for processing,
3 florentins for use of the bridge and summed up that's 20 florentins all in all.")
		  
		  (:option (:test (has-gold>= player 16))
			   (:text "Here's 16 florentins.")
			   (:cond ((attitude>= npc +attitude-friendly+)
				   (:perform (modify-gold! player 16)
					     (remove-from-inventory quest-taker '(object "filled-out-export-forms"))
					     (add-to-inventory player (get-new-object "signed-export-forms"))
					     (ask-for-update! player '[bonuses]))
				   (:text "Ok, fair enough, since it's you.")
				   (:dest :back "Good doing business with you."))
				  (:otherwise
				   (:text "I think I mentioned 20 florentins.")
				   (:dest :back "Your math-skills suck."))))
		  (:option (:test (has-gold>= player 20))
			   (:text "Here's 20 florentins.")
			   (:perform (modify-gold! player 20)
				     (adjust-attitude! npc +attchange-pleased+)
				     (remove-from-inventory player '(object "filled-out-export-forms"))
				     (add-to-inventory player (get-new-object "signed-export-forms"))
				     (ask-for-update! player '[bonuses]))
			   (:node (:text "Thanks buddy.")
				  (:dest :quit "We'll do business again.")))
		  (:option (:test (not (has-gold>= player 16)))
			   (:text "I'll return with the money later.")
			   (:node (:text "Good idea")
				  (:dest :quit "Bye")))))
				  
  (:quit-option "Please excuse me, I have another errand."))
