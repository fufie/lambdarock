(in-package :org.langband.evomyth)

(define-conversation (player npc)
   (:id "mereo-junifer-letter")
   (:text "So you're yet another of Mangrevi's puppets, I'm glad your superiors have started to inform 
us about these things now.  Only last month we hanged a spy that had not identified himself 
correctly.  He was sneaking around our barracks.  Let me warn you now, if you're caught near military 
areas you're in so much trouble that you will beg for death.  Though, I have a few things that needs 
to be done by a sneaky little fellow like you, and it would be inappropriate to use one of my 
honourable soldiers for this.  Go tell Captain Perpetro who you are and who you work for, 
and he will inform you of your duties.")
   
   (:perform (let ((letter-quest (find-quest *variant* "deliver-letter-to-junifer")))
	       ;;(warn ">Found quest ~s" letter-quest)
	       (finish-quest *variant* letter-quest player))
	     (adjust-attitude! npc +attchange-pleased+))
   (:quit-option "Please excuse me general, I have work to do."))
  

(define-conversation (player npc)
    (:id "mereo-junifer")

  (:skip-if (not (flag "met-junifer"))
	    (:node (:text "Who are you?")
		   (:option (:text "I am ~A, and I'm here to see Mereo Junifer." (player.name player))
			    (:perform (adjust-attitude! npc +attchange-annoyed+)
				      (set-flag "met-junifer"))
			    (:dest "mereo-junifer"))
		   (:option (:text "I am ~A, and I'm here to see General Junifer." (player.name player))
			    (:perform (set-flag "met-junifer"))
			    (:dest "mereo-junifer"))))


  (:text "What is your business?")

  (:option (:test (has-object? player "sealed-letter-to-junifer"))
	   (:text "I have an urgent letter to Imperial Mereo Junifer." (player.name player))
	   (:perform (adjust-attitude! npc +attchange-annoyed+))
	   (:dest "mereo-junifer-letter"))
  
  (:option (:test (has-object? player "sealed-letter-to-junifer"))
	   (:text "I have a letter to General Junifer." (player.name player))
	   (:dest "mereo-junifer-letter"))
  
  (:option (:test (has-object? player "opened-letter-to-junifer"))
	   (:text "I have a letter to General Junifer." (player.name player))
	   (:perform (adjust-attitude! npc +attchange-offended+))
	   (:dest "mereo-junifer-letter"))

  (:option (:test (or (has-object? player "sealed-letter-to-tepesco")
		      (has-object? player "opened-letter-to-tepesco")))
	   (:text "I have an urgent letter for Consul Tepesco, do you want it?" (player.name player))
	   (:perform (adjust-attitude! npc +attchange-pleased+))
	   (:dest "consul-tepesco-letter"))

  
  (:option (:text "My name is ~A, you killed my father, prepare to die!" (player.name player))
	   (:perform (adjust-attitude! npc +attchange-offended+))
	   (:dest "mereo-junifer"))
  
  (:quit-option "Bye.")
  )
