(in-package :org.langband.contraband)

(define-conversation (player npc)
   (:id "consul-tepesco-letter")
   (:text "Thank you ~A.  I'm glad the Foreign Office has sent someone, even 
if you have no practical experience.  Everyone is a bit tense and uncertain and 
we need diplomatic people to calm down the situation.  People in Bartertown are 
talking about an upcoming civil war in the Kingdom, but that is just idle talk and 
we need to calm down people.  Please familiarise yourself with Bartertown and 
report back to me later today and I might have some jobs for you." (player.name player))
   
   (:perform (set-flag "work-for-tepesco")
	     (set-flag "next-quest-avi")
	     (let ((letter-quest (find-quest *variant* "deliver-letter-to-tepesco")))
	       ;;(warn ">Found quest ~s" letter-quest)
	       (finish-quest *variant* letter-quest player)))

   (:quit-option "I will do so, please excuse me consul."))

(define-conversation (player npc)
   (:id "consul-tepesco-quest-avi")
  (:text "I have an important job for you. 
 
After the King's death the local merchant guild has tried to obstruct 
our traders, and the copian army has asked the customs officers to be thorough when checking 
all cargo that goes to and from the Kingdom.  The merchants are routinely telling 
customs officers that our traders smuggle and so our honest traders are hampered by 
overeager officers.  The only language they understand seem to be bribes.  And we can't 
live with such a situation. 
 
But we will try to do this 100% by the book and see if that can work. 
What I need you to do is to talk to Avi Farethan, he's across the road.  Help 
him with whatever he needs to get out of town and past the bridge.")
  (:perform (con/place-person "trader-farethan" 6 34)
	    (set-flag "tepesco-quest-avi")
	    (unset-flag "next-quest-avi")
	    (let ((quest (find-quest *variant* "tepesco-quest-avi")))
	      (init-quest *variant* quest npc player)))
  (:quit-option "I will talk to Avi Farethan, consul.")
  )


(define-conversation (player npc)
    (:id "consul-tepesco")
  (:text "How can I help you?")
  (:option (:test (or (has-object? player "sealed-letter-to-junifer")
		      (has-object? player "sealed-letter-to-ulydes")))
	   (:text "I have an urgent letter for a Copian Mereo, do you want it?")
	   ;;(:perform (adjust-attitude! npc +attchange-annoyed+))
	   (:dest "mereo-letter-to-tepesco"))
  
  (:option (:test (has-object? player "sealed-letter-to-tepesco"))
	   (:text "I have an urgent letter for you.")
	   (:perform (adjust-attitude! npc +attchange-pleased+))
	   (:dest "consul-tepesco-letter"))
  
  (:option (:test (has-object? player "opened-letter-to-tepesco"))
	   (:text "I have an urgent letter for you.")
	   (:perform (adjust-attitude! npc +attchange-offended+))
	   (:dest "consul-tepesco-letter"))

  (:option (:test (and (flag "next-quest-avi")
		       (not (doing-quest? player "tepesco-quest-avi"))))
	   (:text "You mentioned you had work for me consul.")
	   (:dest "consul-tepesco-quest-avi"))

  (:option (:test (doing-quest? player "tepesco-quest-avi-return"))
	   (:text "I have safely escorted Avi Farethan through the customs.")
	   (:node (:text "Good work ~A, you seem to be an efficient bureaucrat.
Have a look around town while I check things." (player.name player))
		  (:perform (unset-flag "next-quest-avi")
			    (advance-quest *variant* "tepesco-quest-avi" player :giver npc))
		  (:dest :quit "No problem consul, I'll be back")))
  
  (:option (:test (or (and (done-quest? player "tepesco-quest-avi")
			   (not (doing-quest? player "robbery-quest")))
		      (flag "test-robbery")))
	   (:text "Anything I could assist you with?")
	   (:node (:text "~A! Good thing you're here, one of the copian merchants in town has been robbed
by bandits.  The merchant, Lodicus Archus, was a regular trading partner for many of our traders,
and the stuff stolen was supposed to go into Atrocitas in a few days.  I need you meet Lodicus,
and help him recover the goods. It's a sensitive matter that we atrocitans help with this, but Lodicus
doesn't trust the copian authorities. Be careful." (player.name player))
		  (:perform  (let ((quest (find-quest *variant* "robbery-quest")))
			       (init-quest *variant* quest npc player)))
		  (:dest :quit "I'll see to it, Lodicus' shop is right south of here.")))
	   
  
;;  (:include "generic-area-info")
  (:include "generic-area-info" "tepesco-atrocitas-info")
  
  (:quit-option "I will be back later.")
  )

(define-conversation (player npc)
    (:id "tepesco-atrocitas-info")
  (:text "foo") ;; why, oh why?
  (:option (:test (not (flag "work-for-tepesco")))
	   (:text "What is the situation in Atrocitas?")
	   (:node (:text "The much loved King Aequus died peacefully 
after a long reign, and his eldest son Prince Adauego will be crowned 
king after a time of mourning.  King Aequus was a good king and he will 
be dearly missed.")
		  (:dest :back "I see.")))
  
  (:option (:test (flag "work-for-tepesco"))
	   (:text "What is the situation in Atrocitas?")
	   (:node (:text "As you already know, King Aequus died in his sleep. 
Copian agents are trying to spread false rumours of civil war and evil princes. 
We both know this to be false, and our job is to put down these evil rumours.")
		  (:dest :back "I see.")))
  
  (:option (:text "What's your job?")
	   (:node (:text "I'm a liaison between the local copian authorities 
and the Kingdom of Atrocitas.  I also aid atrocitan citizens to understand the 
copian system and assist as legal aid in matters where atrocitan citizens 
need help.  I also provide information about the Kingdom, and necessary papers 
for copians that wish to visit.")
		  (:dest :back "I see.")))
  )
