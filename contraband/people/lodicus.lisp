(in-package :org.langband.contraband)

(define-conversation (player npc)
    (:id "merchant-lodicus")
  (:text "Why do you bother me?")
  (:option (:text "Who are you?")
	   (:node (:text "Who is asking?")
		  (:option (:text "My name is ~A." (player.name player))
			   (:node (:text "Well ~A, my name is Lodicus Archus and I'm a merchant." (player.name player))
				  (:dest "merchant-lodicus" "Ok.")))
		  (:option (:text "Sorry, that's only on a need to know basis.")
			   (:node (:text "Well, you're wasting my time then.")
				  (:perform (adjust-attitude! npc +attchange-annoyed+))
				  (:dest :quit "Goodbye.")))
		  ))
  (:option (:test (and (doing-quest? player "robbery-quest")
		       (is-atrocitan? player)))
	   (:text "My name's ~A, and I'm from the atrocitan consulate. Consul Tepesco told me that you
had been robbed and need help to investigate." (player.name player))
	   (:node (:text "Good, I was hoping you atrocitans could assist me, as the
Bartertown authorities are completely useless.  My warehouse is on the western border
of the River Ovid near a small pier.  During the night the door to the warehouse was
cut open by big axes and the crates inside robbed.  They must've had a wagon to get
the loot away.  Please go to the warehouse and see if you can find any clues about
who the thieves were.")
		  (:perform ;; break door
		   nil)
		  (:dest :quit "I'll find the warehouse.")))
	   
  )
