(in-package :org.langband.evomyth)

(define-conversation (player npc)
    (:id "elf-fossgard")
  (:text "Hello, what can I do for you?")
  (:option (:text "Are you one of the elves?")
	   (:node (:text "Yes, I'm of the elven people.")
		  (:dest :back "Amazing, your skin isn't green as I thought.")))
  
  (:option (:text "I am looking for a dress for my girlfriend, can you help?")
	   (:node (:text "Bring your girlfriend here so I can do measurements.")
		  (:dest :back "Hmm.")))

  (:option (:test (has-object? player "measurements-junifer"))
	   (:text "Could you make a dress if I gave you the measurements?")
	   (:node (:text "If they're good enough, of course.")
		  (:perform (set-flag "fossgard-can-work-with-measurements"))
		  (:dest :back "Hmm.")))

  (:option (:test (and (doing-quest? player "buy-dress-for-junifer")
		       (flag "fossgard-can-work-with-measurements")
		       (has-object? player "measurements-junifer")))
	   (:text "Here are some measurements, can you do this dress in green silk?")
	   (:node (:text "Haha, this is the dress Captain Perpetro commisioned me to make
for Mereo Junifer.  I made the dress, but the lying bastard doesn't want to pay
the full price for the dress.  You can of course buy the dress.")
		  (:perform (unset-flag "fossgard-can-work-with-measurements")
			    (set-flag "know-perpetro-is-cheap"))
		  (:dest :back "Really...")))

  (:option (:test (flag "know-perpetro-is-cheap"))
	   (:text "So how much is the dress?")
	   (:node (:text "The dress costs 50 florentins, but Perpetro only wanted to pay 30.  You can have
it for 50 since you're a nice guy set up by the no-good captain.")
		  (:perform (unset-flag "know-perpetro-is-cheap")
			    (set-flag "can-buy-dress"))
		  (:dest :back "That's a bit much...")))

  (:option (:test (and (flag "can-buy-dress")
		       (has-gold>= player 50)))
	   (:text "Let me have the dress, here are 50 florentins.")
	   (:node (:text "Thank you, it's been a pleasure doing business with you.")
		  (:perform (unset-flag "can-buy-dress")
			    (add-to-inventory player (get-new-object "green-silk-dress"))
			    (decf (player.gold player) 50)
			    (ask-for-update! player '[bonuses]))
		  (:dest :quit "Goodbye.")))

  (:option (:test (flag "can-buy-dress"))
	   (:text "Hand over the dress or I'll kill you.")
	   (:node (:text "You must be joking.")
		  (:perform (adjust-attitude! npc +attchange-offended+))
		  (:dest :quit "I'll kill you.")))
  )
