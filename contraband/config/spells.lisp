;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/spells.lisp - spells for contraband
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

;;; Example spells

;;; agnosco
(define-spell "Leonardo's Aerial View"
    '(agnosco aeris)
  "gives you overhead view of outdoors terrain")

(define-spell "Broensted's Wet Secrets"
    '(agnosco aqua)
  "gives you information about a liquid/potion")

(define-spell "Dryad Whisper"
    '(agnosco arbor)
  "gives you information about plants in the area")

(define-spell "Ahlsvik's Animal Lore"
    '(agnosco bestia)
  "gives you hidden lore and secrets about an animal")

(define-spell "Jungian Analysis"
    '(agnosco hominid)
  "gives you a glimpse inside the mind of a humanoid creature")

(define-spell "Burnt Hand of Sophia"
    '(agnosco ignis)
  "gives you lore about a fiery monster or item.")

(define-spell "Leeuwenhoek's lens"
    '(agnosco imago)
  "can see invisible creatures")

(define-spell "Endore's Vision"
    '(agnosco luna)
  "detects werewolves and lycantrophes")

(define-spell "Lord Caernarvon's chilly insights"
    '(agnosco mortis)
  "detect undeads and graves")

(define-spell "Galilei's sunny vision"
    '(agnosco sol)
  "sees the world from the sun and maps the area (does not work underground).")

(define-spell "De Beers' nose for gold"
    '(agnosco terra)
  "senses gold/gems/diamonds in a large area.")


;;; amplio

(define-spell "Float like a butterfly"
    '(amplio aeris)
  "Lets the player float like a feather and not fall, also halves burden.")

(define-spell "Concentrate"
    '(amplio aqua)
  "Concentrates the effect of a liquid and can double the effect of it.")

(define-spell "Swift arrow"
    '(amplio arbor)
  "Enchants a set of arrows to be twice as fast as shoot as well as grant a bonus to damage.")

(define-spell "Like the wind"
    '(amplio bestia)
  "Triples the speed of a small bird, e.g a dove or a falcon.")

(define-spell "Blood of giants"
    '(amplio hominid)
  "Makes the player grow 25% and makes him 50% stronger.")

(define-spell "Unleash fire"
    '(amplio ignis)
  "Makes an existing flame explode wildly, as if someone threw a barrel of greek oil on it.")

(define-spell "Predict moves"
    '(amplio imago)
  "Heightens senses and allows a combat-trained caster to sense and predict an opponent's moves,
granting the caster big bonuses to attack and defence.")

(define-spell "Vicious night"
    '(amplio luna)
  "Increases speed and perceptions of all 'creatures of the night' that are visible.")

(define-spell "Metusalem's wish"
    '(amplio mortis)
  "Speeds the aging of a creature considerably.")

(define-spell "Aura of the sun"
    '(amplio sol)
  "Gives the caster a sunny aura that damages undeads.")

(define-spell "Flowing metal"
    '(amplio terra)
  "Galvanises iron into steel.")
 
;; arcesso

(define-spell "Set's servants"
    '(arcesso aeris)
  "summon air elemental")

(define-spell "Hapi's servants"
    '(arcesso aqua)
  "summon water elemental")

(define-spell "Robin's tree wall"
    '(arcesso arbor)
  "summons a wall of trees.")

(define-spell "Lucky whisper"
    '(arcesso bestia)
  "Summons a horse.")

(define-spell "Call for guards"
    '(arcesso hominid)
  "Summons 1d4 armed and loyal guards.")

(define-spell "Slithering fire"
    '(arcesso ignis)
  "Summons fire lizards")

(define-spell "Far away reality"
    '(arcesso imago)
  "Summons terrain from a previously visited area.")

(define-spell "Mirror shield"
    '(arcesso luna)
  "summons a reflecting shield for a limited period.")

(define-spell "Walk of death"
    '(arcesso mortis)
  "summons walking dead")

(define-spell "Rising Sun"
    '(arcesso sol)
  "Changes night into dawn by summoning the sun (ritual only).")

(define-spell "Brunel job"
    '(arcesso terra)
  "summons a bridge across water")

;;; creo

(define-spell "Fishy smell"
    '(creo aeris)
  "Creates a cloud of awful stench that immobilises creatures")

(define-spell "Quelch thirst"
    '(creo aqua)
  "Creates water or wine in bottles and waterskins.")

(define-spell "Heroes feast"
    '(creo arbor)
  "Creates a delicious meal fit for four big heroes")

(define-spell "Messenger dove"
    '(creo bestia)
  "creates a messenger dove out of a dove feather")

(define-spell "Fiery missile"
    '(creo ignis)
  "Creates a flaming arrow that the player can shoot with a bow.")

(define-spell "Dragonscare"
    '(creo imago)
  "Creates an illusion of a big dragon that will scare all on-lookers.")

(define-spell "Blending madness"
    '(creo luna)
  "Creates a small moon hovering over the caster, causing all onlookers to be confused
and possibly go mad.")

(define-spell "Animal corpse"
    '(creo mortis)
  "Creates a dead corpse of an animal on the ground from a piece of the dead animal,
e.g if you have a rabbit's foot it can be used to create a dead rabbit.  The corpse
will be fresh but will not have any magical properties.")

(define-spell "Eternal lantern"
    '(creo sol)
  "Lights up a room or area forever.")

(define-spell "Sand of the desert"
    '(creo terra)
  "Creates large amounts of fine-grained sand, that can be used to make glass.")

;;; deleo

(define-spell "Smash lungs"
    '(deleo aeris)
  "Destroys all air near a creature, including in the lungs.  Chance of
instant kill.")

(define-spell "Drought curse"
    '(deleo aqua)
  "Destroys water and liquid in an area and prevents new liquid from coming back.")

(define-spell "Grub rot"
    '(deleo arbor)
  "Rots away plants and food.")

(define-spell "Drain animal essence"
    '(deleo bestia)
  "Drains an animal for magical powers and damages the health.")

(define-spell "Fire to ashes"
    '(deleo ignis)
  "Extinguishes normal fires, and damges fiery creatures")

(define-spell "Wind of truth"
    '(deleo imago)
  "Destroys any illusions and phantasms in an area.")

(define-spell "Rays of silver"
    '(deleo luna)
  "Moon rays that light up an area and destroys lycantrophes.")

(define-spell "Rest in peace"
    '(deleo mortis)
  "Calms down a ghost and may even make it cease to exist.")

(define-spell "Darkness roll"
    '(deleo sol)
  "floods an area in darkness and damages creatures of light.")

(define-spell "Time's tear"
    '(deleo terra)
  "Ages an item 100 years, which might destroy or damage it.")

;;; mutatio

(define-spell "Gust of wind"
    '(mutatio aeris)
  "Shapes the air and wind to a huge hand that will hit and hurt flying creatures.")

(define-spell "Waterflow"
    '(mutatio aqua)
  "Stop or turn a water stream/river.")

(define-spell "Firm water"
    '(mutatio aqua)
  "Allows the caster to walk on water as if it was firm ground, and even allows the player to
move at double speed on water.")

(define-spell "Twisted wood"
    '(mutatio arbor)
  "Allows you to reshape a wooden item.")

(define-spell "Bear paws"
    '(mutatio hominid)
  "Changes hands to impressively strong bear paws.")

(define-spell "Kelvin control"
    '(mutatio ignis)
  "Allows caster to change temperature of a fire up and down, even freezing fires are allowed.")

(define-spell "Eyesore"
    '(mutatio imago)
  "Effectively blinds creatures by changing their vision.")

(define-spell "Night Howl"
    '(mutatio luna)
  "During nighttime the caster may change shape into a warg.")

(define-spell "Dead Command"
    '(mutatio mortis)
  "Caster may grasp control of undeads and command their actions.")

(define-spell "Curse of Ampere"
    '(mutatio sol)
  "Caster may adjust room light to become very strong, and damaging to undead.")

(define-spell "Dissolve wall"
    '(mutatio terra)
  "Turns a stone-wall into fine sand.")

;;; novo

(define-spell "Cleanse air"
    '(novo aeris)
  "Clears air in area, and also destroys any illusions affecting the air.")

(define-spell "Purify liquid"
    '(novo aqua)
  "Destroys any diseases, poisons and magical properties in a liquid.")

(define-spell "Spring blossom"
    '(novo arbor)
  "Turns wooden weapons into live trees again that will instantly take root.")

(define-spell "Cure blight"
    '(novo bestia)
  "Cures blight and other diseases in an animal.")

(define-spell "Mend wounds"
    '(novo hominid)
  "Closes and heals wounds on caster or other humanoid.")

(define-spell "Kindle flame"
    '(novo ignis)
  "Breathes new life into a dying flame and lets it burn for a few more turns.")

(define-spell "Eyedrops"
    '(novo imago)
  "Cures any blindness or vision distortions for caster.")

(define-spell "Werewolf bane"
    '(novo luna)
  "Cures lycantrophy, can kill creature in process.")

(define-spell "Lazarus' second chance"
    '(novo mortis)
  "Raises a newly deceased creature from death.")

(define-spell "Lamp revival"
    '(novo sol)
  "Refills lamps, lanterns and torches.")

(define-spell "Salos' Quick Repair"
    '(novo terra)
  "Fixes a broken object.")

;;; tutis

(define-spell "Invisible Shield"
    '(tutis aeris)
  "Creates an invisible shield that protects the air in the area and prevents any illusions from entering.")

(define-spell "Water Barrier"
    '(tutis aqua)
  "Creates a massive wall of water that's hard to pass for creatures and no missiles can go through.")

(define-spell "Turn Wood"
    '(tutis arbor)
  "Cannot be hit with mundane wooden weapons.")

(define-spell "Tooth Border"
    '(tutis bestia)
  "Protects the caster's skin from any bite attacks")

(define-spell "Fire walk with me"
    '(tutis ignis)
  "Lets the player ignore normal fire and resist half the damage from magical fires.")

(define-spell "Mind shield"
    '(tutis imago)
  "Blocks the mind from any newly created illusions.")

(define-spell "Magic mirror"
    '(tutis luna)
  "Reflects half of all magical attacks back on the caster.")

(define-spell "Encapsulate death"
    '(tutis mortis)
  "Drains a random stat, but puts the player's soul in a gem and the player is awakened 20 turns later
if he dies.")

(define-spell "Children of the sun"
    '(tutis sol)
  "Protects the player from all light/sun attacks.")

(define-spell "Caravaggio's magic paint"
    '(tutis terra)
  "Protects an item from wear and tear and all attacks.")
