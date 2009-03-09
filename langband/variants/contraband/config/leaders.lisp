(in-package :org.langband.contraband)

(define-monster-kind "mereo-junifer" "Mereo Junifer"
  :numeric-id  1002
  :gfx-sym (tile-paint-value 24 17)
  :desc "She's respected and loved by her soldiers."
  :text-sym (text-paint-value +term-red+ #\p)
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :power-lvl 18
  :hitpoints '(18 . 10)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique> <copian> <copian-army> <mereo>)
  :picture '(variant-gfx "people/junifer.png")
  :gender '<female>) 

(define-monster-kind "captain-perpetro" "Captain Perpetro"
  :numeric-id  1003
  :gfx-sym (tile-paint-value 14 10)
  :desc "He's in command of Renuo civil matters."
  :text-sym (text-paint-value +term-red+ #\p)
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique> <copian> <copian-army>)
  :picture '(variant-gfx "people/perpetro.png")
  :gender '<male>) 

(define-monster-kind "mereo-ulydes" "Mereo Ulydes"
  :numeric-id  1002
  :gfx-sym (tile-paint-value 24 2)
  :desc "He's the Imperial Mereo of Bartertown."
  :text-sym (text-paint-value +term-red+ #\p)
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique> <copian> <mereo>)
  :picture '(variant-gfx "people/ulydes.png")
  :gender '<male>) 

(define-monster-kind "consul-tepesco" "Consul Tepesco"
  :numeric-id  1002
  :gfx-sym (tile-paint-value 6 38)
  :desc "He's the Atrocitan Consul of Bartertown."
  :text-sym (text-paint-value +term-red+ #\p)
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique> <atrocitan> <diplomat>)
  :picture '(variant-gfx "people/tepesco.png")
  :gender '<male>) 


(define-monster-kind "trader-farethan" "Avi Farethan"
  :numeric-id  1101
  :gfx-sym (tile-paint-value 24 67)
  :desc "He's an atrocitan trader."
  :text-sym (text-paint-value +term-blue+ #\p)
  :power-lvl 7
  :hitpoints '(7 . 10)
  :armour 200
  :speed 110
  :abilities '(<never-attack>)
  :alertness 250
  :vision 15
  :type '(<npc> <unique> <atrocitan> <trader>)
 ;; :picture '(variant-gfx "people/ulydes.png")
  :gender '<male>)

(define-monster-kind "elf-fossgard" "Fossgard the tailour"
  :numeric-id  1201
  :gfx-sym (tile-paint-value 24 61)
  :desc "He's a well-known elven tailour, known for his mastery of silk."
  :text-sym (text-paint-value +term-blue+ #\e)
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 110
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :type '(<npc> <unique> <elven>)
  :picture '(variant-gfx "people/fossgard.png")
  :gender '<male>) 


(define-monster-kind "merchant-lodicus" "Lodicus Archus"
  :numeric-id  1102
  :gfx-sym (tile-paint-value 14 8)
  :desc "He's an important merchant in the copian merchant guild."
  :text-sym (text-paint-value +term-red+ #\p)
  :depth 0
  :rarity 1
  :power-lvl 12
  :hitpoints '(12 . 10)
  :armour 200
  :speed 120
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  :type '(<npc> <unique> <copian> <merchant>)
  :picture '(variant-gfx "people/lodicus.png")
  :gender '<male>)
