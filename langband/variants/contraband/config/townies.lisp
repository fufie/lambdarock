(in-package :org.langband.contraband)

(define-monster-kind "copian-guard" "Imperial Guard"
  :numeric-id  13
  :gfx-sym (tile-paint-value 7 5)
  :desc "He is there to make you and other citizens safe."
  :text-sym (text-paint-value +term-red+ #\p)
  :power-lvl 10
  :hitpoints '(10 . 10)
  :armour 20
  :speed 110
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 10
  :type '(<npc> <copian>) ;; guards in army or civilian law?
  :gender '<male>) 

(define-monster-kind "copian-customs-officer" "Customs officer"
  :numeric-id  14
  :gfx-sym (tile-paint-value 14 9)
  :desc "He ensures that papers are ok, all taxes are paid and no illegal goods are trafficked."
  :text-sym (text-paint-value +term-red+ #\p)
  :power-lvl 7
  :hitpoints '(7 . 10)
  :armour 20
  :speed 110
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 10
  :type '(<npc> <copian> <customs>) ;; guards in army or civilian law?
  :gender '<male>) 
