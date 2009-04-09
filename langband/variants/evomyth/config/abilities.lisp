;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/abilities.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-ability-group "nutrition" "Nutrition" '<nutrition>)  
(define-ability-group "sight" "Sight" '<sight>)
(define-ability-group "smell" "Smell" '<smell>)
(define-ability-group "hearing" "Hearing" '<hearing>)
(define-ability-group "mouth" "Mouth" '<mouth>)
(define-ability-group "hands" "Hands" '<hands>)
(define-ability-group "feet" "Feet" '<feet>)
(define-ability-group "muscle" "Muscle" '<muscle>)
(define-ability-group "organs" "Internal Organs" '<organs>)
(define-ability-group "skin" "Skin" '<skin>)
(define-ability-group "skeleton" "Skeleton" '<skeleton>)
(define-ability-group "fur" "Fur" '<fur>)
(define-ability-group "head" "Head" '<head>)

;;; === Nutrition ===
(define-racial-ability "carnivore" "Carnivore"
  :group '<nutrition>
  :description "You're able to get nutrition from meat."
  :key '<carnivore>
  :power-lvl 1
  :levels nil)

(define-racial-ability "herbivore" "Herbivore"
  :group '<nutrition>
  :description "You're able to get nutrition from plants."
  :key '<carnivore>
  :power-lvl 1
  :levels nil)

(define-racial-ability "omnivore" "Omnivore"
  :group '<nutrition>
  :description "You're able to get nutrition from both plants and meat."
  :key '<omnivore>
  :power-lvl 10
  :levels nil)

(define-racial-ability "photosynthesis" "Photosynthesis"
  :group '<nutrition>
  :description "The combination of nutrients from the ground and sun keeps you with sufficient energy."
  :key '<photosynthesis>
  :power-lvl 20
  :hidden t
  :levels nil)

;;; === Sight ===

(define-racial-ability "detail-vision" "Detailed vision"
  :group '<sight>
  :description "Detailed vision allows you to spot details at a long distance and see e.g weak spots on enemies."
  :key '<detailed-vision>
  :power-lvl 5
  :levels 5)

(define-racial-ability "motion-vision" "Motion vision"
  :group '<sight>
  :description "Motion vision allows you to spot movement quickly and easily, and will give you an advantage in fights."
  :key '<detailed-vision>
  :power-lvl 5
  :levels 5)

(define-racial-ability "infrared-vision" "Infrared vision"
  :group '<sight>
  :description "Infrared vision allows you to spot warm and cold objects, regardless of the presence of normal light."
  :key '<infrared-vision>
  :power-lvl 10
  :levels 5)

(define-racial-ability "xray-vision" "X-Ray vision"
  :group '<sight>
  :hidden t
  :description "X-ray vision allows you to look through e.g wooden walls, and see things normally concealed."
  :key '<xray-vision>
  :power-lvl 30
  :levels 5)

;;; === Smell ===

(define-racial-ability "detail-smell" "Detailed smell"
  :group '<smell>
  :description "Detailed smell allows you to smell things over a longer distance, as well as identify more information about targets."
  :key '<detailed-smell>
  :power-lvl 5
  :levels 5)

;;; === Hearing ===

(define-racial-ability "detail-hearing" "Detailed hearing"
  :group '<hearing>
  :description "Detailed vision allows you to spot details at a long distance and see e.g weak spots on enemies."
  :key '<detailed-vision>
  :power-lvl 5
  :levels 5)

;;; === Mouth ===

(define-racial-ability "fangs" "Fangs"
  :group '<mouth>
  :description "Fangs are long pointed teeth used for biting and tearing flesh.  The damage can be severe and critical."
  :key '<fangs>
  :power-lvl 5
  :levels 5)

(define-racial-ability "poison-fangs" "Poisoned fangs"
  :group '<mouth>
  :description "Poisoned fangs allows a solid bite to also distribute poison, typically causing paralysation, crippling pain or even instant death."
  :key '<poison-fangs>
  :power-lvl 5
  :levels 5)

(define-racial-ability "sabretooth-fangs" "Sabretooth fangs"
  :group '<mouth>
  :description "Sabretooth fangs are the ultimate in fangs.  Razor-sharp, long and able to tear even the toughest skin, as well as penetrating deep causing internal damage."
  :key '<sabretooth-fangs>
  :power-lvl 5
  :levels 5)

;;; === Hands ===

(define-racial-ability "claws" "Claws"
  :group '<hands>
  :description "Claws allow your unarmed hits damage more, and increases the chance of critical hits."
  :key '<claws>
  :power-lvl 5
  :levels 5)

;;; === Feet ===

(define-racial-ability "speedy-feet" "Speedy feet"
  :group '<feet>
  :description "Speedy feet allows you to move faster."
  :key '<speedy-feet>
  :power-lvl 5
  :levels 5)

;;; === Muscle ===

(define-racial-ability "strong-muscles" "Strong muscles"
  :group '<muscle>
  :description "Strong muscles allows you to carry more and hit harder."
  :key '<strong-muscle>
  :power-lvl 5
  :levels 5)

;;; === Internal Organs ===

;;; === Skin ===

(define-racial-ability "barkskin" "Barkskin"
  :group '<skin>
  :description "Barkskin makes your skin tough and robust as bark, making it harder to penetrate."
  :key '<barkskin>
  :power-lvl 5
  :levels 5)

;;; === Skeleton ===

(define-racial-ability "strong-skeletong" "Strong skeleton"
  :group '<skin>
  :description "DESCRIPTION."
  :key '<strong-skeleton>
  :power-lvl 5
  :levels 5)


;; === Fur ===

(define-racial-ability "wolf-fur" "Wolf fur"
  :group '<fur>
  :description "Wolf fur is quite resistant to both water and cold."
  :key '<wolf-fur>
  :power-lvl 5
  :levels 5)

;; === Head ===

(define-racial-ability "horn" "Horn"
  :group '<head>
  :description "Horns can be quite useful when head-butting softer things."
  :key '<horn>
  :power-lvl 5
  :levels 5)
