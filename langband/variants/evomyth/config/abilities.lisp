;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/abilities.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-racial-ability "nutrition" "Nutrition"
  :key '<nutrition>)  
(define-racial-ability "sight" "Sight"
  :key '<sight>)
(define-racial-ability "smell" "Smell"
  :key '<smell>)
(define-racial-ability "hearing" "Hearing"
  :key '<hearing>)
(define-racial-ability "mouth" "Mouth"
  :key '<mouth>)
(define-racial-ability "hands" "Hands"
  :key '<hands>)
(define-racial-ability "feet" "Feet"
  :key '<feet>)
(define-racial-ability "muscle" "Muscle"
  :key '<muscle>)
(define-racial-ability "organs" "Internal Organs"
  :key '<organs>)
(define-racial-ability "skin" "Skin"
  :key '<skin>)
(define-racial-ability "skeleton" "Skeleton"
  :key '<skeleton>)
(define-racial-ability "fur" "Fur"
  :key '<fur>)
(define-racial-ability "head" "Head"
  :key '<head>)

;;; === Nutrition ===
(define-racial-ability "carnivore" "Carnivore"
  :type '<nutrition>
  :description "You're able to get nutrition from meat."
  :key '<carnivore>
  :power-lvl 1
  :levels nil)

(define-racial-ability "herbivore" "Herbivore"
  :type '<nutrition>
  :description "You're able to get nutrition from plants."
  :key '<carnivore>
  :power-lvl 1
  :levels nil)

(define-racial-ability "omnivore" "Omnivore"
  :type '<nutrition>
  :description "You're able to get nutrition from both plants and meat."
  :key '<omnivore>
  :power-lvl 10
  :levels nil)

(define-racial-ability "photosynthesis" "Photosynthesis"
  :type '<nutrition>
  :description "The combination of nutrients from the ground and sun keeps you with sufficient energy."
  :key '<photosynthesis>
  :power-lvl 20
  :hidden t
  :levels nil)

;;; === Sight ===

(define-racial-ability "detail-vision" "Detailed vision"
  :type '<sight>
  :description "Detailed vision allows you to spot details at a long distance and see e.g weak spots on enemies."
  :key '<detailed-vision>
  :power-lvl 5
  :levels 5)

(define-racial-ability "motion-vision" "Motion vision"
  :type '<sight>
  :description "Motion vision allows you to spot movement quickly and easily, and will give you an advantage in fights."
  :key '<detailed-vision>
  :power-lvl 5
  :levels 5)

(define-racial-ability "infrared-vision" "Infrared vision"
  :type '<sight>
  :description "Infrared vision allows you to spot warm and cold objects, regardless of the presence of normal light."
  :key '<infrared-vision>
  :power-lvl 10
  :levels 5)

(define-racial-ability "xray-vision" "X-Ray vision"
  :type '<sight>
  :hidden t
  :description "X-ray vision allows you to look through e.g wooden walls, and see things normally concealed."
  :key '<xray-vision>
  :power-lvl 30
  :levels 5)

;;; === Smell ===

(define-racial-ability "detail-smell" "Detailed smell"
  :type '<smell>
  :description "Detailed smell allows you to smell things over a longer distance, as well as identify more information about targets."
  :key '<detailed-smell>
  :power-lvl 5
  :levels 5)

;;; === Hearing ===

(define-racial-ability "detail-hearing" "Detailed hearing"
  :type '<hearing>
  :description "Detailed vision allows you to spot details at a long distance and see e.g weak spots on enemies."
  :key '<detailed-vision>
  :power-lvl 5
  :levels 5)

;;; === Mouth ===

(define-racial-ability "fangs" "Fangs"
  :type '<mouth>
  :description "Fangs are long pointed teeth used for biting and tearing flesh.  The damage can be severe and critical."
  :key '<fangs>
  :power-lvl 5
  :levels 5)

(define-racial-ability "poison-fangs" "Poisoned fangs"
  :type '<mouth>
  :description "Poisoned fangs allows a solid bite to also distribute poison, typically causing paralysation, crippling pain or even instant death."
  :key '<poison-fangs>
  :power-lvl 5
  :levels 5)

(define-racial-ability "sabretooth-fangs" "Sabretooth fangs"
  :type '<mouth>
  :description "Sabretooth fangs are the ultimate in fangs.  Razor-sharp, long and able to tear even the toughest skin, as well as penetrating deep causing internal damage."
  :key '<sabretooth-fangs>
  :power-lvl 5
  :levels 5)

;;; === Hands ===

(define-racial-ability "claws" "Claws"
  :type '<hands>
  :description "Claws allow your unarmed hits damage more, and increases the chance of critical hits."
  :key '<claws>
  :power-lvl 5
  :levels 5)

;;; === Feet ===

(define-racial-ability "speedy-feet" "Speedy feet"
  :type '<feet>
  :description "Speedy feet allows you to move faster."
  :key '<speedy-feet>
  :power-lvl 5
  :levels 5)

;;; === Muscle ===

(define-racial-ability "strong-muscles" "Strong muscles"
  :type '<muscle>
  :description "Strong muscles allows you to carry more and hit harder."
  :key '<strong-muscle>
  :power-lvl 5
  :levels 5)

;;; === Internal Organs ===

;;; === Skin ===

(define-racial-ability "barkskin" "Barkskin"
  :type '<skin>
  :description "Barkskin makes your skin tough and robust as bark, making it harder to penetrate."
  :key '<barkskin>
  :power-lvl 5
  :levels 5)

;;; === Skeleton ===

(define-racial-ability "strong-skeletong" "Strong skeleton"
  :type '<skin>
  :description "DESCRIPTION."
  :key '<strong-skeleton>
  :power-lvl 5
  :levels 5)


;; === Fur ===

(define-racial-ability "wolf-fur" "Wolf fur"
  :type '<fur>
  :description "Wolf fur is quite resistant to both water and cold."
  :key '<wolf-fur>
  :power-lvl 5
  :levels 5)

;; === Head ===

(define-racial-ability "horn" "Horn"
  :type '<head>
  :description "Horns can be quite useful when head-butting softer things."
  :key '<horn>
  :power-lvl 5
  :levels 5)
