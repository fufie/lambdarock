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
  :description "The digestive system is very efficient at digesting animal proteins, and one is able to reap considerably more benefits from meat, eggs, .. Choosing carnivore as diet practically makes you an obligate carnivore. Examples of carnivores in nature are felines, canines, birds of prey, and snakes."
  :key '<carnivore>
  :power-lvl 1
  :excludes '<herbivore>
  :levels nil)

(define-racial-ability "herbivore" "Herbivore"
  :group '<nutrition>
  :description "The digestive system of a herbivore is complex and efficient and breaking down the structure of plant matter.  A herbivore is able to reap benefits from a full range of plant material, where an omnivore's digestive system isn't robust enough to handle the same amount of fiber.  A herbivore will also be able to gain more nutrition from plants than an omnivore.  Examples of herbivores are deer, grazers, kangaroos, etc."
  :key '<herbivore>
  :power-lvl 1
  :excludes '<carnivore>
  :levels nil)

(define-racial-ability "omnivore" "Omnivore"
  :group '<nutrition>
  :description "The digestive system of an omnivore is robust enough to reap nutritional benefit from both animal proteins as well as a wide range of plants.  An omnivore is able to survive on different diets in case of shortage, as well as reaping all the necessary vitamins and minerals through a diverse diet. While an omnivore might not reap the same amount of energy from meat as a carnivore or the same amount of energy from plants as a herbivore, an omnivore diet is usually considered the optimal diet for survival.  Examples of omnivores are humans, most bears and pigs."
  :key '<omnivore>
  :power-lvl 10
  :obsoletes '(<carnivore> <herbivore>)
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
  :levels nil)

(define-racial-ability "motion-vision" "Motion vision"
  :group '<sight>
  :description "Motion vision allows you to spot movement quickly and easily, and will give you an advantage in fights."
  :key '<detailed-vision>
  :power-lvl 5
  :levels nil)

(define-racial-ability "infrared-vision" "Infrared vision"
  :group '<sight>
  :description "Infrared vision allows you to spot warm and cold objects, regardless of the presence of normal light."
  :key '<infrared-vision>
  :power-lvl 10
  :levels nil)

(define-racial-ability "xray-vision" "X-Ray vision"
  :group '<sight>
  :hidden t
  :description "X-ray vision allows you to look through e.g wooden walls, and see things normally concealed."
  :key '<xray-vision>
  :power-lvl 30
  :levels nil)

;;; === Smell ===

(define-racial-ability "detail-smell" "Detailed smell"
  :group '<smell>
  :description "Detailed smell allows you to smell things over a longer distance, as well as identify more information about targets."
  :key '<detailed-smell>
  :power-lvl 5
  :levels nil)

;;; === Hearing ===

(define-racial-ability "detail-hearing" "Detailed hearing"
  :group '<hearing>
  :description "Detailed vision allows you to spot details at a long distance and see e.g weak spots on enemies."
  :key '<detailed-vision>
  :power-lvl 5
  :levels nil)

;;; === Mouth ===

(define-racial-ability "fangs" "Fangs"
  :group '<mouth>
  :description "Fangs are long pointed teeth used for biting and tearing flesh.  The damage can be severe and critical."
  :key '<fangs>
  :power-lvl 5
  :levels nil)

(define-racial-ability "poison-fangs" "Poisoned fangs"
  :group '<mouth>
  :description "Poisoned fangs allows a solid bite to also distribute poison, typically causing paralysation, crippling pain or even instant death."
  :key '<poison-fangs>
  :power-lvl 5
  :levels nil)

(define-racial-ability "sabretooth-fangs" "Sabretooth fangs"
  :group '<mouth>
  :description "Sabretooth fangs are the ultimate in fangs.  Razor-sharp, long and able to tear even the toughest skin, as well as penetrating deep causing internal damage."
  :key '<sabretooth-fangs>
  :power-lvl 5
  :levels nil)

;;; === Hands ===

(define-racial-ability "claws" "Claws"
  :group '<hands>
  :description "Claws allow your unarmed hits damage more, and increases the chance of critical hits."
  :key '<claws>
  :power-lvl 5
  :levels nil)

;;; === Feet ===

(define-racial-ability "speedy-feet" "Speedy feet"
  :group '<feet>
  :description "Speedy feet allows you to move faster."
  :key '<speedy-feet>
  :power-lvl 5
  :levels nil)

;;; === Muscle ===

(define-racial-ability "strong-muscles" "Strong muscles"
  :group '<muscle>
  :description "Strong muscles allows you to carry more and hit harder."
  :key '<strong-muscle>
  :power-lvl 5
  :levels nil)

;;; === Internal Organs ===

;;; === Skin ===

(define-racial-ability "barkskin" "Barkskin"
  :group '<skin>
  :description "Barkskin makes your skin tough and robust as bark, making it harder to penetrate."
  :key '<barkskin>
  :power-lvl 5
  :levels nil)

;;; === Skeleton ===

(define-racial-ability "strong-skeletong" "Strong skeleton"
  :group '<skin>
  :description "DESCRIPTION."
  :key '<strong-skeleton>
  :power-lvl 5
  :levels nil)


;; === Fur ===

(define-racial-ability "wolf-fur" "Wolf fur"
  :group '<fur>
  :description "Wolf fur is quite resistant to both water and cold."
  :key '<wolf-fur>
  :power-lvl 5
  :levels nil)

;; === Head ===

(define-racial-ability "horn" "Horn"
  :group '<head>
  :description "Horns can be quite useful when head-butting softer things."
  :key '<horn>
  :power-lvl 5
  :levels nil)
