;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.evomyth -*-

#|

DESC: variants/evomyth/config/abillities.lisp
Copyright (c) 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.evomyth)

(define-racial-ability "nutrition" "Nutrition"
  :key '<nutrition>)  

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
 
(define-racial-ability "sight" "Sight"
  :key '<sight>)

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

