;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/config/sound.lisp - sound-settings

|#

(in-package :org.langband.contraband)


(define-sound-effect "hit-someone"
    "hit1.ogg" "hit3.ogg")

(define-sound-effect "miss-someone"
    "miss1.ogg" "miss2.ogg")

(define-sound-effect "kill-someone"
    "TMaDth00.ogg" "TMaDth01.ogg")

(define-sound-effect "shut-door"
    "Doorshut.ogg")

(define-sound-effect "eat-something"
    "eat1.ogg")

(define-sound-effect "zap-something"
    "magksnd2.ogg" "magksnd8.ogg")
