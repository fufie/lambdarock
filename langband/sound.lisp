;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: sound.lisp - simple functions that deals with sound
Copyright (c) 2001-2003 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)

(defvar *sound-table* (make-hash-table :test #'equal))
(defvar *music-table* nil)

(defun init-sound-system& (size)
  ;; put init here later
  #+use-sound
  (let ((retval (org.langband.ffi:c-init-sound-system& size)))
    (setf *music-table* (make-array size :fill-pointer 0))
    ;; return this value
    (not (minusp retval))))


(defun play-sound (type &key (loops 0) (channel -1))
  "Plays the given sound(s) for type."
  #+use-sound
  (when (using-sound?)
    (let ((sounds (gethash type *sound-table*)))
      (when sounds
	;; add sounds back in
	(org.langband.ffi:c-play-sound-effect (rand-elm sounds) channel loops)

	))))


(defun load-music (fname)
  "Tries to load the fname."
  #+use-sound
  (when (using-sound?)
    (let ((path (concatenate 'string *engine-data-dir* "audio/music/" fname)))
      (when (probe-file path)
	(let ((idx (vector-push path *music-table*)))
	  (org.langband.ffi:c-load-music-file& path idx)
	  idx)))))


(defun play-music (music loops)
  "Tries to play given music."

  #+use-sound
  (let ((idx (etypecase music
	       (integer music)
	       (string (find music *music-table* :test #'equal)))))

    (when (and (integerp idx) (>= idx 0) (< idx (length *music-table*)))
      (warn "Play music ~s" idx)
      (org.langband.ffi:c-play-music-file idx loops))))

(defun halt-music ()
  "Tries to stop any music playing."
  #+use-sound
  (org.langband.ffi:c-halt-music))

(defun halt-sound-effects (&optional (channel -1))
  "Tries to stop any music playing."
  #+use-sound
  (when (integerp channel)
    (org.langband.ffi:c-halt-sound-effects channel)))

(defun define-sound-effect (key &rest sounds)
  "Defines a sound.  Returns nil when sound is not loaded."

  #+use-sound
  (when (using-sound?)
    (let ((base-path (concatenate 'string *engine-data-dir* "audio/effects/"))
	  (current-sounds (gethash key *sound-table*)))
      
  
      (dolist (i sounds)
	(let ((idx (org.langband.ffi:c-load-sound-effect& (concatenate 'string base-path i) -1)))
	  (pushnew idx current-sounds :test #'equal)))

      (setf (gethash key *sound-table*) current-sounds)
      
      (length current-sounds))
    ))
  

(defun using-sound? ()
  "Returns either T or NIL."
  #-use-sound
  nil
  #+use-sound
  ;; fix: cache value to avoid ffi-call
  (if (= 0 (org.langband.ffi:c-get-sound-status))
      nil
      t))

