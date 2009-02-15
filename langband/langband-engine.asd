;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: langband-engine.asd - another system-def for vanilla
Copyright (c) 2001-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

;; we need certain flags
(eval-when (:execute :load-toplevel :compile-toplevel)

  #+(or cmu allegro lispworks sbcl ecl)
  (pushnew :enough-support-for-langband cl:*features*)
  #+(and clisp langband-development) ;; clisp not ready in debian yet
  (pushnew :enough-support-for-langband cl:*features*)
  
  )

(defpackage :langband-engine-system 
  (:use :cl :asdf))

(in-package :langband-engine-system)
 
(asdf:defsystem "langband-engine"
    :name "Langband Engine"
    :author "Stig E Sandoe"
    :version "0.1.7"
    :licence "GPL"
    :properties ((#:author-email . "stig@langband.org")
		 (#:date . "Autumn 2003")
		 (#:licencefile . "COPYING")
		 )
    :components
    ((:module decl
              :pathname ""
              :components ((:file "pre-build")))

     (:module btypes
	      :pathname "tools/"
	      :components ((:file "binary-types"))
	      :depends-on (decl))

     (:module basics
	      :pathname ""
	      :components ((:file "package")
			   (:file "sys" :depends-on ("package")))
	      :depends-on (btypes))

     (:module foreign
	      :pathname "ffi/"
	      :components ((:file "ffi-helpers")
			   (:file "ffi-load" :depends-on ("ffi-helpers"))

			   #+cmu
			   (:file "ffi-cmu" :depends-on ("ffi-load"))
			   #+sbcl
			   (:file "ffi-sbcl" :depends-on ("ffi-load"))
			   #+allegro
			   (:file "ffi-acl" :depends-on ("ffi-load"))
			   #+lispworks
			   (:file "ffi-lw" :depends-on ("ffi-load"))
			   #+clisp
			   (:file "ffi-clisp" :depends-on ("ffi-load"))
			   #+openmcl
			   (:file "ffi-openmcl" :depends-on ("ffi-load")))
	      :depends-on (basics))

     ;; fix remaining dependency-problems as they show up
     (:module basic
              :pathname ""
              :components ((:file "base")
			   (:file "constants" :depends-on ("base"))
			   (:file "generics")
			   (:file "classes" :depends-on ("generics" "constants"))
			   (:file "adts" :depends-on ("base"))
			   (:file "sound" :depends-on ("base"))
			   (:file "window" :depends-on ("classes" "constants"))
			   (:file "global" :depends-on ("classes" "generics" "base" "constants" "window"))
			   (:file "themes" :depends-on ("global"))
			   (:file "character" :depends-on ("classes" "global"))
			   (:file "object" :depends-on ("classes" "generics" "global"))
			   (:file "equipment" :depends-on ("global" "object"))
			   (:file "creature" :depends-on ("classes" "global"))
			   (:file "player" :depends-on ("classes" "global" "character" "equipment"))
			   (:file "monster" :depends-on ("classes" "global"))
			   (:file "dungeon" :depends-on ("base" "monster" "classes" "constants" "window" "adts"))
			   (:file "building" :depends-on ("generics" "base" "global" "dungeon" "equipment"))
			   
			   (:file "allocate" :depends-on ("generics" "dungeon" "constants" "object" "adts"))
			   (:file "generate" :depends-on ("dungeon" "allocate" "classes" "object"
								    "equipment" "generics"))
			   (:file "print" :depends-on ("generics" "player"))
			   (:file "combat" :depends-on ("generics" "base" "sound" "global"
								   "classes" "dungeon" "player"))
			   (:file "project" :depends-on ("base" "generics" "player" "object" "dungeon" "combat"))
			   (:file "util" :depends-on ("constants" "dungeon" "classes" "global" "generics"
								"generate" "project" "building"))
			   (:file "visuals" :depends-on ("util"))
			   (:file "stores" :depends-on ("building" "generics" "equipment" "character" "util"))
			   (:file "view" :depends-on ("dungeon" "generics" "constants"))
			   (:file "actions" :depends-on ("generics" "util" "global" "generate" "combat" "project"
								    "visuals"))
			   (:file "save" :depends-on ("player" "dungeon" "classes" "global" "generics"))
			   (:file "load" :depends-on ("save" "generate"))
			   (:file "death" :depends-on ("global" "player" "character" "save"))
			   (:file "ai" :depends-on ("generics" "monster" "project" "util" "actions"))
			   (:file "loop" :depends-on ("classes" "player" "death" "ai" "print" "actions"
								"view" "util" "adts" "load"
								"window" "visuals"))
			   (:file "birth" :depends-on ("generics" "constants" "classes" "player" "loop"))
			   (:file "dump" :depends-on ("monster" "classes" "object" "character"
								"global" "building" "equipment"))
			   (:file "init" :depends-on ("themes" "monster" "object" "loop" "adts" "util"))
			   (:file "verify" :depends-on ("player" "global" "base" "dungeon" "monster" "character"))
			   
			   
			   )
	      :depends-on (foreign))
     ))


#-enough-support-for-langband
(warn "Langband-Engine has not been tested with '~a ~a', skips compilation."
      (lisp-implementation-type)
      (lisp-implementation-version))
