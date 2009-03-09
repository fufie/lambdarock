;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: ffi/ffi-load.lisp - settings that must be set before foreign build
Copyright (c) 2001 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)


(eval-when (:execute :load-toplevel :compile-toplevel)
  #+darwin
      (let ((lib "/Users/stig/Library/Frameworks/ZTerminal.framework/ZTerminal"))
      (unless (find :ui *langband-loaded-libs*)
	(load-shared-lib :key :lang-ffi :lib lib)
	(push :ui *langband-loaded-libs*)))
  #-darwin
  (let ((lib-path "./zterm/"))

;;   #+cmu
;;   (SYSTEM:FOREIGN-SYMBOL-ADDRESS "funcall0")

    #+unix
    (progn
      (setq lib-path
	    #+langband-development "./zterm/"
	    #-langband-development "/usr/lib/langband/"))
    #+win32
    (progn
      ;; hack
      (setq lib-path (concatenate 'string (lb-engine:lbsys/ensure-dir-name
					   (namestring (lb-engine:lbsys/get-current-directory)))
				  "zterm/")))

    
    #+unix
    (progn
      #-(or cmu sbcl ecl)
      (unless (find :dc *langband-loaded-libs*)
	(load-shared-lib :key :dc :lib (concatenate 'string lib-path "dircall.so"))
	(push :dc *langband-loaded-libs*))
      
      ;; everyone
      (unless (find :ui *langband-loaded-libs*)
	(load-shared-lib :key :lang-ffi :lib (concatenate 'string lib-path "lbui.dylib"))
	(push :ui *langband-loaded-libs*)))
    
    #+win32
    (progn
      (load-shared-lib :key :lang-ffi :lib (concatenate 'string lib-path "lbui.dll")))

    ))
