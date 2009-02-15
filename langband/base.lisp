;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: base.lisp - basic code for the rest of the game
Copyright (c) 2000-2004 - Stig Erik Sandoe

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; 28 bits
  (deftype u-fixnum () '(unsigned-byte 28))
  ;; 16 bits
  (deftype u8b () '(unsigned-byte 8))
  (deftype u16b () '(unsigned-byte 16))
  (deftype u24b () '(unsigned-byte 24))
  (deftype u32b () '(unsigned-byte 32))

  ;;  (deftype vinfo-bit-type () `(unsigned-byte 32))
  (deftype vinfo-bit-type () `(unsigned-byte 16))

  (deftype =char-code= ()
    #+handle-char-as-num
    'u16b
    #-handle-char-as-num
    'character
    )

  ;; the types that return-action of an event may have.
  (deftype return-actions ()
    `(member :remove-event :keep-event))

  (deftype event-types ()
    '(member :on-create :step-on-coord))
  )

;; the conditions should be used more..
(define-condition langband-quit (condition) ()) 
(define-condition savefile-problem (serious-condition)
  ((desc :initarg :desc :reader saveproblem.desc)))

(define-condition illegal-data-definition (serious-condition)
  ((id   :initarg :id   :reader illegal-data.id)
   (desc :initarg :desc :reader illegal-data.desc)))

(define-condition illegal-object-data (illegal-data-definition)
  ())

(define-condition illegal-monster-data (illegal-data-definition)
  ())

(define-condition illegal-attack-data (illegal-data-definition)
  ())

(define-condition illegal-room-data (illegal-data-definition)
  ())

(define-condition illegal-char-class-data (illegal-data-definition)
  ())

(define-condition illegal-char-race-data (illegal-data-definition)
  ())

(define-condition illegal-char-stat-data (illegal-data-definition)
  ())

(define-condition illegal-ui-theme-data (illegal-data-definition)
  ())


(defmacro signal-condition (type &rest args)
  `(signal (make-condition ,type ,@args)))

(defmacro error-condition (type &rest args)
  `(error (make-condition ,type ,@args)))

;;; === Some binary types
(bt:define-unsigned u64 8)
(bt:define-signed s64 8)
(bt:define-unsigned u128 16)

;;; === Some macros we need right away

(defmacro defsubst (name arglist &body body)
  "Declare an inline defun."
  `(progn
    (declaim (inline ,name))
    (defun ,name ,arglist ,@body)))

(defmacro defcustom (name type init doc)
  "Define a typed variable."
  `(progn
    (declaim (type ,type ,name))
    (defvar ,name (the ,type ,init) ,doc)))

(defmacro defconst (name type init doc)
  "Define a typed constant."
  `(progn
    (declaim (type ,type ,name))
    (defconstant ,name (the ,type ,init) ,doc)))

(defmacro def-exportconst (name init &optional doc)
  "Define a typed constant."
  `(progn
    (eval-when (:execute :load-toplevel :compile-toplevel)
      (export ',name))
    (defconstant ,name ,init ,doc)))


;;; === End important/general macros

;;; === Some dynamic variables of importance for the rest of the system:


;;(defvar *game-parameters* (make-hash-table :test #'eq)
;;  "a table with keyword game-parameters")

;; four very important variables :-)
(defvar *variant* nil "variant in use.  one should not rebind this
too frequently.")
(defvar *level* nil "The current level, for good and bad.")
(defvar *dungeon* nil "global dungeon object")
(defvar *player* nil "the player object")

(defvar *strategy* nil "Bound by the AI-controller to the active strategy.")

(defvar *redraw* (make-hash-table :test #'eq)
  "what to redraw, flags with NIL/T values.")

(defvar *update* (make-hash-table :test #'eq)
  "What to update, flags with NIL/T values.")

(defvar *visevents* '())
(defvar *current-speed-factor* 100.0)

(defvar *cur-dun* nil
  "a dynamic variable which is set to an object
of type DUN-DATA (see: dungeon.lisp) and is valid
throughout dungeon-generation")

(defvar *hitpoint-warning* 3
  "Value in [0..9] of when to warn about hitpoint-losses")

;; must change
(defvar *cur-win* nil "Pointer to the currently active window.
Should not be altered directly.")

(defvar *input-event* nil "An input-event that can be reused to avoid
consing up a new one.")


(defvar *obj-type-mappings* (make-hash-table :test #'eq)
  "keeps track of mapping from key to object-types, used by factories.")

(defvar *engine-version* "0.1.7" "A version specifier that can be used for
display and listings, not useful for internal code.")
(defvar *engine-num-version* 125 "A numeric version for the engine that can
be used by internal code.  It will typically be incremented with every
non-compatible change, so when connecting a variant to the engine, this is
the number you should look at. It's quick to compare against and it's unambigious.")

(defvar *engine-source-dir* "./")
(defvar *engine-data-dir* "./data/")
(defvar *engine-config-dir* "./config/")

;; must be set to T by init for use of graphics.
(defvar *graphics-supported* nil)
(defvar *turn-mode* t "Use turn-mode instead of nice animations.")

(defvar *current-ui-theme* nil "The ui-theme currently being used.")
(defvar *screen-width* 800)
(defvar *screen-height* 600)

(defvar *message-handler* nil "An object of class message-handler that handles handling and display of messages.")

(defvar *printfield-info* (make-hash-table :test #'eq))
(defvar *printfield-hooks* (make-hash-table :test #'eq))


;; these specify how many possible windows there are
;; should be variable, not constants!
(def-exportconst +max-frames+ 10 "max frames")
(def-exportconst +predefined-frames+ 10 "predefined frames")


;;; === End dynamic variables

(defmacro while (test &body body)
  "repeat BODY while TEST is true"
  `(do ()
       ((not ,test))
     ,@body))

(defmacro when-bind ((var expr) &body body)
  "generalisation of (let ((var expr)) (when var ...))."
  `(let ((,var ,expr))
    (when ,var
      ,@body)))

(defmacro unless-bind ((var expr) &body body)
  "generalisation of (let ((var expr)) (unless var ...))."
  `(let ((,var ,expr))
    (unless ,var
      ,@body)))

(defun split-seq-on (str &optional (ch #\Space))
  "returns a list of strings formed by breaking STR at every occurance
of CH (which is not included).  Works for any sequence, not just strings,
but optimized for vectors."
  (when str
    (do* ((prev-pos 0 (1+ next-pos))
          (next-pos (position ch str)
                    (position ch str :start prev-pos))
          (stuff (list (subseq str 0 next-pos))
                 (cons (subseq str prev-pos next-pos)
                       stuff)))
        ((null next-pos) (nreverse stuff)))))


(defmacro charify-number (num)
  #+handle-char-as-num
  num
  #-handle-char-as-num
  `(code-char ,num)
  )

(defmacro numberify-char (chr)
  #+handle-char-as-num
  chr
  #-handle-char-as-num
  `(char-code ,chr)
  )


(defun positive-integer? (obj)
  "Returns T if obj is an integer and > 0."
  (and (integerp obj) (> obj 0)))

(defun non-negative-integer? (obj)
  "Returns T if obj is an integer and >= 0."
  (and (integerp obj) (>= obj 0)))

;; turn into a deftype later
(defun nonboolsym? (sym)
  (and sym (not (eq sym t)) (symbolp sym)))

(defun symbolify (data)
  "Returns a symbol in a form which can be understood when reading code."
  (if (eq data nil)
      nil
      `',data))


#-sbcl
(declaim (ftype (function (u-fixnum) character) i2a))
#+sbcl
(declaim (ftype (function (u-fixnum) (values character &optional)) i2a))
(defsubst i2a (num)
  "Returns the letter corresponding to #\a + num."
  (declare (type u-fixnum num))
  (the character (code-char (the u-fixnum (+ #.(char-code #\a) num)))))

#-sbcl
(declaim (ftype (function (character) fixnum) a2i))
#+sbcl
(declaim (ftype (function (character) (values fixnum &optional)) a2i))
(defun a2i (char)
  "Returns the number corresponding to the char given, where #\a is 0."
;;  (assert (characterp char))
  (let ((res (- (char-code char) (char-code #\a))))
    (when (< res 0)
      (warn "A2I got illegal char ~s ~s" char res))
    res))

;; hack ever so long
#-sbcl
(declaim (ftype (function (u-fixnum) u-fixnum) randint))
#+sbcl
(declaim (ftype (function (u-fixnum) (values u-fixnum &optional)) randint))
(defun randint (num)
  (1+ (random num)))

;; hack ever so long
#-sbcl
(declaim (ftype (function (u-fixnum u-fixnum) u-fixnum) rand-range))
#+sbcl
(declaim (ftype (function (u-fixnum u-fixnum) (values u-fixnum &optional)) rand-range))
(defun rand-range (a b)
  (+ a (random (1+ (- b a)))))

#-sbcl
(declaim (ftype (function (u-fixnum u-fixnum) u-fixnum) rand-spread))
#+sbcl
(declaim (ftype (function (u-fixnum u-fixnum) (values u-fixnum &optional)) rand-spread))
(defun rand-spread (a b)
  (rand-range (- a b) (+ a b)))

(defun rand-elm (seq)
  "Returns a random element from given sequence."
  (let* ((len (length seq))
	 (elm (random len)))
    (elt seq elm)))

#+compiler-that-inlines
(defsubst int-/ (a b)
;;  (declare (type u-fixnum a b))
;;  (the u-fixnum
    (prog1 (floor a b))
;;    )
  )


#-compiler-that-inlines
(defmacro int-/ (a b)
  "Integer division, as in C."
  `(prog1 (floor ,a ,b)))

(defun round-/ (a b)
  (prog1 (floor (+ (/ a b) 1/2))))

(defun shrink-array! (arr)
  "Shrinks the array and removes NIL gaps. Returns the new size."

  (let ((len (length arr))
	(cur-write 0)
	(cur-obj nil))
    (declare (type u-fixnum cur-write len))
    
    (loop for cur-read of-type fixnum from 0 below len
	  do
	  (setq cur-obj (aref arr cur-read))
	  (when cur-obj
	    (setf (aref arr cur-write) cur-obj)
	    (incf cur-write))
	  )

    (loop for i of-type fixnum from cur-write below len
	  do
	  (setf (aref arr i) nil))
    
    cur-write))


(defun add-object-to-array! (arr cur-size max-size aobj)
  "Adds an object to array. Returns T if succesful
and NIL if unsuccesful."
  (declare (type u-fixnum cur-size max-size))
  (cond ((< cur-size max-size)
	 ;; we have room
;;	 (warn "Adding ~a to array" aobj)
	 (setf (aref arr cur-size) aobj)
	 t)
	;; we're full
	(t
	 (lang-warn "equipment full..")
	 nil)))


(defun shuffle-array! (tmp-arr len)
  "Shuffles the given array"
  (declare (type u-fixnum len))
  
  (loop for i of-type u-fixnum from 0 below len
	for rnd-val = (+ i (random (- len i)))
	do
	(rotatef (aref tmp-arr i) (aref tmp-arr rnd-val)))
  
  tmp-arr)

(defun get-array-with-numbers (len &key fill-pointer)
  "Returns an array with increasing numbers."
  (let ((arr (if fill-pointer
		 (make-array len :fill-pointer t)
		 (make-array len))))
    
    (loop for i from 0 below len
	  do
	  (setf (aref arr i) i))
    
    arr))

(defun parse-dice (str)
  "Parses a dice and returns a CONS with num-dice and base-dice."
  (let ((pos (position #\d str)))
    (cons (parse-integer (subseq str 0 pos))
	  (parse-integer (subseq str (1+ pos))))))

#-sbcl
(declaim (ftype (function (u-fixnum u-fixnum) u-fixnum) roll-dice))
#+sbcl
(declaim (ftype (function (u-fixnum u-fixnum) (values u-fixnum &optional)) roll-dice))
(defun roll-dice (number dice)
  "Rolls dice numbber of times and returns the result."
  (declare (type u-fixnum number dice))
  
  (if (and (plusp number) (plusp dice))
      (loop for x from 1 to number
	    summing (randint dice))
      0))
#-sbcl
(declaim (ftype (function (string) u-fixnum) parse-and-roll-dice))
#+sbcl
(declaim (ftype (function (string) (values u-fixnum &optional)) parse-and-roll-dice))
(defun parse-and-roll-dice (str)
  "Parses and rolls the dice-str."
  (let ((nums (parse-dice str)))
    (roll-dice (car nums) (cdr nums))))


(defmacro bit-flag-add! (loc &rest flags)
  "Same as 'loc |= flags', and uses LOGIOR."
  `(setf ,loc (logior ,loc ,@flags)))

(defmacro bit-flag-remove! (loc flag)
  "Same as 'loc &= ~(flag)', and uses LOGANDC2."
  `(setf ,loc (logandc2 ,loc ,flag)))

;; change me into a macro at some point?
#-compiler-that-inlines
(defmacro bit-flag-set? (loc flag)
  `(/= 0 (logand ,loc ,flag)))

#+compiler-that-inlines
(defsubst bit-flag-set? (loc flag)
  "Checks if the given flag is set, and returns T or NIL."
  (/= 0 (logand loc flag)))

#-compiler-that-inlines
(defmacro bit-flag-and (pos1 pos2)
  `(/= 0 (logand ,pos1 ,pos2)))

#+compiler-that-inlines
(defsubst bit-flag-and (pos1 pos2)
  (/= 0 (logand pos1 pos2)))

(defun verify-id (id)
  "Verifies the id, returns NIL on failure, T when ok."
  (flet ((char-checker (x)
	   (cond ((eql x #\-)
		  t)
		 ((eql x #\/)
		  t)
		 ((eql x #\*)
		  t)
		 ((alpha-char-p x) ;; fix to only lowercase later
		  t)
		 ;; a temporary one, remove later
		 ((digit-char-p x)
		  t)
		 (t nil))))
    (if (stringp id)
	(every #'char-checker id)
	nil)))
		      
   

(defun compile-in-environment (func)
  (let (
	#+cmu (*compile-print* nil)
	      #+cmu (*load-verbose* nil)
	      (*load-print* nil)
	      ;;#+cmu (*error-output* o-str)
	      #+cmu (extensions:*gc-verbose* nil)
	      )
    (funcall func)))

(defun text-to-ascii (str)
  "converts a c-type string to a lisp-string in ascii."
  
  (let ((backslashed nil)
	(controlled nil))
    
    (with-output-to-string (s)
      (loop for x across str
	    do 
	    ;;(warn "checking ~s" x)
	    (cond (backslashed
		   (case x
		     (#\\ (write-char #\\ s))
		     (#\s (write-char #\Space s))
		     (#\b (write-char #\Backspace s))
		     (#\n (write-char #\Linefeed s))
		     (#\r (write-char #\Return s))
		     (#\t (write-char #\Tab s))
		     (#\e (write-char #\Escape s))
		     ;; skip hex
		     (otherwise
		      (write-char x s)))
		   (setq backslashed nil))

		  (controlled
		   (write-char (code-char (logand (char-code x) #o37)) s)
		   (setq controlled nil))
		  ((eql x #\\) 
		   (setq backslashed t))
		  ((eql x #\^)
		   (setq controlled t))
		  
		  (t
		   (write-char x s))))
      s)))

(defun get-late-bind-function (package name)
  "Tries to find a function that may not exist at read, compile
or load time, ie totally dynamic."
  (let* ((pack (find-package package))
         (sym (find-symbol (symbol-name name) pack)))
    (when (fboundp sym)
      (fdefinition sym))))

#+xp-testing
(defun do-a-test (stage)
  (when-bind (func (get-late-bind-function 'lb-test 'run-lb-test))
    (funcall func stage :verbose t)))

(defun centre-string (str max-len)
  "Tries to return a centred version of the string."
  (format nil (format nil "~~~a:@<~a~~>" max-len str)))

(defun lang-warn (format-string &rest format-args)
  "Prints a warning for Langband-system.  It works almost like
regular WARN except that no condition is sent, use regular WARN for such
cases.  Leaks memory, only use when testing."

  #-clisp
  (format *error-output* "~&~@<LB-Warning:  ~3i~:_~A~:>~%"
          (apply #'format nil format-string format-args))
  ;; ugly
  #+clisp
  (format *error-output* "~&~a~%" (apply #'format nil format-string format-args)))

(defsubst mystrcat (x y)
  "Basically catenates strings and tries to stringify arguments to be sure"
  (concatenate 'string (string x) (string y)))


(defun get-symcase-fun ()
  "Returns the symcase-fun as a symbol."
  #+allegro
  (ecase excl:*current-case-mode*
    (:case-sensitive-lower
     'string-downcase)
    (:case-insensitive-upper
     'string-upcase))
  #-allegro
  'string-upcase)

(defmacro concat-pnames (&rest args) 
  "concatenates strings or symbols and returns an interned
symbol which can be passed to e.g defun (as name of function)."

  (let ((str (gensym))
        (case-fun (get-symcase-fun)))

    `(let ((,str (,case-fun (reduce #'mystrcat (list ,@args)))))
       (if (and (plusp (length ,str)) (eql (char ,str 0) #\:))
           (intern (subseq ,str 1) (find-package :keyword))
           (intern ,str)
           ))
    ))

;; first two bits of bitfield is code
(defconstant +lbbf-clean+ 0) ;; will clean a tile, ignores all bits
(defconstant +lbbf-char+ 1)  ;; bits 3-10 is character, bits 11-18 is 8-bit colour
(defconstant +lbbf-gfx+ 2)   ;; bits 3-10 is file, bits 11-26 is tilenumber in file  
(defconstant +lbbf-idx+ 3) ;; bits 3-10 is index, bits 11-18 is 8-bit colour


;; hackish, must be improved later
(declaim (inline text-paint-value))
(defun text-paint-value (attr char)
  (let ((charnum (etypecase char
		   (character (char-code char))
		   (integer char))))
    (logior +lbbf-char+
	    (dpb charnum (byte 8 2) 0)
	    (dpb attr (byte 8 10) 0)
	    )))

(declaim (inline tile-paint-value))
(defun tile-paint-value (file tile)
  (logior +lbbf-gfx+
	  (dpb file (byte 8 2) 0)
	  (dpb tile (byte 16 10) 0)
	  ))

(declaim (inline idx-paint-value))
(defun idx-paint-value (attr idx)
  (logior +lbbf-idx+
	  (dpb idx (byte 8 2) 0)
	  (dpb attr (byte 8 10) 0)
	  ))

#||
;; not used!
;; put the tile-number in bits 9-24
(defmacro tile-number (tile)
  `(dpb ,tile (byte 16 8) 0))

;; put the tile-file in bits 25-32
(defmacro tile-file (file)
  `(dpb ,file (byte 8 24) 0))
||#

#||
;; not used
(defmacro make-legal-attr (attr)
  `(dpb ,attr (byte 8 8) 0))
||#

#||
(defmacro tile-number (num)
  `(+ +graphics-start+ ,num))

;; also support names!
(defmacro tile-file (num)
  `(+ +graphics-start+ ,num))
||#
#||
(defmacro tile-number (num)
  num)

;; also support names!
(defmacro tile-file (num)
  num)
||#


(defun is-vowel? (the-char)
  "Returns T if THE-CHAR is a vowel."
  (find the-char '(#\a #\e #\i #\o #\u #\y)))

#-sbcl
(declaim (ftype (function (fixnum fixnum) fixnum) max-cap))
#+sbcl
(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) max-cap))
(defun max-cap (max val)
  "If val is less than max, return val, else return max."
  (if (> val max)
      max
      val))
#-sbcl
(declaim (ftype (function (fixnum fixnum) fixnum) min-cap))
#+sbcl
(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) min-cap))
(defun min-cap (min val)
  "If val is greater than min, return val, else return min."
  (if (< val min)
      min
      val))

(defun reset-redraw! (creature flag)
  "Turns off the given flag."
  (declare (ignore creature))
  (setf (gethash flag *redraw*) nil))

(defun ask-for-redraw! (creature flag)
  "Turns on the given redraw flag."
  (declare (ignore creature))
  (setf (gethash flag *redraw*) t))

(defun want-redraw? (creature flag)
  "Checks if a redraw-flag is set."
  (declare (ignore creature))
  (gethash flag *redraw*))

(defun any-redraws? (creature)
  "Checks if any redraws are wanted."
  (declare (ignore creature))
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (when value
		 (return-from any-redraws? t)))
	   *redraw*)
  nil)

(defmacro define-redraw-key (key desc)
  (declare (ignore desc))
  `(export (list ',key)))

(defun reset-update! (creature flag)
  "Turns off the given flag."
  (declare (ignore creature))
  (setf (gethash flag *update*) nil))

(defun ask-for-update! (creature flag)
  "Turns on the given update flag."
  (declare (ignore creature))
  (setf (gethash flag *update*) t))

(defun want-update? (creature flag)
  "Checks if an update-flag is set."
  (declare (ignore creature))
  (gethash flag *update*))

(defun any-updates? (creature)
  "Checks if any updates are wanted."
  (declare (ignore creature))
  (maphash #'(lambda (key value)
	       (declare (ignore key))
	       (when value
		 (return-from any-updates? t)))
	   *update*)
  nil)

(defmacro define-update-key (key desc)
  (declare (ignore desc))
  `(export (list ',key)))

(defun get-list-of-redraws (creature)
  "Returns a list of active redraw-flags."
  (declare (ignore creature))
  (let ((flags '()))
    (maphash #'(lambda (key value) (when value (push key flags)))
	     *redraw*)
    flags))

(defun get-list-of-updates (creature)
  "Returns a list of active update-flags."
  (declare (ignore creature))
  (let ((flags '()))
    (maphash #'(lambda (key value) (when value (push key flags)))
	     *update*)
    flags))

