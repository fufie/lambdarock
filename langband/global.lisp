;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: global.lisp - globally available functions/methods
Copyright (c) 2000-2004, 2009 - Stig Erik Sandoe

|#

(in-package :org.langband.engine)


(defmacro with-frame ((num) &body body)
  `(let ((*cur-win* (aref *windows* ,num)))
    ,@body))

(defmacro with-full-frame (dummy &body body)
  (declare (ignore dummy))
  `(unwind-protect
    (progn 
      (switch-to-full-frame&)
      (with-frame (+full-frame+)
	,@body))
    (switch-to-regular-frameset&)))

;; very bulky
(defmacro with-dialogue (dummy &body body)
  (declare (ignore dummy))
  ;; posibly find better solution later
  `(invoke-in-dialogue #'(lambda () ,@body)))

(defun sdl-handle-key (key k-ev)
  "Translates an incoming key from SDL into a langband key-event K-EV."
  (when (< 0 key 126)
    (setf (kbd-event.key k-ev) (code-char key)))
  ;; hackish,
  (when (< 272 key 282)
    (setf (kbd-event.key k-ev) (ecase key
				 (273 #\8)
				 (274 #\2)
				 (275 #\6)
				 (276 #\4)
				 (277 #\0)
				 (278 #\7)
				 (279 #\1)
				 (280 #\9)
				 (281 #\3))))
  
  ;; hackish,
  (when (< 255 key 266)
    (setf (kbd-event.key k-ev) (ecase key
				 (256 #\0)
				 (257 #\1)
				 (258 #\2)
				 (259 #\3)
				 (260 #\4)
				 (261 #\5)
				 (262 #\6)
				 (263 #\7)
				 (264 #\8)
				 (265 #\9))))
  
  t)


(defun fetch-event (event-obj only-poll)
  "Tries to fetch a new event in EVENT-OBJ.  if ONLY-POLL is true it will
just do a quick poll, not wait for an event.  Returns (updated) EVENT-OBJ
when a new event was found, otherwise returns NIL."
  
  (let ((listen-arg (if only-poll 1 0))
	(read-obj nil))
    (loop
     (setf read-obj (org.langband.ffi:c-listen-for-event listen-arg))
     (cond ((plusp read-obj)
	    ;;(warn "Returning(~s) evt: ~s" only-poll read-obj) 
	    ;; analyze
	    (cond ((bit-flag-set? read-obj #x01) ;; mouse-event
		   (let ((x (ldb (byte 12 6) read-obj))
			 (y (ldb (byte 12 18) read-obj))
			 (m-ev (input-event.mouseclick event-obj))
			 (button :left))
		     (when (bit-flag-set? read-obj #x02)
		       (setf button :left))
		     (when (bit-flag-set? read-obj #x04)
		       (setf button :right))
		     (when (bit-flag-set? read-obj #x08)
		       (setf button :middle))
		     
		     (setf (input-event.type event-obj) :mouse
			   (mouse-event.x m-ev) x
			   (mouse-event.y m-ev) y
			   (mouse-event.button m-ev) button)
		     (return-from fetch-event event-obj)))
		  
		  ;; keyboard-event
		  (t
		   (let ((key (ldb (byte 16 8) read-obj))
			 (k-ev (input-event.keypress event-obj)))
		     
		     (setf (kbd-event.key k-ev) nil
			   (kbd-event.ctrl k-ev) nil
			   (kbd-event.alt k-ev) nil
			   (kbd-event.shift k-ev) nil) ;; reset

		     
		     (when (bit-flag-set? read-obj #x02)
		       (setf (kbd-event.ctrl k-ev) t))
		     (when (bit-flag-set? read-obj #x04)
		       (setf (kbd-event.alt k-ev) t))
		     (when (bit-flag-set? read-obj #x08)
		       (setf (kbd-event.shift k-ev) t))

		     (ecase (get-system-type)
		       (sdl (sdl-handle-key key k-ev)))
		     
		     (when (kbd-event.key k-ev)
		       (setf (input-event.type event-obj) :key)
		       (return-from fetch-event event-obj))
		     
		     (when (and (plusp key) (/= key 303)) ;; avoid shift
		       (warn "Got back unhandled key ~s" key))
		     
		     ))
		  ))
		  
	   (only-poll
	    ;;(warn "read-obj is ~s for polling" read-obj)
	    (return-from fetch-event nil))
	   
	   (t nil))
     )))

(defun read-one-character (&key left-mouse right-mouse)
  "Reads one character from the C-side."
  (org.langband.ffi:c-flip-framebuffer)
  (loop	;; we might get a mouse-event!
   (let ((ev (fetch-event *input-event* nil)))
     ;;(warn "got back ~s" ev)

     (when ev
       (when (and left-mouse (eq (input-event.type ev) :mouse)
		  (eq (mouse-event.button (input-event.mouseclick ev)) :left))
	 (return-from read-one-character left-mouse))
       
       (when (and right-mouse (eq (input-event.type ev) :mouse)
		  (eq (mouse-event.button (input-event.mouseclick ev)) :right))
	 (return-from read-one-character left-mouse))
           
       (when (eq (input-event.type ev) :key)
	 ;; fix
	 (return-from read-one-character (kbd-event.key (input-event.keypress ev))))
       
       ))))


(defmethod convert-obj (obj to &key)
  (error "Conversion from ~s to ~s not implemented." obj to)
  ;;(coerce obj to)
  )

(defmethod activate-object (obj &key)

  obj)

(defmethod activate-object :around ((obj activatable) &key)
   (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find ACTIVATE-OBJECT for type ~a" (type-of obj))
     (return-from activate-object nil))

   ;; we pass along the same arguments.. 
   (let ((result (call-next-method)))
     ;; we only say that an object is activated if it returned the object
     (cond ((eq obj result)
	    (setf (slot-value obj 'activated) t)
	    obj)
	   
	   (t
	    (lang-warn "Activation of object ~a failed, return was ~a" obj result)
	    nil)
	   )))

(defun has-information? (key &key (variant *variant*))
  "Returns T if there is information for the given key."
  (assert (and (stringp key) (typep variant 'variant)))
  (when (gethash key (variant.information variant))
    t)) ;; a bit superfluous

(defun get-information (key &key (default nil) (variant *variant*))
  "Will just return one value, it's not like GETHASH but takes
same arguments."
  (assert (and (stringp key) (typep variant 'variant)))
  (multiple-value-bind (val f-p)
      (gethash key (variant.information variant) default)
    (if (not f-p)
	default
	val)))

(defun (setf get-information) (value key &key (variant *variant*))
  "Assigns value to an information key."
  (assert (and (stringp key) (typep variant 'variant)))
  (setf (gethash key (variant.information variant)) value)
  value)

(defun remove-information! (key &key (variant *variant*))
  "Returns T if the information existed, NIL if it didn't exist."
  (assert (and (stringp key) (typep variant 'variant)))
  (remhash key (variant.information variant)))

(defun register-information& (&rest args)
  "Registers a group of information on the format:
(register-information key1 info1 key2 info2 ...)"
  (unless (= 0 (mod (length args) 2))
    (warn "Uneven information registration (~s ...)" (car args))
    (return-from register-information& nil))

  (loop for (k v) on args by #'cddr
	do
	(setf (get-information k) v))

  t)

;; some conveniance flagging functions (just shorthands for information table stuff)
(defun set-flag (flag &optional (value t))
  (setf (get-information flag) value))

(defun unset-flag (flag)
  ;; should remove flag?
  (setf (get-information flag) nil))

(defun flag (flag)
  (get-information flag))


;; expanding function defined later in file.
(defmacro format-message! (format-str &rest args)
  `(print-message! (format nil ,format-str ,@args)))

(defmacro format-note! (format-str &rest args)
  `(print-note! (format nil ,format-str ,@args)))


(defun define-effect (symbol name &key number bit-flag)
  (let ((var-obj *variant*))
    (pushnew (make-instance 'effect :symbol symbol :name name
			    :number number :bit-flag bit-flag)
	     (variant.effects var-obj) :test #'eq :key #'effect.symbol)))

(defun is-legal-effect? (variant effect)
  "Returns T if the given effect is legal."
  (assert (and (symbolp effect) (not (eq nil effect))))
  (if (find effect (variant.effects variant) :test #'eq :key #'effect.symbol)
      t
      nil))


;; see variants/vanilla/config/defines.lisp for examples

(defun define-element (symbol name &key (bit-flag 0) (number 0))
  (let ((var-obj *variant*))
    (pushnew (make-instance 'element :symbol symbol :name name
			    :number number :bit-flag bit-flag)
	     (variant.elements var-obj) :test #'eq :key #'element.symbol)))



(defun is-legal-element? (variant element)
  "Returns T if the given element is legal."
  (assert (and (symbolp element) (not (eq nil element))))
  (if (find element (variant.elements variant) :test #'eq :key #'element.symbol)
      t
      nil))

(defun get-element-flag (variant element)
  "Returns the bit-flag for the given element."
  (check-type variant variant)
    (cond ((typep element 'element)
	   (element.bit-flag element))
	  ((and (symbolp element) (not (eq nil element)))
	   (let ((elm (find element (variant.elements variant)
			    :test #'eq :key #'element.symbol)))
	     (if (and elm (typep elm 'element))
		 (element.bit-flag elm)
		 (error "The element ~s is not registered for variant '~a'"
			element (variant.name variant)))
	     ))
	  (t
	   (error "Unknown argument to GET-ELEMENT-FLAG: ~s" element))))

(defun get-element-number (variant element)
  "Returns the numeric index for the given element."
  (check-type variant variant)
  (cond ((typep element 'element)
	 (element.number element))
	((and (symbolp element) (not (eq nil element)))
	 (let ((elm (find element (variant.elements variant)
			  :test #'eq :key #'element.symbol)))
	   (if (and elm (typep elm 'element))
	       (element.number elm)
	       (error "The element ~s is not registered for variant '~a'"
		      element (variant.name variant)))
	   ))
	(t
	 (error "Unknown argument to GET-ELEMENT-NUMBER: ~s" element))))

(defun get-element-number-from-bit-flag (variant bit-flag)
  "Returns the numeric index for a given bit-flag."
  (check-type variant variant)
  (loop for x in (variant.elements variant)
	do
	(when (= (element.bit-flag x) bit-flag)
	  (return-from get-element-number-from-bit-flag (element.number x))))
  (error "Unable to find element with bit-flag ~s" bit-flag))

;;; == variant-related code

(defmethod initialise-monsters& (variant &key)
  (error "No INIT-MONSTERS for ~s" (type-of variant)))
  
(defmethod initialise-floors& (variant &key)
  (error "No INIT-FLOORS for ~s" (type-of variant)))

(defmethod initialise-objects& (variant &key)
  (error "No INIT-OBJECTS for ~s" (type-of variant)))

(defun is-variant? (obj)
  "Checks if OBJ is a VARIANT-object."
  (and obj (typep obj 'variant)))

(defvar *registered-variants* (make-hash-table :test #'equal))
  
(defun register-variant& (id var-constructor &key desc)
  "Registers a variant-object."
  
  (check-type var-constructor function)
  (setf (gethash id *registered-variants*) (list desc var-constructor)))

(defun get-variant-info (id)
  "Returns variant-info that's tregistered for the given id."
  (gethash id *registered-variants*))

(defun get-registered-variants ()
  "Returns a list of ids to registered variants."
  (loop for x being the hash-keys of *registered-variants*
	collecting x))
  
(defun load-variant& (id &key (verbose t))
  "Tries to load a variant."
  (declare (ignore verbose))
  (let ((var-data (gethash id *registered-variants*))
	(var-obj nil))
    (cond ((and (consp var-data)
		(functionp (second var-data)))
	   (format-note! "[Loading '~a' variant, please wait]" id)
	   (setf var-obj (funcall (second var-data))))
	  (t
	   (error "Unable to find variant ~s" id)))
    
    (when (and var-obj (typep var-obj 'variant))
      var-obj)))


;; uses id.. 
(defmethod variant-home-path ((variant variant))
  (variant-home-path (get-id variant)))

(defmethod variant-home-path ((variant string))
  (concatenate 'string (home-langband-path) variant "/"))

(defmethod variant-data-fname ((var-obj variant) data-fname)
  "Returns a full pathname for data."
  (let ((file-path (variant.config-path var-obj)))
    (if file-path
	(concatenate 'string (lbsys/ensure-dir-name file-path) data-fname #+clisp ".lisp")
	data-fname)))

(defmethod variant-save-directory ((var-obj variant))
  (variant-save-directory (get-id var-obj)))

(defmethod variant-save-directory ((variant string))
  (concatenate 'string (variant-home-path variant) "saves/"))

(defun load-variant-data& (var-obj data-file)
  "Loads variant-data from appropriate directory."

  (let ((fname (variant-data-fname var-obj data-file)))
    ;;(warn "trying to load ~s ~s" fname (probe-file fname))
    (handler-case
	(load fname)
      (illegal-data-definition (co)
	(warn "Failed to initialise variant data in file ~a: [~a] ~a"
	      fname (illegal-data.id co) (illegal-data.desc co))
	nil)
      #||
    (serious-condition (co)
      (warn "Serious error/condition happened while loading variant-data in ~s: ~s"
	    fname co)
      nil)
      ||#
    )))
	

(defmacro attack-effect (arguments &body body)
  (assert (= (length arguments) 4))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defun get-attack-type (key &optional (variant *variant*))
  "Returns a possible attack-type object registered with the given variant object."
  (gethash key (variant.attack-types variant)))


(defun execute-turn-events! (var-obj)
  "Executes any turn-events."
  (let* ((turn (variant.turn var-obj))
	 (turn-table (variant.turn-events var-obj))
	 (turn-ev (gethash turn turn-table)))

    (when turn-ev
      (warn "Executing events ~a" turn-ev)
      (remhash turn turn-table))))

(defun register-turn-event! (var-obj wanted-turn event)
  "Adds a turn-event."

  (push event (gethash wanted-turn (variant.turn-events var-obj))))


;;(defun get-monster-filters (type var-obj)
;;  (gethash type (variant.filters var-obj)))


(defun apply-filters-on-obj (type var-obj obj)
  (let ((filters (gethash type (variant.filters var-obj))))
    (dolist (i filters)
      (funcall (cdr i) var-obj obj))))


(defun get-level-builder (id &optional (var-obj *variant*))
  "Returns a level-builder or nil."
  (assert (or (symbolp id) (stringp id)))
  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (gethash key table)))

(defun register-level-builder! (id builder &optional (var-obj *variant*))
  "Registers a level-builder which must be a function."
  (assert (or (symbolp id) (stringp id)))
  (assert (functionp builder))

  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) builder)))

(defun get-named-gameobj-table (variant key slot)
  "Returns a named (slot) gameobj-table from the variant for
a level/identifier (key)."
  (let ((id (etypecase key
	      (level (level.id key))
	      (string key))))
    (when-bind (table (slot-value variant slot))
      (gethash id table))))


(defun make-gender (&key id symbol name win-title)
  "Creates an instance of type GENDER and returns it."
  (make-instance 'gender :id id :symbol symbol
		 :name name :win-title win-title))

(defmethod get-gender ((variant variant) (key string))
  (find key (variant.genders variant) :key #'get-id :test #'equal))

(defmethod get-gender ((variant variant) (key symbol))
  (find key (variant.genders variant) :key #'gender.symbol :test #'eq))


(defmethod get-setting ((variant variant) key)
  (gethash key (variant.settings variant)))

(defmethod (setf get-setting) (setting (variant variant) key)
  (when setting
    (unless (keywordp key)
      (warn "Registered setting without a keyword as key [~s]"
	    key))
    (setf (gethash key (variant.settings variant)) setting)))


(defmethod convert-obj ((htbl hash-table) (to (eql :vector))
			&key sort-table-p sorted-by-key
			sorted-by-fun fill-pointer)
  "Takes a hash-table and returns a vector with the elements."
  
  (let* ((len (hash-table-count htbl))
	 (arr (if fill-pointer
		  (make-array len :initial-element nil :fill-pointer t)
		  (make-array len :initial-element nil))))
	 
    (declare (type u-fixnum len))
    
    (loop for i of-type u-fixnum from 0
	  for x being the hash-values of htbl
	  do
	  (setf (aref arr i) x))
    
    (when sort-table-p
      (let ((sort-args (list arr (if sorted-by-fun sorted-by-fun #'<))))
	(when sorted-by-key
	  (setq sort-args (append sort-args (list :key sorted-by-key))))
	(setq arr (apply #'sort sort-args))))
    
    arr))

(defmethod convert-obj ((letter character) (to (eql :colour-code)) &key)
  ;; make this one into an array-access later
  "Returns a code which can be sent to C-functions as colour."
  
  (case letter
    (#\d +term-dark+)
    (#\w +term-white+)
    (#\s +term-slate+)
    (#\o +term-orange+)
    (#\r +term-red+)
    (#\g +term-green+)
    (#\b +term-blue+)
    (#\u +term-umber+)

    (#\D +term-l-dark+)
    (#\W +term-l-white+)
    (#\v +term-violet+)
    (#\y +term-yellow+)
    (#\R +term-l-red+)
    (#\G +term-l-green+)
    (#\B +term-l-blue+)
    (#\U +term-l-umber+)

    (otherwise
     (error "Fell through (CONVERT-OBJ ~s -> :colour-code)" letter)
     #-(or cmu sbcl)
     +term-white+)))

(defmethod convert-obj (code (to (eql :letter)) &key)
  "Returns a char for the appropriate colour-code."
  #||
  (warn "compare ~s vs ~s, ~a vs ~a, ~a vs ~a, ~a vs ~a, ~a"
	code +term-dark+
	(type-of code) (type-of +term-dark+) (char-code code) (char-code +term-dark+)
	(eq code +term-dark+) (eql code +term-dark+) (case code (+term-dark+ t) (t nil)))
  ||#

  (cond  ((eql code +term-dark+)    #\d) 
	 ((eql code +term-white+)   #\w)
	 ((eql code +term-slate+)   #\s) 
	 ((eql code +term-orange+)  #\o) 
	 ((eql code +term-red+)     #\r) 
	 ((eql code +term-green+)   #\g) 
	 ((eql code +term-blue+)    #\b) 
	 ((eql code +term-umber+)   #\u) 
	 
	 ((eql code +term-l-dark+)  #\D) 
	 ((eql code +term-l-white+) #\W) 
	 ((eql code +term-violet+)  #\v) 
	 ((eql code +term-yellow+)  #\y) 
	 ((eql code +term-l-red+)   #\R) 
	 ((eql code +term-l-green+) #\G) 
	 ((eql code +term-l-blue+)  #\B) 
	 ((eql code +term-l-umber+) #\U) 

	 (t
	  (error "Fell through (CONVERT-OBJ ~s -> :letter)" (char-code code))
	  #-cmu
	  #\w)))

(defun is-colour-code? (obj)
  "not safe"
  (and (non-negative-integer? obj)
       (< obj 16)))

(defsubst get-system-type () 'sdl)

#-compiler-that-inlines
(defmacro grid (x y)
  `(the fixnum (+ (the fixnum (* 256 ,y)) ,x)))

#+(and compiler-that-inlines (not sbcl))
(declaim (ftype (function (fixnum fixnum) fixnum) grid))
#+(and compiler-that-inlines sbcl)
(declaim (ftype (function (fixnum fixnum) (values fixnum &optional)) grid))

#+compiler-that-inlines
(defsubst grid (x y)
  (declare (type fixnum x y))
  (the fixnum (+ (the fixnum (* 256 y)) x)))

#-compiler-that-inlines
(defmacro grid-y (g)
  `(the fixnum (int-/ ,g 256)))

#+(and compiler-that-inlines (not sbcl))
(declaim (ftype (function (fixnum) fixnum) grid-y))

#+(and compiler-that-inlines sbcl)
(declaim (ftype (function (fixnum) (values fixnum &optional)) grid-y))

#+compiler-that-inlines
(defsubst grid-y (g)
  (declare (type fixnum g))
  (the fixnum (int-/ g 256)))

#-compiler-that-inlines
(defmacro grid-x (g)
  `(the fixnum (prog1 (mod ,g 256))))

#+(and compiler-that-inlines (not sbcl))
(declaim (ftype (function (fixnum) fixnum) grid-x))
#+(and compiler-that-inlines sbcl)
(declaim (ftype (function (fixnum) (values fixnum &optional)) grid-x))

#+compiler-that-inlines
(defsubst grid-x (g)
  (declare (type fixnum g))
  (the fixnum (prog1 (mod g 256))))

(defun game-data-path (fname)
  "Returns a pathname for fname."
  (merge-pathnames (pathname fname) *engine-config-dir*))

(defun load-game-data (fname)
  "Tries to load the data-file fname."
  (load (game-data-path fname)))


(defun read-pref-file (fname)
  "Tries to read a named preference file."
  (load-game-data fname))

(defun loadable-value (val)
  "Return VAL in a form that can be LOADed into Lisp again."
  (cond ((or (keywordp val) (stringp val) (characterp val)
	     (numberp val) (arrayp val))
	 val)
	((eq val nil)
	 nil)
	((or (symbolp val) (consp val))
	 (list 'quote val))
	(t
	 (error "Unknown how to make val ~s of type ~s loadable" val (type-of val)))))

;;; === Start message code

(defmethod init-message-system& ((handler message-handler))
  "Inits necessary settings for the message-system."
  (setf (msghandler.cur-max-col handler) (get-frame-width +message-frame+)))

(defmethod get-messages ((handler message-handler))
  "Returns shown messages."
  (msghandler.shown-msgs handler))

;; ok!
(defun print-message! (msg &key (attr +term-white+) (noise :none))
  "If msg is nil, things are flushed."
  ;;  (warn "going fu on ~s" (type-of str))

  (unless msg
    (error "Empty message!!!")
    #-(or cmu sbcl)
    (return-from print-message! nil))

  (unless (plusp (length msg))
    (return-from print-message! nil))

  (let ((handler *message-handler*)
	(mess (cons (make-message :text msg :attr attr :noise noise) nil)))
    (if (msghandler.incoming-msgs handler)
	(setf (cdr (last (msghandler.incoming-msgs handler))) mess)
	(setf (msghandler.incoming-msgs handler) mess)))
  ;;(warn "incoming ~s" (msghandler.incoming-msgs handler))

  t)

(defmethod advance-message-sys! ((handler message-handler-more))
  (setf (msghandler.state handler) :ready)
  
  ;; we advance one line!
  (incf (msghandler.cur-msg-row handler))
  ;; also reset col
  (setf (msghandler.cur-msg-col handler) 0)
  
  (let* ((win (get-window +message-frame+))
	 (max-rows (window.height win)))

    ;; if we're now past the bottom, clear all, and start again
    (when (>= (msghandler.cur-msg-row handler) max-rows)
      (clear-window win)
      (setf (msghandler.cur-msg-row handler) 0))
    ))

(defmethod advance-message-sys! ((handler message-handler-flow))
  (setf (msghandler.state handler) :ready)
  
  ;; we advance one line!
  (incf (msghandler.cur-msg-row handler))
  ;; also reset col
  (setf (msghandler.cur-msg-col handler) 0)
  
  (let* ((win (aref *windows* +message-frame+))
	 (max-rows (window.height win))
	 (max-cols (window.width win)))

    ;; if we're now past the bottom, copy the last n-1 lines one step up
    ;; and restart on the last line
    (when (>= (msghandler.cur-msg-row handler) max-rows)
      (loop for row from 1 below max-rows
	    do
	    (loop for col from 0 below max-cols
		  do
		  (setf (window-coord win +foreground+ col (1- row))
			(window-coord win +foreground+ col row))))
      (clear-row win 0 (1- max-rows))
      ;; bad hack!
      (paint-window win)
      (setf (msghandler.cur-msg-row handler) (1- max-rows)))
    
    ))


(defmethod try-printing-messages! ((handler message-handler-more))
  
  (when (and (eq (msghandler.state handler) :ready)
	     (consp (msghandler.incoming-msgs handler)))
    (let* ((win (aref *windows* +message-frame+))
	   ;;(handler *message-handler*)
	   (msg (first (msghandler.incoming-msgs handler)))
	   (msg-len (length (message.text msg))))

      ;; check if we have room
      (cond ((and (> (msghandler.cur-msg-col handler) 0) ;; if we're at scratch we go from there
		  (> (+ 8 msg-len (msghandler.cur-msg-col handler)) (msghandler.cur-max-col handler)))
	     ;;(warn "No room")
	     ;; we have no room
	     (output-string! win (msghandler.cur-msg-col handler) (msghandler.cur-msg-row handler) +term-l-blue+ "-more-")
	     (setf (msghandler.cur-msg-col handler) 0)
	     (setf (msghandler.state handler) :pause))

	    (t
	     ;;(warn ">Printing ~s" (message.text msg))
	     (output-string! win (msghandler.cur-msg-col handler) (msghandler.cur-msg-row handler)
			     (message.attr msg) (message.text msg))
	     (push msg (msghandler.shown-msgs handler))
	     (pop (msghandler.incoming-msgs handler))
	     (incf (msghandler.cur-msg-col handler) (1+ msg-len))
	     t))

      ;; make a noise?
      (when (and (message.noise msg) (not (eq (message.noise msg) :none)))
	(case (message.noise msg)
	  (:beep (warn "BEEP!"))
	  (t (warn "NOISE ~s" (message.noise msg)))))
      
      )))

(defmethod try-printing-messages! ((handler message-handler-flow))
  
  (when (and (eq (msghandler.state handler) :ready)
	     (consp (msghandler.incoming-msgs handler)))
    
    (let* ((win (aref *windows* +message-frame+))
	   ;;(handler *message-handler*)
	   (msg (first (msghandler.incoming-msgs handler)))
	   (msg-len (length (message.text msg))))

      ;; check if we have room
      (when (and (> (msghandler.cur-msg-col handler) 0) ;; if we're at scratch we go from there
		 (> (+ 1 msg-len (msghandler.cur-msg-col handler)) (msghandler.cur-max-col handler)))
	
	;; we have no room, but we should not give up, but progress.
	;;(warn "No room, we will try to reset, advance, and then print ~s" (message.text msg))
		
	(setf (msghandler.cur-msg-col handler) 0)
	(advance-message-sys! handler)
	)

      ;;(warn ">Print ~s (~s,~s)" (message.text msg) (msghandler.cur-msg-col handler) (msghandler.cur-msg-row handler))

      (output-string! win (msghandler.cur-msg-col handler) (msghandler.cur-msg-row handler)
		      (message.attr msg) (message.text msg))
      (push msg (msghandler.shown-msgs handler))
      (pop (msghandler.incoming-msgs handler))
      (incf (msghandler.cur-msg-col handler) (1+ msg-len))
      
      t)))



(defun flush-messages! (&key (forced nil) (reset-col nil))
  (declare (ignore forced))
  ;; not sure this is right, but here goes
  (let ((handler *message-handler*))
    (advance-message-sys! handler)
    (try-printing-messages! handler)
    (when reset-col
      (setf (msghandler.cur-msg-col handler) 0))
    nil))

(defun show-messages (&key (offset 0))
  (declare (ignore offset))
  (clear-window *cur-win*)
  (let ((last-line (get-last-window-line *cur-win*))
	(handler *message-handler*))
    ;;(warn "last line is ~s" last-line)
    (loop for i from 0 to (- last-line 2)
	  for msg in (msghandler.shown-msgs handler)
	  do
	  ;;(warn "Value ~s" msg)
	  (put-coloured-str! (message.attr msg) (message.text msg) 1 (1+ i))
	  ))
  (pause-last-line!))

;;; === End message code



(defun print-note! (msg &key (attr +term-white+) (row -1))
  "Prints a centred note on the last line."

  (let* ((win *cur-win*)
	 (row (if (or (< row 0) (< (window.height win) row))
		  (get-last-window-line win)
		  row))
	
	;; screws up somewhat when graphics is enabled
	(col (- (int-/ (get-last-window-column win) 2)
		(int-/ (length msg) 2))))

    (clear-row win 0 row)
    (output-string! win col row attr msg)
    (refresh-window win)
    (org.langband.ffi:c-flip-framebuffer)
    t))


(defun pause-at-line! (row &key msg attr)
  "Puts a 'press any key' prompt at the given row, returns after key has been pressed."
  (unless msg
    (setf msg "[Press any key to continue]"))
  (unless attr
    (setf attr +term-white+))
  (print-note! msg :attr attr :row row)
  
  (let ((read-char (read-one-character :left-mouse #\Space)))
    (clear-row *cur-win* 0 row)
    read-char))

(defun get-last-window-line (win)
  "Returns integer denoting last line in the window."
  (1- (window.height (get-window win))))

(defun get-last-window-column (win)
  "Returns integer denoting last line in the window."
  (1- (window.width (get-window win))))
    
(defun pause-last-line! (&key msg attr)
  "Calls pause-at-line! at the last line of *cur-win*"
  (pause-at-line! (get-last-window-line *cur-win*) :msg msg :attr attr))


(defun paint-gfx-image& (fname x y)
  "Paints the given gfx image with x,y as top-left coordinates.
If the file has not been loaded, it will be loaded."
  (let ((idx (find-image *variant* fname)))
    (unless idx ;; try to load then
      (setf idx (load-image& *variant* fname -1 0)))
    (when (and idx (<= 0 idx))
      (paint-gfx-image *cur-win* idx x y))))

(defun paint-gfx-image (window idx x y &optional (layer +foreground+))
  "Paints a graphic image with given index with x,y as top-left coordinates.
Can also specify what layer it should be at."
;;  (warn "Paint ~s ~s to ~s" idx (aref (variant.images *variant*) idx) window)

  (let* ((tile-wid 8) ;; hack
	 (tile-hgt 16) ;; hack
	 (img-wid (org.langband.ffi:c-get-image-width idx))
	 (img-hgt (org.langband.ffi:c-get-image-height idx))
	 (wid (int-/ img-wid tile-wid))
	 (hgt (int-/ img-hgt tile-hgt)))

;;    (when (= 0 (mod img-wid tile-wid))
;;      (incf wid))

;;    (warn "Paint ~d (~d,~d,~d,~d)" idx x y (+ x wid) (+ y hgt))
    
    (loop for j from 0 below hgt
	  do
	  (loop for i from 0 below wid
		do
		(let ((val (tile-paint-value idx (+ i (* j wid)))))
		  ;;(warn "~d at (~d,~d)" (+ i (* j wid)) (+ i x) (+ y j))
		  (setf (window-coord window layer (+ i x) (+ y j)) val)
		  ;; paint to (x+i, y+j) and paint tile-num (i + j*wid)
		  ;;(incf cnt)
		  )
		))
    ;; fresh coord
    (refresh-window window +winflag-delay-paint+)
    ))

(defun fill-area (window img tile-num x1 y1 x2 y2)
  "Paints tile number TILE-NUM from given image IMG, to the rectangle (x1,y1,x2,y2)
in window WINDOW."
  
  (let ((val (tile-paint-value img tile-num)))
    (loop for x from x1 below x2
	  do
	  (loop for y from y1 below y2
		do
		(setf (window-coord window +foreground+ x y) val)))))

(defun load-scaled-image& (fname idx wid hgt)
  (declare (ignore idx wid hgt))
  (warn "Scale-load image ~s" fname))


(defun find-image (variant fname)
  "Tries to find index for an image, NIL when there is none."
  (when variant
    (loop for i from 0
	  for x across (variant.images variant)
	  do
	  (when (equal fname x)
	    (return-from find-image i))))
  nil)

(defun load-image& (variant fname idx transcolour)
  "Tries to load the named image in given idx spot."

  (flet ((load-op (fname idx tr)
           (org.langband.ffi:load-gfx-image& fname idx tr)))

    (block negative-idx
      (when (minusp idx)
	(check-type variant variant)
	(let ((image-table (variant.images variant)))
	  (loop for i from 20 below (length image-table)
		for val = (aref image-table i)
		do
		(when (eq val nil)
		  (setf idx i)
		  (return-from negative-idx i)))
	  (error "Unable to find available space for image in image-table."))))

    (let ((trans (if (plusp transcolour)
		     (1+ transcolour)
		     0))
	  (path (cond ((stringp fname) ;; assume engine
		       (concatenate 'string *engine-data-dir* "graphics/" fname))
		      ((and (consp fname) (eq (car fname) 'engine-gfx))
		       (concatenate 'string *engine-data-dir* "graphics/" (second fname)))
		      ((and (consp fname) (eq (car fname) 'variant-gfx))
		       (concatenate 'string (variant.gfx-path variant) (second fname)))
		      (t
		       (error "Don't know how to handle fname ~s" fname)))))
      ;;(warn "Loading ~s as ~s" fname path)
      (unless (probe-file path) ;; check!
	(return-from load-image& nil))
      
      (load-op path idx trans))
    
    (when variant
      (register-image& variant fname idx))
    
    idx))

(defun load-image-spec& (variant idx spec)
  "Loads a more complex image-spec, see variants/vanilla/base.lisp for example."
  (cond ((and (stringp spec) (= (length spec) 0))
	 nil)
	
	((and (consp spec) (stringp (second spec)))
	 (when (plusp (length (second spec)))
	   (load-image& variant spec idx #xffffff00)))

	((and (consp spec) (eq (first spec) 'or))
	 ;; we have a list of alternatives.. the first working one wins
	 (dolist (x (cdr spec))
	   (let ((res (load-image& variant x idx #xffffff00)))
	     (cond ((integerp res)
		    (return-from load-image-spec& res))
		   ((eq nil res)
		    nil)
		   (t
		    (error "Unknown result from LOAD-IMAGE& ~s" res))))))
	
	(t
	 (error "Unknown image-spec ~s" spec))))

(defun delay (msecs)
  "Delays given amount of msecs."
  (declare (optimize (safety 0) (speed 3) (debug 0)
		     #+cmu (ext:inhibit-warnings 3)
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     ))
  (declare (type fixnum msecs))
  (let ((amount (/ msecs 1000.0)))
    #+(and linux allegro)
    (mp:process-sleep amount) ;; sleep is br0ken in allegro linux
    #-(and linux allegro)
    (sleep amount)))

(defun print-text! (col row colour text &key (end-col 80))
  "Don't call this if you need non-consing or fast operation."
  
  (let ((startcol col)
;;	(startrow row)
	(cur-col col)
	(cur-row row)
;;	(end-col 80)
	(splitted nil))

    (unless (consp text)
      (setf text (list text)))

    (dolist (piece text)
      (let ((cur-colour colour)
	    (text nil))

	(cond ((consp piece)
	       (setf cur-colour (first piece)
		     text (second piece)))
	      (t
	       (setf text piece)))
    
	(setf text (nsubstitute #\Space #\Newline text))
	(setf text (nsubstitute #\Space #\Return text))
	(setf text (nsubstitute #\Space #\Linefeed text))
	(setf splitted (mapcar #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed) x))
			       (split-seq-on text #\Space)))
	
	;;    (warn "text ~s -> ~s" text splitted)
	
	(flet ((print-word (word)
		 (let ((word-len (length word)))
		   ;;	     (warn "Printing word ~s at ~s,~s" word cur-col cur-row)
		   (put-coloured-str! cur-colour word cur-col cur-row)
		   (incf cur-col word-len)
		   (put-coloured-str! cur-colour " " cur-col cur-row)
		   (incf cur-col)
		   (1+ word-len))))

	  (loop for cur-word in splitted
		for i from 0
		do
		(when (plusp (length cur-word))
		  (cond ((< (+ cur-col (length cur-word) 1) end-col)
			 (print-word cur-word))
			(t
			 (incf cur-row)
			 (setf cur-col startcol)
			 (print-word cur-word)))))
	  )))
    
    (values cur-col cur-row)))


(defun quit-game& ()
  "Tries to clean up a few variables."
  (setf *variant* nil
	*level* nil
	*player* nil
	*dungeon* nil)
  (lbsys/garbage-collect :global t)
  ;;#+boys-eating-their-vegetables
  (finish-output cl:*error-output*)
  (finish-output cl:*standard-output*)
  (finish-output cl:*trace-output*)
  (case (get-system-type)
    ((x11 gcu sdl)
     (org.langband.ffi:c-cleanup-c-side&)
     (signal (make-condition 'langband-quit)))
    (otherwise
     (format t "~&Thanks for helping to test Langband.~2%")
     (signal (make-condition 'langband-quit))
     ))
  nil)
 
;;; === Code related to floors

(defun get-floor-type (id &key (variant *variant*))
  "Returns an object of type FLOOR-TYPE or NIL."
  ;; hack
  (when (typep id 'floor-type)
    (return-from get-floor-type id))
  
  (let ((table (variant.floor-types variant)))
    (gethash id table)))

(defun (setf get-floor-type) (floor id)
  "Adds a floor-type with given id to the appropriate table."
  (let ((table (variant.floor-types *variant*)))
    ;; something already in there with the id?
    (multiple-value-bind (old-obj found-p)
	(gethash id table)
      (when found-p
	(warn "Replacing old floor ~s with id ~s, with new floor ~s"
	      old-obj id floor)))
    (setf (gethash id table) floor)))

(defun define-floor-type (id name &key mimic numeric-id flags text-sym gfx-sym)
  "Defines a floor-type and registers it.  The floor is returned."

  (unless (or (verify-id id)
	      (integerp id)) ;; remove integer part later
    (warn "floor-id ~s not valid" id)
    (return-from define-floor-type nil))

  (let ((ftype (make-instance 'floor-type :id id
			      :name name
			      :mimic mimic)))

    (cond ((non-negative-integer? gfx-sym)
	   (setf (gfx-sym ftype) gfx-sym))
	  (gfx-sym
	   (error "Uknown gfx-sym value ~s for floor ~s" gfx-sym id)))
    
    (cond ((non-negative-integer? text-sym)
	   (setf (text-sym ftype) text-sym))
	  (text-sym
	   (error "Uknown text-sym value ~s for floor ~s" text-sym id)))

    (when (integerp numeric-id)
      (setf (floor.numeric-id ftype) numeric-id))
    
    (when (integerp flags)
      (setf (floor.flags ftype) flags))
    
    (setf (get-floor-type id) ftype)
    
    (when (positive-integer? (floor.numeric-id ftype))
      (setf (get-floor-type (floor.numeric-id ftype)) ftype))
    
    ftype))


;;; === end floor code

;;; == stat-related code

(defun define-character-stat (symbol name &key abbreviation bit-flag
			      number data positive-desc negative-desc)
  "Defines and registers a stat with the current variant."

  (let ((the-stat (make-instance 'character-stat :symbol symbol :name name))
	(variant *variant*))
    
    (check-type variant variant)

    (cond ((non-negative-integer? number)
	   (setf (stat.number the-stat) number))
	  (t
	   (signal-condition 'illegal-char-stat-data :id name
			     :desc (format nil "Unknown number ~s for stat" number))))

    (cond ((positive-integer? bit-flag)
	   (setf (stat.bit-flag the-stat) bit-flag))
	  (t
	   (signal-condition 'illegal-char-stat-data :id name
			     :desc (format nil "Unknown bit-flag ~s for stat" bit-flag))))
    
    
    (when abbreviation
      (setf (stat.abbreviation the-stat) abbreviation))

    (when (stringp positive-desc)
      (setf (stat.positive-desc the-stat) positive-desc))
    
    (when (stringp negative-desc)
      (setf (stat.negative-desc the-stat) negative-desc))
    
    (when (consp data)
      (setf (stat.data the-stat) data)
      ;; let's hack things better

      (let ((field-list '()))
	(dolist (list data)
	  (push (make-stat-field :lower (car list) :upper (cadr list)
				 :data (loop for (first second) on (cddr list) by #'cddr
					     collect (cons first second)))
		field-list))
	(setf (stat.fields the-stat) (nreverse field-list))
;;	(warn "Fields: ~s" (stat.fields the-stat))
	))

      

    ;; now let's add it

    (pushnew the-stat (variant.stats variant) :test #'eq :key #'stat.symbol)

    (setf (variant.stats variant) (stable-sort (variant.stats variant) #'< :key #'stat.number)) 
    
    the-stat))

(defmethod make-stat-array ((variant variant))
  (make-array (variant.stat-length variant) :initial-element 0))

(defmethod is-stat-array? ((variant variant) obj)
  (and (arrayp obj)
       (= (length obj) (variant.stat-length variant))
       ;; add more?
       ))

;;; The stat-functions below should be checked and possible be improved
;;; now that there is a class/object and not just random tables

(defun get-stat-obj (variant key)
  (let ((stats (variant.stats variant)))
    (etypecase key
      (number (elt stats key))
      (symbol (find key stats :key #'stat.symbol))
      )))

(defun get-stat-name-from-num (num)
  "Improve later.."
  (let* ((variant *variant*)
	 (stat-obj (elt (variant.stats variant) num)))
    
    (check-type stat-obj character-stat)
    
    (stat.abbreviation stat-obj)))

(defun get-stat-name-from-sym (sym)
  "Improve later.."
  (let* ((variant *variant*)
	 (stat-obj (find sym (variant.stats variant) :key #'stat.symbol)))
    
    (check-type stat-obj character-stat)
    
    (stat.abbreviation stat-obj)))

(defun get-stat-num-from-sym (sym)
  "Improve later.."
  (let* ((variant *variant*)
	 (stat-obj (find sym (variant.stats variant) :key #'stat.symbol)))
    
    (check-type stat-obj character-stat)
    
    (stat.number stat-obj)))

(defun is-legal-stat? (variant sym)
  "Checks if SYM names a legal stat for the variant."
  (find sym (variant.stats variant) :key #'stat.symbol))

(defun gsdfn (table num)
  (svref table num))

(defun build-stat-table-from-symlist (variant symlist)
;;  (warn "Building stat-table of ~s" symlist)
  (let ((table (make-stat-array variant)))
    (dolist (i symlist)
      (setf (svref table (get-stat-num-from-sym (car i)))
	    (cadr i)))
    table))

(defmethod get-decor-name ((decor decor))
  (decor.name decor))

(defmethod get-decor-name ((decor active-trap))
  (trap.name (decor.type decor)))

(defmethod gfx-sym ((obj active-trap))
  (gfx-sym (decor.type obj)))

(defmethod text-sym ((obj active-trap))
  (text-sym (decor.type obj)))

(defun is-trap? (obj)
  "Checks if OBJ is of type ACTIVE-TRAP."
  (typep obj 'active-trap))

(defun define-trap-type (id name &key gfx-sym text-sym
			 effect min-depth max-depth rarity)
  "Defines and registers a trap-type."
  (unless (verify-id id)
    (warn "Trap-id ~s not valid" id)
    (return-from define-trap-type nil))
  
  (let ((trap-obj (make-instance 'trap-type :id id :name name
				 :min-depth min-depth :max-depth max-depth
				 :rarity rarity
				 ))
	(table (variant.traps *variant*)))

    (cond ((non-negative-integer? gfx-sym)
	   (setf (gfx-sym trap-obj) gfx-sym))
	  (t
	   (error "Unknown gfx-sym value ~s for trap ~s" gfx-sym id)))

    (cond ((non-negative-integer? text-sym)
	   (setf (text-sym trap-obj) text-sym))
	  (t
	   (error "Unknown text-sym value ~s for trap ~s" text-sym id)))
    
    
;;    (warn "Effect is ~s ~s ~s" effect (functionp effect) (compiled-function-p effect))
    (when (functionp effect)
      ;; we assume it is not compiled
      (setf effect (compile nil effect))
      (when (functionp effect)
	(setf (trap.effect trap-obj) effect)))
    
    (setf (gethash id table) trap-obj)
    
    trap-obj))

(defmacro trap-effect (arguments &body body)
  (assert (= (length arguments) 4))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defun has-frame? (key type)
  (if (= (org.langband.ffi:c-has_frame key type) 1)
      t
      nil))

(defun get-frame-gfx-tiles? (key type)
  (if (= (org.langband.ffi:c-get_frame-gfx-tiles key type) 1)
      t
      nil))

(defun is-frame-shown? (variant frame)
  "Checks if the given frame is shown."
  (declare (ignore variant))
  (let ((shown? nil)
	(win nil))
    (unless (eq frame nil)
      (setf win (get-window frame)))
    (when win
      (setf shown? (and (not (window.disabled? win)) (window.visible? win))))
    ;;(warn "basic frame: ~s" shown?)
    shown?))

(defun basic-frame-shown? (variant)
  "Checks if the basic frame is shown."
  (is-frame-shown? variant +charinfo-frame+))


(defun install-window-font! (window theme)
  "Tries to install a font in the window, might fall back on theme value."  

  (flet ((get-font-values (font)
	   (let ((fontname "unknown")
		 (size 16)
		 (style 0))
	     (cond ((consp font)
		    (setf fontname (first font))
		    (when (nonboolsym? (second font))
		      (ecase (second font)
			(normal nil)
			(bold (bit-flag-add! style #x01))
			(italic (bit-flag-add! style #x02))))
		    (when (positive-integer? (third font))
		      (setf size (third font))))
		   ((stringp font)
		    (setf fontname font))
		   (t
		    (error-condition 'illegal-ui-theme-data :id (theme.key theme)
				     :desc (format nil "Unknown font-value ~s in window ~s" font window))))
	     (values fontname style size))))

    (let ((retval -1))
      (multiple-value-bind (fontname size style)
	  (get-font-values (window.font window))

	(unless (stringp fontname)
	  (error-condition 'illegal-ui-theme-data :id (theme.key theme)
			   :desc (format nil "Fontname is not a string, but ~s in window ~s" fontname window)))
	
	(setf fontname (concatenate 'string *engine-data-dir* "fonts/" fontname))
	;;(warn "A. Font ~s,~s,~s for ~s" fontname size style (window.id window))
	(setf retval (org.langband.ffi:c-install-font-in-frame! (window.num-id window) fontname size style))

	;;(warn "A. retval is ~s" retval)
	    
	(when (minusp retval) ;; something screwed up, try default font
	  (multiple-value-setq (fontname size style)
	    (get-font-values (theme.font theme)))
	    
	  (setf fontname (concatenate 'string *engine-data-dir* "fonts/" (theme.font theme)))
	  
	  ;;(warn "B. Font ~s,~s,~s for ~s" fontname size style (window.id window))
	  (setf retval (org.langband.ffi:c-install-font-in-frame! (window.num-id window) fontname size style))
	  ;;(warn "B. retval is ~s" retval)
	  )

	;; return if it was success or not
	(if (minusp retval)
	    nil
	    t)))))


  
(defun update-term-sizes! ()
  "Calls the C-side to get the accurate sizes of windows."

  ;; first ensure that we have legal frames
  (dotimes (i +predefined-frames+)
    (let ((var (aref *windows* i)))
      
      (unless var
	;; check if there should be one
	(when (has-frame? i +frametype-predefined+)
	  (warn "Must create a dummy window, lacking a themed version.")
	  (setf var (make-instance 'window :num-id i)))
	(setf (aref *windows* i) var))))

  
  ;; first install fonts
  (dotimes (frame +predefined-frames+)
    (when-bind (win (aref *windows* frame))
      (install-window-font! win *current-ui-theme*)))
  
  
  ;;(warn "Update")
  (dotimes (i +predefined-frames+)
    (let ((var (aref *windows* i)))
      
      ;; we need the font-results
      (when var
	;; we need to update our info
	(let ((wid (org.langband.ffi:c-get-frame-tile-width i +frametype-predefined+)))
	  ;;(warn "wid is ~s for ~s" wid var)
	  (setf (window.tile-width var) wid))
	(let ((hgt (org.langband.ffi:c-get-frame-tile-height i +frametype-predefined+)))
	  ;;(warn "hgt is ~s for ~s" hgt var)
	  (setf (window.tile-height var) hgt))
	;;(describe var)
	)))

  ;;(warn "Calculate")
  ;; now do the calculations!
  (handler-case
      (establish-ui-theme-size-calculations& *current-ui-theme*)
    (illegal-ui-theme-data (co)
      (warn "Failed to calculate working sizes for subwindows for theme ~a: ~a"
	    (illegal-data.id co) (illegal-data.desc co))
      (return-from update-term-sizes! nil)))

  ;;(warn "Recalc")
  (org.langband.ffi:c-recalculate-frame-placements! 0)

  ;;(warn "Read")
  
  (dotimes (i +predefined-frames+)
    (let ((var (aref *windows* i)))
      
      (unless var
	;; check if there should be one
	(when (has-frame? i +frametype-predefined+)
	  (warn "Must create a dummy window, lacking a themed version.")
	  (setf var (make-instance 'window :num-id i))))
      
      ;;(warn "Checking ~s" var)
      ;; ok, we have a frame we need more info about
      (when var
	;; we need to update our info
	(setf (window.width var)       (org.langband.ffi:c-get-frame-columns i +frametype-predefined+)
	      (window.height var)      (org.langband.ffi:c-get-frame-rows i +frametype-predefined+)
	      ;;(window.tile-width var)  (org.langband.ffi:c-get-frame-tile-width i +frametype-predefined+)
	      ;;(window.tile-height var) (org.langband.ffi:c-get-frame-tile-height i +frametype-predefined+)
	      (window.gfx-tiles? var)  (get-frame-gfx-tiles? i +frametype-predefined+))

	;;(describe var)
	;;(assert (plusp (window.width var)))
	;;(assert (plusp (window.height var)))
	#+never
	(warn "window ~s has size [~d,~d,~d,~d] and gfx ~s" i
	      (window.width var) (window.height var)
	      (window.tile-width var) (window.tile-height var)
	      (window.gfx-tiles? var))

	(setf (aref *windows* i) var))
      ))

  t)



(defun get-frame-width (&optional (term -1))
  "deprecated, use get-window + window.width"
  (cond ((typep term 'window)
	 (window.width term))
	((>= term 0)
	 (window.width (aref *windows* term)))
	(t
	 (window.width *cur-win*))))


(defun get-frame-height (&optional (term -1))
  "deprecated, use get-window + window.height"
  (cond ((typep term 'window)
	 (window.height term))
	((>= term 0)
	 (window.height (aref *windows* term)))
	(t
	 (window.height *cur-win*))))

(defun engine-allows-gfx-tiles? ()
  "Return T if the running program uses gfx-tiles?"
  (eq (get-system-type) 'sdl))

(defun window-allows-gfx-tiles? (&optional (term -1))
  "Check if a given term or window uses gfx-tiles"
  (cond ((typep term 'window)
	 (window.gfx-tiles? term))
	((>= term 0)
	 (window.gfx-tiles? (aref *windows* term)))
	(t
	 (window.gfx-tiles? *cur-win*))))

(defun graphical-map? ()
  "Is the current map allowing graphical tiles?"
  (window-allows-gfx-tiles? *map-frame*))

(defun use-images? ()
  "Can we use inline images?"
  (if (eq (get-system-type) 'sdl)
      t
      nil))

;;; to have empty place-holders
#-image-support
(defun load-scaled-image& (fname idx wid hgt)
  (declare (ignore fname idx wid hgt))
  nil)

(defun image-exists? (name)
  "Checks if given filename for an image actually exists."
  (let ((fname (concatenate 'string *engine-data-dir* "graphics/" name)))
    (probe-file fname)))

(defun put-coloured-line! (colour text col row)
  "Erases rest of line and puts a string."
  (clear-row *cur-win* col row +max-wincol+ +foreground+) ;; hack
  (unless (or (eq text nil) (and (stringp text) (= (length text) 0)))
    (output-string! *cur-win* col row colour text)))

;; needs to print empty stuff?
(defun put-coloured-str! (colour text col row)
  (output-string! *cur-win* col row colour text)
  (values))

(defun output-string! (win col row colour text)
  ;;(warn "Writing text ~s to %d,%d in ~s" text col row (window.id win))
  ;; this is _slow_

  (when (and (integerp win) (>= win 0))
    (setf win (aref *windows* win)))

  (when (or (not (non-negative-integer? colour))
	    (>= colour 256))
    (error "Found very odd colour value ~s in output-string! [~s]" colour text))
  
  (let ((flag +winflag-delay-paint+))
    (loop for x across text
	  for i from col
	  do
	  (setf (window-coord win +foreground+ i row) (text-paint-value colour x))
	  (paint-coord win i row flag))

    (flush-coords win col row (length text) 1)
    ))


(defun texture-background! (win fname alpha)
  "Tries to texture the background of WIN with image-data from FNAME."
  (declare (ignore alpha))
  (let ((idx nil)
	(var *variant*)
	(num-win (if (integerp win) win (window.num-id win)))
	(window (if (integerp win) (aref *windows* win) win)))
    
    (when (and (stringp fname) (plusp (length fname)))
      (setf idx (find-image var fname))
      (when (or (eq idx nil) (minusp idx))
	(setf idx (load-image& var fname -1 0))
	(when (and (integerp idx) (minusp idx))
	  (setf idx nil))))

    ;;(warn "Assigning background ~s ~s to window ~s" fname idx (window.id window))
    (setf (window.background window) idx)
    ;; c-side needs negative value for bad values
    (org.langband.ffi:c-add-frame-bg! num-win (if (eq idx nil) -1 idx))))


(defun switch-to-full-frame& ()
  (loop for i from 1 below +max-frames+
	do
	(deactivate-window i))
  (activate-window +full-frame+)
  (paint-window +full-frame+))

;:; also *map-frame*
(defvar *regular-frameset* (list +message-frame+ +charinfo-frame+ +infodisp-frame+
				 +misc-frame+ +inv-frame+ +tiledfields-frame+))

(defun %switch-in-window (win)
  (when (non-negative-integer? win)
    (setf win (aref *windows* win)))
  
  (when (is-window? win)
    (unless (window.disabled? win)
      (activate-window win)
      (paint-window win))))

;; fix to no-cons later
(defun switch-to-regular-frameset& ()
  (deactivate-window +full-frame+)

  (%switch-in-window *map-frame*)
  (dolist (i *regular-frameset*)
      (%switch-in-window i))
  t)

(defun invoke-in-dialogue (fun)
  (cond ((eql (window.num-id *cur-win*) +dialogue-frame+)
	 (funcall fun))
	(t
	 (unwind-protect
	      (progn
		(deactivate-window +charinfo-frame+)
		(deactivate-window *map-frame*)
		(deactivate-window +tiledfields-frame+)
		(activate-window +dialogue-frame+)
		(paint-window +dialogue-frame+)
		(with-frame (+dialogue-frame+)
		  (funcall fun)))

	   (progn
	     (clear-window +dialogue-frame+)
	     (deactivate-window +dialogue-frame+)
	     
	     (when-bind (win (get-window +charinfo-frame+))
	       (unless (window.disabled? win)
		 (activate-window win)
		 (paint-window win)))
	     
	     (when-bind (win (get-window +tiledfields-frame+))
	       (unless (window.disabled? win)
		 (activate-window win)
		 (paint-window win)))
	     
	     
	     (when-bind (win (get-window +infodisp-frame+))
	       (unless (window.disabled? win)
		 (activate-window win)
		 (paint-window win)))

	     (when-bind (win (get-window *map-frame*))
	       (activate-window win)
	       (paint-window win))
	     
	     )))
	))

(defvar *key-operations* (make-hash-table :test #'equal))

(defun define-key-operation (key operation)
  "defines a key-operation which later can be bound."
  ;; to trigger warnings early
;;  (when (functionp operation)
;;    (setf operation (compile nil operation)))
  (setf (gethash key *key-operations*) operation))

(defun find-key-operation (key)
  "returns an operation or NIL."
  (gethash key *key-operations*))

(defun get-key-operations ()
  "Returns an alist of keys and their operations."

  (let ((collected nil))
    (maphash #'(lambda (k v)
		 (push (cons k v) collected))
	     *key-operations*)
    (nreverse collected)))

(defun define-key-table (name)
  "Returns a key-table."
  (declare (ignore name))
  (make-hash-table :test #'eql))

(defun make-inner-key-table ()
  (make-hash-table :test #'equal))


				   
(defvar *current-key-table* nil)
(defvar *angband-keys* (define-key-table "angband"))
(defvar *roguelike-keys* (define-key-table "angband-roguelike"))

(defun use-key-table (key)
  (setf *current-key-table*
	(cond ((or (eq key :rogue) (eq key :roguelike))
	       *roguelike-keys*)
	      ((or (eq key :angband) (eq key :normal))
	       *angband-keys*)
	      ((hash-table-p key) key)
	      (t
	       (error "Unknown key-table ~s" key)))))

(defun define-keypress (key-table where key operation)
  "Defines a keypress and ties it to the appropriate
operation."

  (let ((table (gethash where key-table))
	(oper (find-key-operation operation)))

    (unless oper
      (warn "Unable to find operation '~a' in ~a for key '~a'"
	    operation (cons key-table where) key)
      #-cmu
      (return-from define-keypress nil))
    
    (unless table
      (setf table (make-inner-key-table))
      (setf (gethash where key-table) table))
    
    ;; would be faster to use oper directly, but when using operation it's easier
    ;; to update a key-operation without updating key-entries. 
    (setf (gethash key table) operation) 
    
    ))
    

(defun check-keypress (table key)
  "checks a keypress vs the given table"
  (let ((wanted-sym (gethash key table))
	(poss-fun nil))
    (check-type wanted-sym symbol)
    (setf poss-fun (find-key-operation wanted-sym))
    (assert (or (eq poss-fun nil) (typep poss-fun 'function)))
    poss-fun))



(defun register-image& (variant fname idx)
  "Registers an image with a variant."
  (setf (aref (variant.images variant) idx) fname))


(defun %print-imagelist ()
  (loop for i from 0
	for x across (variant.images *variant*)
	do
	(format t "~&~d. ~a~%" i x)))

(defun set-cursor-visibility (arg)
  "Should the cursor be visible?"
  arg)

(defun set-cursor-to (win cursor x y)
  "Sets the specified cursor to x,y in the given window.  The
cursor argument specifies the type of cursor.  Types are e.g
:input and :crosshair."

  ;;(warn "Setting ~s cursor to ~s,~s" cursor x y)
  
  (let ((painted nil)
	(gfx-use? nil))
    (when (integerp win)
      (setf win (aref *windows* win)))

    (setf gfx-use? (window.gfx-tiles? win))
    
    (case cursor
      (:bad-crosshair
       (if gfx-use?
	   (setf (window-coord win +effect+ x y) (tile-paint-value +tilefile-crosshairs+ 0))
	   (setf (window-coord win +effect+ x y) (text-paint-value +term-red+ #\X)))
       (setf painted t))
      
      (:legal-crosshair
       (if gfx-use?
	   (setf (window-coord win +effect+ x y) (tile-paint-value +tilefile-crosshairs+ 1))
	   (setf (window-coord win +effect+ x y) (text-paint-value +term-l-green+ #\X)))
       (setf painted t))
      (otherwise nil))

    (when painted
      (paint-coord win x y)
      (flush-coords win x y 1 1))
    
  
    cursor))

;; haven't I implemented this somewhere else????
(defun get-direction-from-coord-diff (diff-x diff-y)
  "Return a numbered direction from coordinate diff."
  (cond ((plusp diff-x)
	 (cond ((plusp diff-y) 3)
	       ((minusp diff-y) 9)
	       (t 6)))
	((minusp diff-x)
	 (cond ((plusp diff-y) 1)
	       ((minusp diff-y) 7)
	       (t 4)))
	(t
	 (cond ((plusp diff-y) 2)
	       ((minusp diff-y) 8)
	       (t 5)))))
		
(defun define-visual-projectile (id &key gfx-path text-path) ;; add rest later
  (assert (verify-id id))
  
  (let ((visual-effect (make-instance 'visual-projectile :id id)))

    (when (arrayp gfx-path)
      (setf (projectile.gfx-path visual-effect) gfx-path))
    (when (arrayp text-path)
      (setf (projectile.text-path visual-effect) text-path))

    (setf (gethash id (variant.visual-effects *variant*)) visual-effect)
    
    visual-effect))

(defvar *settings* (make-hash-table :test #'equal)) ;; hack

(defun copy-hash-table (htbl)
  "Tries to return a copy of the hash-table.  Some values might
be shared."
  (let ((table (make-hash-table :test (hash-table-test htbl))))
    (maphash #'(lambda (k v)
		 (setf (gethash k table) v))
	     htbl)
    table))

(defun setting-lookup (setting key &optional default)
  "Looks up a setting in the setting-table and if the setting is not found it
returns the optional default value. "
  (assert (hash-table-p setting))
  (assert (stringp key))
  (multiple-value-bind (value found-p)
      (gethash key setting)
    (unless found-p
      (setf value default))

    ;;(warn "~s -> ~s (~s)" key value default)
    
    value))

(defun (setf setting-lookup) (value setting key)
  "Sets the setting to some value."
  (assert (hash-table-p setting))
  (assert (stringp key))

  (setf (gethash key setting) value))
  
(defun get-settings-obj (id)
  "Returns a settings-table by id."
  ;;(warn "Getting settings ~s" id)
  (gethash id *settings*))

(defun define-settings (id &rest settings)
  "Defines a settings object with an id."
  (assert (consp id))
  
  (let ((table nil))
    
    (when (cdr id)
      (let ((parent (get-settings-obj (second id))))
	(cond (parent
	       (setf table (copy-hash-table parent)))
	      (t
	       (warn "Unable to find settings: ~s" (second id))))))

    (unless table
      (setf table (make-hash-table :test #'equal)))
    
    (loop for (k v) on settings do
	  (setf (gethash k table) v))

    ;;(warn "Registering settings at ~s" (first id))
    (setf (gethash (first id) *settings*) table)
    
    table))

(defun define-visual-state (key priority &key desc gfx-sym)
  "Defines and registers a visual state in the variant."
  (let ((state (make-instance 'visual-state)))
    (setf (visual-state.key state) key
	  (visual-state.priority state) priority
	  (visual-state.desc state) desc
	  (gfx-sym state) gfx-sym)

    (pushnew state (variant.visual-states *variant*) :key #'visual-state.key)

    (setf (variant.visual-states *variant*) (sort (variant.visual-states *variant*) #'<
						  :key #'visual-state.priority))
						  
    state))

(defun modify-visual-state! (variant key new-value)
  "Activates the display of a visual state."
  (dolist (i (variant.visual-states variant))
    (when (eq key (visual-state.key i))
      (setf (visual-state.active i) new-value)
      ;;(warn "changed ~s to ~s" key new-value)
      (return-from modify-visual-state! t)))
  nil)

(defun get-visual-state (variant pos)
  "Asks for the state at visual position POS, returns
a state or NIL."

  (let ((states (variant.visual-states variant))
	(count 0))
    (loop for st in states 
	  do
	  (when (visual-state.active st)
	    (when (= count pos)
	      (return-from get-visual-state st))
	    (incf count)))
    nil))
    

(defun display-visual-states (variant)
  "Tries to display the necessary visual states."
  ;; first do infodisp
  (when (eq (get-system-type) 'sdl)
    (let ((win (aref *windows* +infodisp-frame+))
	  (states (variant.visual-states variant)))
    
      (block infodisp-display 
	(when (and (not (window.disabled? win))
		   (window.visible? win))
	  ;;(warn "visuals")
	  (let* ((wid (window.width win))
		 (hgt (window.height win))
		 ;;(max (* wid hgt))
		 (cur-row 0)
		 (cur-col 0))
	
	    (dotimes (j hgt)
	      (dotimes (k wid)
		(setf (window-coord win +foreground+ k j) 0)))
	
	    (dolist (i states)
	      (when (visual-state.active i)
		(setf (window-coord win +foreground+ cur-col cur-row) (gfx-sym i))
		(incf cur-col)
		(when (= cur-col wid)
		  (setf cur-col 0
			cur-row (1+ cur-row)))
		(when (= cur-row hgt)
		  (return-from infodisp-display t))
		))
	    
	    (paint-window win)
	    )))
      ))

  (when (eq (get-system-type) 'gcu)

    (let* ((win (aref *windows* +misc-frame+))
	   (maxlen (- (window.width win) 15)) ;; what we can use
	   (states (variant.visual-states variant))
	   (cur-col 0)
	   (len 0))

      (loop for i from 0 below maxlen
	    do
	    (setf (window-coord win +foreground+ i 0) 0))
    
      (loop for i from 0
	    for st in states
	    do
	    (when (visual-state.active st)
	      (setf len (length (visual-state.desc st)))
	      (win/format win cur-col 0 +term-l-green+ "~a, " (visual-state.desc st))
	      (incf cur-col (+ 2 len))
	  
	      ))
      (paint-window win)))
  
  t)

(defvar *defsettings* (make-hash-table :test #'equal))

(defun default-setting (id)
  (gethash id *defsettings*))

(defun (setf default-setting) (value id)
  (setf (gethash id *defsettings*) value))

(defun idx-value (idx)
  "Returns value stored in an index on the C-side."
  (org.langband.ffi:c-get-idx-value idx))

(defun (setf idx-value) (value idx)
  "Returns value stored in an index on the C-side."
  (cond ((integerp value)
	 (org.langband.ffi:c-set-idx-intvalue idx value))
	((stringp value)
	 (org.langband.ffi:c-set-idx-stringvalue idx value))
	(t
	 (error "Unsupported value for idx-value: ~s" value))))


;;; === Deprecated functions

;; add deprecated stuff here
