;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: sys.lisp - Various system-related code
Copyright (c) 2001-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defvar *dumps-directory* "dumps/" "Where should various debug-dumps go?")

;; ripped from clocc/port
(defun lbsys/getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+cmu (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                    :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'getenv var)))


(defun lbsys/make-sure-dirs-exist& (dir &key (verbose nil))
  "mostly a call to ENSURE-DIRECTORIES-EXIST,
but stops errors from floating out.. returns NIL instead."
  (let ((the-path (merge-pathnames (etypecase dir
                                     (pathname dir)
                                     (string (pathname dir))))))
    (handler-case
        (ensure-directories-exist the-path 
                                  :verbose verbose)
      (file-error (co)
        (warn "Something went wrong [~a] during directory creation [~s], returning NIL."
              co the-path)
        nil))))

(defun lbsys/ensure-dir-name (str)
  "Makes sure the str has a / suffix"
  (unless str
    (error "Illegal directory ~s given to ensure-dir-name" str))

  (let ((last-char (char str (1- (length str)))))
    
    (if (or (eql last-char #\/)
	    (eql last-char #\\))
	str
	(concatenate 'string str "/"))))
    
(defun home-langband-path ()
  "Returns the path (as a string, not pathname) to the langband-dir in the home-dir."
  #-win32
  (ignore-errors
    (let ((home-dir (lbsys/getenv "HOME")))
      (when (and home-dir (length home-dir))
        (setq home-dir (lbsys/ensure-dir-name home-dir))
        ;;      (print home-dir)
        (concatenate 'string home-dir ".angband/langband/"))))
  #+win32
  "c:/")

(defun lbsys/get-current-directory ()
  "The default directory."
  #+allegro (excl:current-directory)
  #+clisp (ext:default-directory)
  #+cmu (ext:default-directory)
;;  #+sbcl (sb-ext:default-directory)
  #+cormanlisp (ccl:get-current-directory)
  #+lispworks (hcl:get-working-directory)
  #+lucid (lcl:working-directory)
  #+sbcl (truename ".")
  #-(or allegro sbcl clisp cmu cormanlisp lispworks lucid) (truename "."))

(defun lbsys/class-name (obj)
  "Returns a string with class name for an object."
  (cl:class-name (cl:class-of obj)))

#-langband-release
(defun pr-ht (htbl)
  (maphash #'(lambda (k v) (format t "~&{~s} -> {~s}~%" k v)) htbl))

#-langband-release
(defun all-funs (pck)
  (let ((pack (find-package pck))
	(syms nil))
    (do-symbols (s pack)
      (when (and (eq (symbol-package s) pack)
		 (fboundp s))
	(pushnew s syms)))
    (sort syms #'string< :key #'symbol-name)))

(defun lbsys/directory (path)
  "This is a wrapper to make 'old' behaviour work for DIRECTORY, given that /foo/ returns
all elements in the dir /foo/"
  #-allegro
  (cl:directory (merge-pathnames (make-pathname :name :wild :type :wild :version :wild) (pathname path)))
  #+allegro
  (cl:directory (pathname path)))


#+sbcl
(defmacro lbsys/time (form)
  `(%lbsys/time (lambda () ,form)))

#-sbcl
(defmacro lbsys/time (form)
  form)

#+sbcl
(defun %lbsys/time (fun)
  (declare (type function fun))
  (let (old-bytes-consed
	new-bytes-consed
	cons-overhead
	diff)
    ;; Calculate the overhead...
    (setq old-bytes-consed (sb-ext:get-bytes-consed))
    ;; Do it a second time to make sure everything is faulted in.
    (setq old-bytes-consed (sb-ext:get-bytes-consed))
    (setq new-bytes-consed (sb-ext:get-bytes-consed))
    (setq cons-overhead (- new-bytes-consed old-bytes-consed))
    ;; Now get the initial times.
    (setq old-bytes-consed (sb-ext:get-bytes-consed))
 
    (multiple-value-prog1
	;; Execute the form and return its values.
	(funcall fun)
      (setq new-bytes-consed (sb-ext:get-bytes-consed))
      (setq diff (- new-bytes-consed old-bytes-consed cons-overhead))
      (when (plusp diff)
	(format *trace-output* "~&Consing: ~S.~%" (max diff 0))
	)
      )))

;;(trace text-to-ascii)
#+allegro
(let ((counter 0))
  (defun %lbsys/dump-profile-to-file ()
    (let ((pname (concatenate 'string *dumps-directory* "prof." (format nil "~a" (incf counter)) ".dump")))
      (with-open-file (s (pathname pname)
			 :direction :output
			 :if-exists :supersede)
	(prof:show-flat-profile :stream s :verbose t)
	(prof:show-call-graph :stream s :verbose t)
	;;(let ((cl:*standard-output* s))
	;;  (prof:show-call-counts :count 300))
	))))

#+allegro
(defmacro lbsys/tricky-profile (expr type)
  `(prof:with-profiling (:type ,type) ;; :count 300)
    (prog1
	,expr
      (%lbsys/dump-profile-to-file))))

#-allegro
(defmacro lbsys/tricky-profile (expr type)
  (declare (ignore type))
  `(time ,expr))

(defun lbsys/garbage-collect (&key (global nil))
  "Tries to enforce a garbage collect."
  (declare (ignore global))
  #+cmu (ext:gc)
  #+allegro (excl:gc t)
  #+clisp (ext:gc)
  #+sbcl (sb-ext:gc)
  #+lispworks (hcl:normal-gc)
  #-(or allegro cmu clisp lispworks sbcl)
  (warn "explicit GC not implemented."))

(in-package :org.langband.datastructures)

;; donated code by W Newman

;;; a helper function for any failed-assertion-ish handlers
(defun %backtrace-when-batch ()
  #+cmu (declare (optimize (speed 1))) ; to suppress optimization notes
  #+cmu (when ext:*batch-mode*
          (format *error-output* "~&~%backtrace:~%")
          (debug:backtrace 128 *error-output*)))
#-lispworks
(declaim (ftype (function (&optional t &rest t) nil) oops))
#-lispworks
(declaim (ftype (function (&optional t) nil) oops-missing-arg))
(defun oops (&optional explanation
		       ;; Under SBCL's debugger, it can be hard or
		       ;; impossible to get at lexical variables to
		       ;; figure out what went wrong. To help with
		       ;; this, we support passing arbitrary data into
		       ;; OOPS, where they'll be exposed through
		       ;; BREAK.
		       &rest data)
  (%backtrace-when-batch)
  ;; used to do this, but it doesn't play nicely with IGNORE-ERRORS:
  ;;(apply #'break "Oops!" explanation data)
  (error "~@<Oops!~@[ ~2I~_~?~]~:>" explanation data))

(defun oops-missing-arg (&optional nameoid)
  (oops (format nil "missing argument~@[ ~A~]" nameoid)))


#-(or lispworks sbcl)
(declaim (ftype (function (&rest string) string) string+))
(defun string+ (&rest strings)
  (apply #'concatenate (cons 'string strings)))

#-(or lispworks sbcl)
(declaim (ftype (function (&rest (or string symbol)) symbol) symbolicate gensymicate))
#-(or lispworks sbcl)
(declaim (ftype (function (&rest (or string symbol)) keyword) keywordicate))
;;(declaim (ftype (function ((or string symbol) (or string symbol)) symbol) conc-nameicate))
(defun symbolicate (&rest strings-and-symbols)
  (values (intern (apply #'string+ (mapcar #'string strings-and-symbols)))))
(defun keywordicate (&rest strings-and-symbols)
  (values (intern (apply #'string+ (mapcar #'string strings-and-symbols))
		  :keyword)))
(defun gensymicate (&rest strings-and-symbols)
  (gensym (apply #'string+ (mapcar #'string strings-and-symbols))))

(defmacro with-args-once ((&rest arg-stems) &body body)
  `(let (,@(mapcar (lambda (arg-stem)
                     `(,arg-stem (gensym ,(string arg-stem))))
                   arg-stems))
     (list 'let
           (list ,@(mapcar (lambda (arg-stem)
                             `(list ,arg-stem ,(symbolicate arg-stem "-ARG")))
                           arg-stems))
	   ,@body)))

(locally
  #+cmu (declare (optimize (speed 1))) ; don't want no optimization notes..
  (deftype fixnum/ (den) `(integer ,(ceiling most-negative-fixnum den) ,(floor most-positive-fixnum den)))
  (deftype ufixnum/ (den) `(integer 0 ,(floor most-positive-fixnum den)))
  (deftype ufixnum () '(ufixnum/ 1))
  (deftype ufixnum- (dec) `(integer 0 ,(- most-positive-fixnum dec)))
  (deftype +fixnum/ (den) `(integer 1 ,(floor most-positive-fixnum den)))
  (deftype +fixnum () '(+fixnum/ 1))
  (deftype -fixnum () `(integer ,most-negative-fixnum -1)))

(deftype maybe (&rest types) `(or ,@types null))

;;;; RSTACK = a stack of recycled objects manipulated by reference. The
;;;; user has access to the objects themselves, but RSTACK's copy of
;;;; each reference is private. RSTACK reuses objects to avoid consing.
;;;; Since copy ctors aren't idiomatic in Lisp, RSTACK doesn't have a
;;;; PUSH operation; instead we extend the stack with (INCF
;;;; (RSTACK-COUNT RSTACK)), then manipulate the object returned by
;;;; (RSTACK-TOP RSTACK).

(defstruct (rstk ;;(:print-function print-rstk)
		 (:conc-name rstk.)
		 (:copier nil))
  (bare-count 0 :type ufixnum)
  (bare-vector #() :type (simple-array t 1))
  ;; a function used to create a new element for the array
  (make-element (oops) :type function :read-only t)
  ;; a function to return a pointer to a cleared copy of an element. Note that
  ;; if the pointer returned is to a different object instead, the different
  ;; object is the one which is used thereafter. This behavior would be
  ;; important e.g. in the case of RSTKs of hash tables, since there is no
  ;; portable way to shrink a hash table, and we might want to use a new
  ;; average-size hash table instead of endlessly reusing one which has grown
  ;; to be enormous.
  (cleared-element (oops) :type function :read-only t))

;; an element of a priority queue
(defstruct (pqe (:conc-name pqe.)
		(:copier nil))
  ;; the stored object itself
  (x nil :type t)
  ;; the priority of the stored object
  (pri 0 :type fixnum))

;;; a priority queue
;;;
;;; Note that even though this is implemented as an RSTK, it's
;;; generally inappropriate for users to do bare RSTK operations on
;;; it. Unfortunately, without static typing it's tedious both to
;;; write and to run code which enforces this..
#-ecl
(defstruct (pq (:include rstk
                         (make-element #'make-pqe)
                         (cleared-element #'pqe.clear!))
	       (:copier nil)))
