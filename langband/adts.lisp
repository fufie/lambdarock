;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.datastructures -*-

#|

DESC: adts.lisp - Various ADTs

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

;;; This code has been donated to langband by William Harold Newman and is
;;; a no-cons priority-queue plus helper-code. It's not really meant to
;;; be understood, just used.  The code has been customised and arranged to
;;; fit with langband and may miss components needed to make it work with
;;; other apps. Use at your own risk.  -Stig
;;; (some helper functions are in sys.lisp as well)


(in-package :org.langband.datastructures)


;;; helper functions for NEED and DENY
(defun %intermingle-datanames-and-datavalues (datanames datavalues)
  (mapcan #'list datanames datavalues))
#-lispworks
(declaim (ftype (function (t list list) nil) %failed-test-in-need))
(defun %failed-test-in-need (expr datanames datavalues)
  (%backtrace-when-batch)
  (let ((data (%intermingle-datanames-and-datavalues datanames datavalues)))
    ;; used to do this, but it doesn't play nicely with IGNORE-ERRORS:
    ;;(/show "in %FAILED-TEST-IN-NEED" expr data)
    ;;(apply #'break "failed test in NEED" expr data)
    (error "~@<failed NEED: ~2I~_null ~S~@[ ~_DATA=~S~]~:>" expr data)))

(defmacro need (expr &rest data)
  (let ((value (gensym "NEED-VALUE-")))
    `(let ((,value ,expr))
       (if ,value
           ,value
           (%failed-test-in-need ',expr ',data (list ,@data))))))

(defmacro with-gensyms (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   `(,symbol (gensym ,(string symbol))))
                 symbols)
     ,@body))

#-sbcl
(declaim (ftype (function ((simple-array t) &optional t fixnum) (simple-array t)) lengthened-vector))
#+sbcl
(declaim (ftype (function ((simple-array t) &optional t fixnum)
			  (values (simple-array t) &optional)) lengthened-vector))
(defun lengthened-vector (old-vector &optional initial-element new-length-arg)
  (declare (type (simple-array t *) old-vector))
  (declare (type (or null fixnum) new-length-arg))
  (let* ((old-length (length old-vector))
         (new-length (if new-length-arg
                         new-length-arg
                         (1+ (* 2 old-length)))))
    ;; (The following hairy type is artfully chosen to avoid fixnum
    ;; overflow in the calculation of NEW-LENGTH.)
    (declare (type (mod #.(1- (floor (/ most-positive-fixnum 2)))) old-length))
    (declare (type fixnum new-length))
    ;; I don't anticipate shortening a vector with this function, so
    ;; it's probably a programmer error if I try to use it to shorten
    ;; a vector.  (The function would correctly return a shortened
    ;; copy if I took out the assertion, though.)
    (need (>= new-length old-length)) 
    (let ((new-vector (make-array new-length
                                  :initial-element initial-element)))
      (dotimes (i old-length)
        (setf (aref new-vector i) (aref old-vector i)))
      new-vector)))



;;; the INDEX-th element upward from bottom of RSTK. Settable.

(defun rstk--index-too-big (index)
  (error "too-big index ~s" index))

(declaim (inline rstk.aref (setf rstk.aref)))
(defun rstk.aref (rstk index)
  (unless (< index (rstk.bare-count rstk))
    (rstk--index-too-big index))
  (aref (rstk.bare-vector rstk) index))

(defun (setf rstk.aref) (new-value rstk index)
  (unless (< index (rstk.bare-count rstk))
    (rstk--index-too-big index))
  (setf (aref (rstk.bare-vector rstk) index) new-value))

;;; RSTK-AREF without limitation to valid elements
(declaim (inline rstk.aref-dammit))
(defun rstk.aref-dammit (rstk index)
  (aref (rstk.bare-vector rstk) index))

;;; the INDEX-th element downward from top of UNDER-STACK. Settable.

(defmacro rstk.zref (rstk-arg index-arg) 
  (with-args-once (rstk index)
    `(rstk.aref ,rstk (- (rstk.bare-count ,rstk) ,index 1))))

;;; the top element of RSTK
;;; BTWE, this *probably* shouldn't be inline, but I'm not sure.

#-sbcl
(declaim (ftype (function (rstk) t) rstk.top))
#+sbcl
(declaim (ftype (function (rstk) (values t &optional)) rstk.top))
(defun rstk.top (rstk)
  (rstk.zref rstk 0))

;;; a helper function for (SETF RSTK.COUNT)

(declaim (ftype (function (rstk) (values)) rstk.lengthen-bare-vector))
;;#+sbcl
;;(declaim (ftype (function (rstk) (values &optional)) rstk.lengthen-bare-vector))
(defun rstk.lengthen-bare-vector (rstk)
  (declare (optimize speed
		     #+sbcl (sb-ext:inhibit-warnings 3)
		     ;;#+cmu (ext:inhibit-warnings 3)
		     ))
  (let* ((old-bare-vector-length (length (rstk.bare-vector rstk)))
         ;; Make the stack grow exponentially so that only a logarithmically
         ;; large number of expansions are required for the stack to grow
         ;; exponentially large, avoiding O(N*N) time complexity for growing
         ;; a stack to size N.)
         (new-bare-vector-length (1+ (* 2 old-bare-vector-length))) ; ARB
         (make-element (rstk.make-element rstk)))
    (setf (rstk.bare-vector rstk)
          (lengthened-vector (rstk.bare-vector rstk)
                             nil
                             new-bare-vector-length))
    (loop for index of-type (ufixnum/ 2)
	  from old-bare-vector-length
	  below new-bare-vector-length
	  do
	  (setf (aref (rstk.bare-vector rstk) index)
		(funcall make-element))))
  (values))
  
;;; the number of elements in RSTK. Settable. 

(declaim (inline rstk.count))
(defun rstk.count (rstk)
  (rstk.bare-count rstk))

#-sbcl
(declaim (ftype (function (ufixnum rstk) ufixnum) (setf rstk.count)))
#+sbcl
(declaim (ftype (function (ufixnum rstk) (values ufixnum &optional)) (setf rstk.count)))
(defun (setf rstk.count) (new-count rstk)
  (declare (type ufixnum new-count))
  (declare (optimize speed))
  (if (<= new-count (rstk.count rstk))
      (loop for i of-type (ufixnum/ 2) from new-count below (rstk.count rstk)
	    do
	    (setf (rstk.aref rstk i)
		  (funcall (rstk.cleared-element rstk)
			   (rstk.aref rstk i))))
      (loop while (< (length (rstk.bare-vector rstk)) new-count) do
	    (rstk.lengthen-bare-vector rstk)))
  (setf (rstk.bare-count rstk) new-count))

;;; iteration over stack contents, upwards and downwards

(defmacro do-rstk ((i rstk-arg) &body body)
  (with-gensyms (j)
    (with-args-once (rstk)
      `(dotimes (,j (rstk.count ,rstk))
         (let ((,i (rstk.aref ,rstk ,j)))
           ,@body)))))
(defmacro od-rstk ((i rstk-arg) &body body)
  (with-gensyms (j)
    (with-args-once (rstk)
      `(dotimes (,j (rstk.count ,rstk))
         (let ((,i (rstk.zref ,rstk ,j)))
           ,@body)))))

#||
(defun print-rstk (rstk stream print-depth)
  (declare (type rstk rstk))
  (declare (type stream stream))
  (declare (ignore print-depth))
  (puprint-logical-block (stream nil :prefix "#!(" :suffix ")")
    (prin1 'rstk stream)
    (do-rstk (element rstk)
      (format stream " ~_~w" element)))
  (values))
||#

;;; RSTK analogues of CL:POSITION, CL:FIND, and CL:DELETE.
;;; BTWE/TO DO: Find out whether a generalized inline version of this could
;;; handle :TEST and :KEY keyword arguments efficiently.

(defun position-in-rstk (x rstk &key (test #'eql) (key #'identity))
  (declare (type t x))
  (declare (type rstk rstk))
  (declare (type function test key))
  (let ((bare-vector (rstk.bare-vector rstk)))
    (dotimes (i (rstk.bare-count rstk))
      (when (funcall test x (funcall key (aref bare-vector i)))
        (return-from position-in-rstk i)))
    nil))

(declaim (inline find-in-rstk))
(defun find-in-rstk (x rstk &rest rest)
  (let ((position (apply #'position-in-rstk x rstk rest)))
    (when position
      (rstk.aref rstk (the fixnum position)))))

(defun delete-from-rstk (x rstk &key (test #'eql) (key #'identity))
  (declare (type t x))
  (declare (type rstk rstk))
  (declare (type function test key))
  (let ((j 0)
        (bare-vector (rstk.bare-vector rstk)))
    (declare (type (ufixnum- 1) j))
    (dotimes (i (rstk.count rstk))
      (let ((bare-vector-i (aref bare-vector i)))
        (unless (funcall test x (funcall key bare-vector-i))
          (rotatef (aref bare-vector j)
                   (aref bare-vector i))
          (incf j))))
    (setf (rstk.count rstk) j))
  rstk)

#-sbcl
(declaim (ftype (function (pqe) pqe) pqe.clear!))
#+sbcl
(declaim (ftype (function (pqe) (values pqe &optional)) pqe.clear!))
(defun pqe.clear! (pqe)
  (setf (pqe.x pqe) nil
        (pqe.pri pqe) 0)
  pqe)


;;; How many elements are in PQ?

(declaim (inline pq.count))
(defun pq.count (pq)
  (declare (type pq pq))
  (rstk.count pq))

;;; Return the index of the parent element of INDEX-th element of PQ.
#-sbcl
(declaim (ftype (function (+fixnum) ufixnum) pq.index.parent-index))
#+sbcl
(declaim (ftype (function (+fixnum) (values ufixnum &optional)) pq.index.parent-index))
(declaim (inline pq.index.parent-index)) ; (only used in 3 places)
(defun pq.index.parent-index (index)
  (declare (optimize speed))
  (values (floor (1- index) 2)))

;;; Insert an object X into PQ with priority PRI.

(declaim (ftype (function (pq t fixnum) (values)) pq.put))
(defun pq.put (pq x pri)
  (declare (optimize speed))
  ;; Make space for the new element.
  (incf (rstk.count pq) 1)
  ;; From Drozdek, we have HeapEnqueue(EL):
  ;;   Put EL at the end of the queue;
  ;;   While EL is not in the root and EL>parent(EL)
  ;;      Swap EL with its parent.
  (do ((i (1- (rstk.count pq)) (pq.index.parent-index i))) ; index where we.. 
                                        ; ..propose to write EL
      ((or (zerop i)
           (<= pri (pqe.pri (rstk.aref pq (pq.index.parent-index i)))))
       (let ((insert-point (rstk.aref pq i)))
         (declare (type pqe insert-point))
         (setf (pqe.x insert-point) x
               (pqe.pri insert-point) pri)))
    (declare (type ufixnum i))
    ;; Move parent to the spot where we were considering putting EL (by
    ;; exchanging struct references instead of copying struct contents).
    (rotatef (rstk.aref pq i) (rstk.aref pq (pq.index.parent-index i))))
  ;; Voila.
  (values))

;;; the index of the J-th child (0-based) of PARENT-th element of a priority
;;; queue
#-sbcl
(declaim (ftype (function ((ufixnum/ 2) (mod 2)) (+fixnum/ 2)) pq.index.child-index))
#+sbcl
(declaim (ftype (function ((ufixnum/ 2) (mod 2))
			  (values (+fixnum/ 2) &optional)) pq.index.child-index))
(declaim (inline pq.index.child-index)) ; (only used 3 places)
(defun pq.index.child-index (parent j)
  (+ 1 j (* 2 parent)))

#-sbcl
(declaim (ftype (function (pq) (values t (maybe fixnum))) pq.top))
#+sbcl
(declaim (ftype (function (pq) (values t (maybe fixnum) &optional)) pq.top))
;;; this one added for langband, it may be wrong
(defun pq.top (pq)
  (declare (optimize speed))
  (if (zerop (rstk.count pq))
      (values nil nil)
      (let ((front (rstk.aref pq 0)))
	(values (pqe.x front) (pqe.pri front)))))

;;; Remove the highest-priority object X from PQ and return (VALUES X PRI),
;;; where PRI is the priority of X. If there are no more objects in the queue,
;;; return (VALUES NIL NIL).

#-sbcl
(declaim (ftype (function (pq) (values t (maybe fixnum))) pq.get))
#+sbcl
(declaim (ftype (function (pq) (values t (maybe fixnum) &optional)) pq.get))
(defun pq.get (pq)
  (declare (optimize speed))
  (if (zerop (rstk.count pq))
      (values nil nil)
      (let* (;; The return value is the old value of the root.
             (old-root (rstk.aref pq 0))
             ;; the number of elements remaining after removing an element
             (n1 (1- (rstk.count pq)))
             ;; The old last element is now extra, and needs to be inserted
             ;; somewhere else.
             (last (rstk.aref pq n1))
             (pri-last (pqe.pri last))
             ;; insertion index for LAST
             (insert 0))
        ;; Choose INSERT by descending through heap until we reach a
        ;; leaf or we reach a point where putting LAST in INSERT would
        ;; leave INSERT larger than either of its children. As we
        ;; descend, we move biggest children upwards to make space for
        ;; our final insertion at INSERT.
        (loop
          (let ((left (pq.index.child-index insert
                                            0))) ; index of INSERT left child
            (when (>= left n1)          ; When INSERT is childless (is a leaf)
              (return))
            (let* ((right (pq.index.child-index insert 1)) ; right child index
                   (biggest left)       ; index of biggest child
                   (pri-biggest         ; priority of biggest child
                    (pqe.pri (rstk.aref pq biggest))))
              (when (and (< right n1)   ; when right child exists and is..
                         (> (pqe.pri (rstk.aref pq right)) ; ..bigger..
                            pri-biggest)) ; ..than left
                (setf biggest right
                      pri-biggest (pqe.pri (rstk.aref pq right))))
              (if (>= pri-last pri-biggest)
                  (return)
                  (progn
                    (rotatef (rstk.aref pq insert) (rstk.aref pq biggest))
                    (setf insert biggest))))))
        ;; Insert LAST at INSERT.
        (rotatef (rstk.aref pq insert) (rstk.aref pq n1))
        ;; By now OLD-ROOT should've been swapped back to the end of the array,
        ;; so that it'll be the one discarded by (DECF (RSTK-COUNT PQ)).
        (need (eq old-root (rstk.aref pq n1)))
        ;; Save values from OLD-ROOT, discard the tail element of the
        ;; array (overwriting the values in OLD-ROOT), then return the
        ;; saved values from OLD-ROOT.
        (let ((x (pqe.x old-root))
              (pri (pqe.pri old-root)))
          (decf (rstk.count pq))
          (values x pri)))))
            
;;; Check that PQ really is a heap.
(declaim (ftype (function (pq) (values)) pq.assert-heapness))
(defun pq.assert-heapness (pq)
  (dotimes (i (ceiling (rstk.count pq) 2))
    (declare (type (ufixnum/ 2) i))
    (let ((ipri (pqe.pri (rstk.aref pq i))))
      (dotimes (j 2) ; for children of I
        (declare (type (ufixnum/ 2) j))
        (let ((child-index (pq.index.child-index i j)))
          (when (< child-index (rstk.count pq)) ; when CHILD-INDEX in range
            (need (>= ipri (pqe.pri (rstk.aref pq child-index)))))))))
  (values))

#-sbcl
(declaim (ftype (function (pq) pq) pq.clear!))
#+sbcl
(declaim (ftype (function (pq) (values pq &optional)) pq.clear!))
(defun pq.clear! (pq)
  (setf (rstk.count pq) 0)
  pq)

;;; Return the priority of the highest-priority element in PQ, or NIL
;;; if there are no elements in PQ.

#-sbcl
(declaim (ftype (function (pq) (maybe fixnum)) pq.max-pri))
#+sbcl
(declaim (ftype (function (pq) (values (maybe fixnum) &optional)) pq.max-pri))
(defun pq.max-pri (pq)
  (if (zerop (rstk.count pq))
      nil
      (pqe.pri (rstk.aref pq 0))))

;;; Iterate over contents of PQ (destructively, popping the contents
;;; out of PQ as we go). If new contents are added to PQ as iteration
;;; goes on, that's fine, they'll be iterated over too. (Thus, this
;;; should work e.g. for event simulation.)

(defmacro do-pq ((x pq &optional pri) &body body)
  (with-gensyms (x-0 pri-0)
    `(loop
       (multiple-value-bind
           (,x-0 ,pri-0)
           (pq.get ,pq)
         (unless ,pri-0
           (return))
         (let ,(if pri
                   `((,x ,x-0) (,pri ,pri-0))
                   `((,x ,x-0)))
           ,@body)))))

;;; ===========================================
;;; an ordinary queue (different source)
;;; ===========================================

(defun make-queue ()
  (cons nil nil))

(defun queue-as-list (q)
  (car q))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
      (setf (cdr (cdr q)) (list obj)
	    (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))
