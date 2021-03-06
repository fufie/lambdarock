;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.dialogue -*-

#|

DESC: modules/dialogues/conversation.lisp - main bulk of conversation code
Copyright (c) 2002-2003 - Knut Arild Erstad

|#

(in-package :org.langband.dialogue)

(defun pop-current-node ()
  (assert *current-conversation-nodes* ()
	  "Attempted to pop empty stack of conversation nodes.")
  (pop *current-conversation-nodes*))

(defun set-current-node (new-node)
  (if (find new-node *current-conversation-nodes*)
      (loop until (eql new-node (car *current-conversation-nodes*))
	    do (pop-current-node))
      (push new-node *current-conversation-nodes*)))

(defun goto-start-node ()
  ;;(format t "GOTO-START-NODE: ~A~%" *current-conversation-nodes*)
  (loop while (cdr *current-conversation-nodes*)
	do (pop-current-node))
  ;;(format t "returning... GOTO-START-NODE: ~A~%" *current-conversation-nodes*)
  (car *current-conversation-nodes*))

(defun goto-previous-node (&optional (times 1))
  (assert (> (length *current-conversation-nodes*) times))
  (dotimes (i times)
    (pop-current-node))
  (car *current-conversation-nodes*))

(defun get-filtered-options (node cparam)
  (let ((options '()))
    (dolist (opt (cnode.options node))
      (if (conversation-option-p opt)
	  (when (or (null (copt.test opt))
		    (funcall (copt.test opt) cparam))
	    (push opt options))
	(let ((node (if (conversation-node-p opt)
			opt
		      (gethash opt *conversations*))))
	  ;; get options from included nodes recursively
	  (when node
	    (let ((rec-options (get-filtered-options node cparam)))
	      (setf options (nconc (nreverse rec-options) options)))))))
    ;; delete duplicates from recursively included options before returning
    ;; (doesn't hurt, and could be useful in some cases)
    (nreverse (delete-duplicates options :test #'eql))))

(defun get-node-text (node cparam)
  (let ((text (cnode.text node)))
    (ctypecase text
      (string text)
      (function (funcall text cparam)))))

(defun get-option-text (opt cparam)
  (let ((text (copt.text opt)))
    (ctypecase text
      (string text)
      (function (funcall text cparam)))))

(defun get-destination (option cparam)
  (let ((dest (copt.dest option)))
    (if (functionp dest)
	(funcall dest cparam)
	dest)))

(defun skip-node? (node cparam)
  ;; eventually like this? (or (and (functionp ...)) (and (stringp ...)))
  (and (functionp (cnode.skip-test node))
       (funcall (cnode.skip-test node) cparam)))

(defun get-conversation-node (id-or-node cparam)
  (assert id-or-node)
  (if (keywordp id-or-node)
      id-or-node
      (let ((node id-or-node)
	    (max-count 10))
	;; this loop is not meant to be pretty :>
	(loop
	 (when (zerop (decf max-count))
	   (error "GET-CONVERSATION-NODE: Probably circular conversation skipping."))
	 (when (stringp node)
	   (setf node (gethash node *conversations*)))
	 (when (keywordp node)
	   (return node))
	 (assert (conversation-node-p node))
	 ;; push current node onto stack even if it is skipped
	 (set-current-node node)
	 ;; skip or return
	 (if (skip-node? node cparam)
	     (setf node (cnode.skip-dest node))
	     (return node))))))

(defun maybe-perform (fun cparam)
  (when fun
    (funcall fun cparam)))

(defun %conversation (&rest args &key id &allow-other-keys)
  (let ((node (apply #'make-conversation-node args)))
    (when id
      (setf (gethash id *conversations*) node))
    node))

(defun %option (&rest args)
  (apply #'make-conversation-option args))

(defun %quit-option (&optional (text "Bye."))
  (%option :text text :dest :quit))

(defun %dest-option (dest &optional (text "[continue]"))
  (%option :text text :dest dest))

(defun text->color-list-text (string &key
				     (normal-color +term-l-blue+)
				     (action-color +term-yellow+)
				     (action-start-char #\[)
				     (action-end-char #\])
				     (remove-action-chars nil))
  "Convert text to list of color and text segments used by PRINT-TEXT!."

  ;; this doesn't need to be fast, so use a slow, recursive algorithm
  (labels ((split-text (string)
	     (if (or (null string)
		     (zerop (length string)))
		 nil
	       (let ((pos1 (position action-start-char string)))
		 (if (null pos1)
		     `((,normal-color ,string))
		   (let ((pos2 (position action-end-char string :start pos1)))
		     (cond ((null pos2)
			    ;; this shouldn't happen
			    ;; note that no nesting of [] is allowed
			    (warn "TEXT->COLOR-LIST-TEXT: error in string ~S" string)
			    `((,normal-color ,string)))
			   ((zerop pos1)
			    (let ((action-string
				   (if remove-action-chars
				       (subseq string (1+ pos1) pos2)
				     (subseq string pos1 (1+ pos2)))))
			      `((,action-color ,action-string)
				,@(split-text (subseq string (1+ pos2))))))
			   (t
			    (let ((action-string
				   (if remove-action-chars
				       (subseq string (1+ pos1) pos2)
				     (subseq string pos1 (1+ pos2)))))
			      `((,normal-color ,(subseq string 0 pos1))
				(,action-color ,action-string)
				,@(split-text (subseq string (1+ pos2)))))))))))))
    (split-text string)))

(defun display-conversation-node (node cparam)
  "Display a single conversation node and its conversation options (replies)."
  (clear-window *cur-win*)
  ;;(warn "Displaying ~s with params ~s" node cparam)
  ;; maybe perform something
  (maybe-perform (cnode.perform node) cparam)
  ;; display text
  (multiple-value-bind (col-offset row-offset)
      (print-text! 6 2 +term-l-blue+
		   (text->color-list-text
		    (get-node-text node cparam)
		    :normal-color +term-l-blue+))
    (declare (ignore col-offset))
  (let* ((row (+ 2 row-offset))
	 (picture '(engine-gfx "people/male-hobbit-rogue.png"))
	 (npc-name "Unknown person")
	 (col 3)
	 (i -1)
	 (code-a (char-code #\a))
	 (options (get-filtered-options node cparam)))

    ;;(warn "checking ~s" (cparam.npc cparam)) 
    (ignore-errors
      (when (typep (cparam.npc cparam) 'active-monster)
	(when-bind (name (get-creature-name (cparam.npc cparam)))
	  (setf npc-name name))
	(when-bind (pic (slot-value (amon.kind (cparam.npc cparam)) 'picture))
	  ;;(warn "Found pic ~s" pic)
	  (when (or (stringp pic) (consp pic))
	    (setf picture pic)))))

    (let ((name-col (- (get-frame-width +dialogue-frame+) (+ 5 (length npc-name)))))
      (output-string! *cur-win* name-col 1 +term-l-green+ npc-name))
    
    ;; show picture (make it depend on conversation, should also have some checks on size and placement)
    (when (use-images?)
      ;; hackish, improve later
      (when (and picture (or (stringp picture) (consp picture)))
	(let ((pic-col (- (get-frame-width +dialogue-frame+) 20)))
	  (paint-gfx-image& picture pic-col 3))))


    ;; assign characters to filtered options and display them
    (dolist (opt options)
      (incf i)
      (let* ((c (code-char (+ code-a i)))
	     (text (get-option-text opt cparam)))
	;; assume that there are not too many options to fit the screen
	(put-coloured-str! +term-white+ (format nil "~A." c) col row)
	(multiple-value-bind  (dummy-col dummy-row)
	    (print-text! 6 row +term-l-green+
				   (text->color-list-text
				    text :normal-color +term-l-green+))
	  (declare (ignorable dummy-col))
	  (setf row (1+ dummy-row)))))

    ;; print prompt
    (put-coloured-str! +term-l-blue+ "-> Reply by pressing a key: " col (1+ row))
    ;; loop until we get a valid key
    (let ((max-code (+ code-a i))
	  (key (read-one-character)))
      (loop until (or (eql key +escape+)
		      (<= code-a (char-code key) max-code))
	    do (setf key (read-one-character)))
      ;; return the chosen option, or nil if escape was pressed
      (if (eql key +escape+)
	  nil
	  (nth (- (char-code key) code-a) options))))))

(defun display-conversation (id cparam)
  (let ((node (get-conversation-node id cparam)))
    (loop
     (let ((option (display-conversation-node node cparam)))
       (when (null option)
	 ;; quit when escape is pressed, the conversations should probably just
	 ;; be designed so this does not create flag problems
	 ;; (maybe change this behavior later, or allow it to be configurable?)
	 (return))
       (assert (conversation-option-p option))
       ;; perform hook
       (maybe-perform (copt.perform option) cparam)
       (let* ((dest (get-destination option cparam))
	      (nnode (get-conversation-node dest cparam)))
	 (ctypecase nnode
	   (keyword (case nnode
		      (:quit  (return))
		      (:start (setf node (goto-start-node)))
		      (:back  (setf node (goto-previous-node)))
		      (t      (warn "Unknown conversation keyword ~A." nnode))))
	   (conversation-node (setf node nnode))))))))

(defun activate-conversation (id player npc)
  (let ((*current-conversation-nodes* nil))
    (display-conversation id (make-conversation-parameters :player player :npc npc))))

;; finding an npc
(defun interactive-choose-npc (dungeon player &key (max-distance 5) (can-talk-test nil))
  (let ((dir (get-aim-direction)))
    (when dir
      (assert (numberp dir))
      (when (= dir 5)
	;; just for fun, return the player :->
	(return-from interactive-choose-npc player))
      ;; search for npc
      (let ((x (location-x player))
	    (y (location-y player))
	    (dx (aref *ddx* dir))
	    (dy (aref *ddy* dir)))
	(dotimes (i max-distance)
	  (incf x dx)
	  (incf y dy)
	  (let ((npc (first (cave-monsters dungeon x y))))
	    (when (and npc (or (not can-talk-test)
			       (funcall can-talk-test player npc)))
	      (return-from interactive-choose-npc npc)
	      )))))))

(defun lookup-conversation-id (dungeon player npc &key (default nil))
  (declare (ignorable dungeon player))
  (let ((retval default))
    ;;(warn "Looking for ~s" npc)
    (when (typep npc 'active-monster)
      (when-bind (conv (gethash (get-id (amon.kind npc)) *conversations*))
	(setf retval conv)))

    retval))


(defun interactive-start-conversation (dungeon player &key will-talk-test can-talk-test (max-distance 5))
  (let ((npc (interactive-choose-npc dungeon player
				      :max-distance max-distance
				      :can-talk-test can-talk-test)))
    (cond ((null npc)
	   (print-message! "There is nobody to talk to there."))
	  ((eql npc player)
	   (print-message! "You chat with yourself for a while."))
	  ((and will-talk-test
		(not (funcall will-talk-test player npc)))
	   (format-message! "~A refuses to talk to you." (get-creature-name npc)))
	  ((or (not will-talk-test) ;; all ok
	       (funcall will-talk-test player npc)) ;; safeguard
	   (let ((conv (lookup-conversation-id dungeon player npc)))
	     (warn "Got conv ~s for id ~s" conv npc)
	     (if (typep conv 'conversation-node)
		 (with-dialogue ()
		   (activate-conversation conv player npc))
		 (format-message! "The ~A has nothing to say." (get-creature-name npc)))
	     ))
	  
	  )))

(defun wizard-start-conversation (dungeon player)
  (declare (ignore dungeon))
  (let* ((id (get-string-input "Conversation ID: " :max-length 50))
	 (node (gethash id *conversations*)))
	(when node
	  (assert (conversation-node-p node))
	  (with-dialogue ()
	    (activate-conversation node player nil)))))

;; macro stuff from here on
(defun find-clause (keyword clauses)
  (rest (find keyword clauses :key #'car)))

(defun collect-clauses (keywords clauses)
  (loop for x in clauses
	when (member (car x) keywords) collect x))

(defun conv-closure (pc-sym npc-sym &rest exprs)
  (let ((params (gensym)))
    `(lambda (,params)
      (let ((,pc-sym (cparam.player ,params))
	    (,npc-sym (cparam.npc ,params)))
	(declare (ignorable ,pc-sym ,npc-sym))
	,@exprs))))

(defun convert-text-clause (clause pc-sym npc-sym)
  ;; text clauses are either
  ;; (1) (:text "string")
  ;; (2) (:text "format string" format-args)
  ;; (3) (:text (expression(s) returning string))
  (cond ((and (= (length clause) 1) (stringp (car clause)))
	 (car clause))
	((stringp (car clause))
	 (conv-closure pc-sym npc-sym
		       `(format nil ,(car clause) ,@(cdr clause))))
	(t
	 (apply #'conv-closure pc-sym npc-sym clause))))

(defun convert-id-clause (clause)
  (cond ((null clause)
	 nil)
	((and (= (length clause) 1) (stringp (car clause)))
	 (car clause))
	(t
	 (error "Error in ID clause ~S" clause))))

(defun convert-function-clause (clause pc-sym npc-sym)
  ;; used for both perform and test
  ;; for now, only null or function, maybe add special flag stuff later?
  (cond ((null clause)
	 nil)
	(t
	 (apply #'conv-closure pc-sym npc-sym clause))))

(defun convert-dest-clause (clause)
  ;; only strings and keywords allowed for now
  (cond ((null clause)
	 nil) ;; need to check for either :dest or :node keyword elsewhere
	((and (= (length clause) 1)
	      (or (stringp (car clause))
		  (keywordp (car clause))))
	 (car clause))
	(t
	 (error "Error in DEST clause ~S" clause))))

(defun convert-node-clause (clause pc-sym npc-sym)
  (cond ((null clause)
	 nil)
	(t
	 (convert-node-expression clause pc-sym npc-sym))))

(defun convert-node-or-dest-clause (clause pc-sym npc-sym)
  ;; figure out automatically if this is a :DEST expr, like ("foo")
  ;; or a :NODE expr, something like ((:text "blah") ...)
  (cond ((and (= (length clause) 1)
	      (or (stringp (car clause))
		  (keywordp (car clause))))
	 (convert-dest-clause clause))
	(t
	 (convert-node-clause clause pc-sym npc-sym))))

(defun test-cond-clause (clause)
  (let ((otherwise-clauses (collect-clauses '(:otherwise) clause)))
    (assert (= (length otherwise-clauses) 1) ()
	    "Each :COND clause must have exactly one :OTHERWISE test.")
    (assert (every #'consp clause) ()
	    "Each :COND expression must be on the form (TEST EXPR ...).")
    (let ((last (car (last clause))))
      (assert (eql (car last) :otherwise) ()
	      "The last test expression of a :COND clause must be :OTHERWISE."))))

(defun convert-cond-clause (clause pc-sym npc-sym)
  (cond ((null clause)
	 nil)
	(t
	 (test-cond-clause clause)
	 ;; note that we don't need to treat :otherwise specially here because
	 ;; it already is true when used as a boolean
	 (conv-closure
	  pc-sym npc-sym
	  `(cond ,@(loop for (test . inner-clauses) in clause
			 collect `(,test ,(convert-node-or-dest-clause
					   inner-clauses pc-sym npc-sym))))))))

(defun convert-option-clause (clause pc-sym npc-sym)
  ;; test for unknown and required clauses
  (test-option-clauses clause)
  (let ((text (convert-text-clause (find-clause :text clause)
				   pc-sym npc-sym))
	(test (convert-function-clause (find-clause :test clause)
				       pc-sym npc-sym))
	(perform (convert-function-clause (find-clause :perform clause)
					  pc-sym npc-sym))
	(dest (convert-dest-clause (find-clause :dest clause)))
	(node (convert-node-clause (find-clause :node clause)
				   pc-sym npc-sym))
	(cond-node (convert-cond-clause (find-clause :cond clause)
					pc-sym npc-sym)))
    ;; assume everything is ok here
    `(%option :text ,text :test ,test :perform ,perform
	      :dest ,(or dest node cond-node))))

(defun convert-quit-option-clause (clause)
  `(%quit-option ,@clause))

(defun convert-dest-option-clause (clause)
  `(%dest-option ,@clause))

(defun convert-skip-test (clause pc-sym npc-sym)
  (when clause
    (conv-closure pc-sym npc-sym (first clause))))

(defun convert-skip-dest (clause pc-sym npc-sym)
  (when clause
    (let ((expr (second clause)))
      (ecase (first expr)
	(:dest (assert (and (= (length expr) 2)
			    (stringp (second expr))))
	       (second expr))
	(:node (convert-node-expression (rest expr) pc-sym npc-sym))))))

(defun count-clauses (clauses)
  ;; first collect and count keywords
  (let ((counts (make-hash-table)))
    (dolist (clause clauses)
      (unless (and (consp clause)
		   (keywordp (car clause)))
	(error "DEFINE-CONVERSATION: Each clause must be on the form
 (:KEYWORD &REST STUFF), not ~A" clause))
      (let ((key (car clause)))
	(setf (gethash key counts) (1+ (or (gethash key counts) 0)))))
    counts))

(defmacro dassert  (test places datum &rest args)
  (let ((new-datum (concatenate 'string datum "~%The clauses are:~%~S"))
	(new-args (append args '(clauses))))
    `(assert ,test ,places ,new-datum ,@new-args)))

(defun test-conversation-clauses (clauses)
  (let ((counts (count-clauses clauses))
	;(*print-length* 100)
	;(*print-circle* t)
	;(*print-level* 100)
	)
    ;; check the requirements of each allowed keyword
    (flet ((cnt (key)
	     (or (gethash key counts) 0)))
      (macrolet ((cassert (test places datum &rest args)
		   (let ((new-datum (concatenate 'string datum "~%The clauses are:~%~S"))
			 (new-args (append args '(clauses))))
		     `(assert ,test ,places ,new-datum ,@new-args))))
	(cassert (= (cnt :text) 1) ()
		 "Each conversation node requires exactly one :TEXT clause.")
	(cassert (>= (+ (cnt :option) (cnt :quit-option) (cnt :dest) (cnt :include))
		     1) ()
		 "Each conversation node requires at least one :OPTION, :QUIT-OPTION, :INCLUDE or :DEST.")
	(cassert (<= (cnt :dest) 1) ()
		 "Too many :DEST clauses.")
	(cassert (<= (cnt :id) 1) ()
		 "More than one :ID clause in conversation node.")
	(cassert (<= (cnt :perform) 1) ()
		 "More than one :PERFORM clause in conversation node.")
	(cassert (<= (cnt :skip-if) 1) ()
		 "Too many :SKIP-IF clauses (more than one might be allowed in the future).")
	))
    ;; make sure there are no unknown clauses
    (loop for key being the hash-keys of counts do
	  (unless (member key '(:text :option :quit-option :id :perform :skip-if :dest :include))
	    (error "Unknown clause ~S in conversation node." key)))))

(defun test-option-clauses (clauses)
  (let ((counts (count-clauses clauses)))
    ;; check the requirements of each allowed keyword
    (flet ((cnt (key)
	     (or (gethash key counts) 0)))
      (assert (= (cnt :text) 1) ()
	      "Each conversation option requires exactly one :TEXT clause.")
      (assert (= (+ (cnt :dest) (cnt :node) (cnt :cond)) 1) ()
	      "Each conversation option requires exactly one :DEST, :NODE or :COND clause.")
      (assert (<= (cnt :test) 1) ()
	      "More than one :TEST clause in conversation option.")
      (assert (<= (cnt :perform) 1) ()
	      "More than one :PERFORM clause in conversation option."))
    ;; make sure there are no unknown clauses
    (loop for key being the hash-keys of counts do
	  (unless (member key '(:text :dest :node :cond :test :perform))
	    (error "Unknown clause ~S in conversation option." key)))))

(defun convert-node-expression (clause pc-sym npc-sym)
  "Convert conversation expression from macro syntax to functional syntax."
  ;; test for unknown and required clauses
  (test-conversation-clauses clause)
  (let* ((id (convert-id-clause (find-clause :id clause)))
	 (text (convert-text-clause (find-clause :text clause)
				    pc-sym npc-sym))
	 (perform (convert-function-clause (find-clause :perform clause)
					   pc-sym npc-sym))
	 (skip-clause (find-clause :skip-if clause))
	 (skip-test (convert-skip-test skip-clause pc-sym npc-sym))
	 (skip-dest (convert-skip-dest skip-clause pc-sym npc-sym))
	 (options (loop for (keyword . body)
			in (collect-clauses
			    '(:option :quit-option :dest :include) clause)
			nconc (ecase keyword
				(:option
				 (list (convert-option-clause body pc-sym npc-sym)))
				(:quit-option
				 (list (convert-quit-option-clause body)))
				(:include
				 body)
				(:dest
				 (list (convert-dest-option-clause body)))))))
    `(%conversation :id ,id :text ,text :perform ,perform
      :skip-test ,skip-test :skip-dest ,skip-dest
      :options (list ,@options))))


(defmacro define-conversation ((pc-sym npc-sym) &rest args)
  ;; make sure args has an ID clause (the other clauses can be tested in convert-node-expression)
  (unless (find-clause :id args)
    (error "DEFINE-CONVERSATION must have an :ID clause."))
  `(eval-when (:load-toplevel :execute)
    ,(convert-node-expression args pc-sym npc-sym)))

