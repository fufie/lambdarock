(in-package :cl-user)

(setf cl:*load-verbose* nil
      cl:*load-print* nil)

(setf asdf:*central-registry* (cons "../albert/" asdf:*central-registry*))

(load "/home/stig/lisp/albert/cl-ppcre/cl-ppcre.asd")
(load "/home/stig/lisp/albert/albert.asd")
;;(asdf:oos 'asdf:load-op :cl-ppcre)
(asdf:oos 'asdf:load-op :albert)

;;(require :albert)
;;(load "../lispdoc/sds-sys.x86f")

(setf apispec:*xml2sexp-prog* "/home/stig/lisp/albert/expat/alb_xml2sexp")

(dolist (i '(+ - * / = < <= > >= /=
	     dotimes cons list push pushnew string dolist assoc
	     null consp integerp consp symbolp characterp stringp
	     listp numberp typep minusp plusp check-type assert
	     decf incf 1- 1+ mod floor truncate
	     get getf nconc append
	     aref svref nth elt length list-length
	     first second third fourth fifth
	     caar cadr cddr 
	     #+cmu COMMON-LISP::BACKQ-APPEND
	     #+cmu COMMON-LISP::BACKQ-CONS
	     #+cmu COMMON-LISP::BACKQ-LIST
	     #+cmu COMMON-LISP::BACKQ-LIST*
	     ))
	     
  (pushnew i lisp2csf:*ignorable-calls*))

;;; Langband-specific handling of some macros
(defmethod lisp2csf:check-body-expression (expr-type expression)
  (cond ((equal (symbol-name expr-type) "WHEN-BIND")
	 (let ((var-calc (second (second expression))))
	   ;;(warn "Second is ~s -> ~s" (second expression) var-calc)
	   (lisp2csf:analyse-body-expression var-calc))
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "WITH-FRAME")
	 (push expr-type lisp2csf:*cur-collected-calls*)
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "WITH-DUNGEON")
	 (push expr-type lisp2csf:*cur-collected-calls*)
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "WITH-BINARY-OUTPUT-TO-VECTOR")
	 ;; do nothing
	 t)
	((equal (symbol-name expr-type) "WITH-DIALOGUE")
	 (push expr-type lisp2csf:*cur-collected-calls*)
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	((equal (symbol-name expr-type) "UNLESS-BIND")
	 (let ((var-calc (second (second expression))))
	   ;;(warn "Second is ~s -> ~s" (second expression) var-calc)
	   (lisp2csf:analyse-body-expression var-calc))
	 (map nil #'lisp2csf:analyse-body-expression (cddr expression))
	 t)
	(t nil)))

(defmethod lisp2csf:analyse-object ((objtype (eql 'defsubst)) obj)

  (let* ((fun-repr (cadr obj))
	 (fun-name (etypecase fun-repr
		     (symbol (string fun-repr))
		     (cons (format nil "~a" fun-repr))))
	 
	 (arg-list (caddr obj))
	 (body (cdddr obj)))
    
    (let ((meth-obj (lisp2csf::%create-method fun-name
				    :type :function
				    :lambda-list arg-list
				    :body body)))
      (push meth-obj (slot-value lisp2csf:*current-package* 'apispec-base:content))))

  t)

(defmethod lisp2csf:analyse-object ((objtype (eql 'def-exportconst)) obj)
  (lisp2csf:analyse-object 'cl:export (list 'export (list (second obj))))
  (lisp2csf:analyse-object 'cl:defconstant (list 'defconstant (second obj) (third obj))))


;;(setf (albert:albert-setting '("albert" "presentation" "only-exported")) t)
;;(setf (albert:albert-setting '("albert" "submarine-quiet")) t)
;;(setf (albert:albert-setting '("albert" "verbose")) t)
(setf (albert:albert-setting '("albert" "presentation" "formats")) (list "html")
      (albert:albert-setting '("albert" "html" "template" "index")) "tools/index.atm"
      (albert:albert-setting '("albert" "html" "template" "package")) "tools/package.atm")
;;(setf (albert:albert-setting '("albert" "lisp2csf" "display-progress")) t)

(defun dble ()
  (load "langband-engine.asd")
  (load "variants/vanilla/langband-vanilla.asd")
  ;;(show-files :langband-engine)
  (albert:document-systems :langband-engine :langband-vanilla))

(dble)


#+cmu
(when ext:*batch-mode*
  (quit))

