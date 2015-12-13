;;;; defmacro-enhance.lisp
;;;;
;;;; This file is part of the defmacro-enhance library, released under GPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Alexander Popolitov

(in-package #:defmacro-enhance)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *last-versions* (make-hash-table)))

(defmacro def-*!-symbol-p (symb)
  "Define test-function for argument to be prefix-bang-hyphen symbol."
  (let ((s-symb (string symb)))
    `(defun ,(intern (format nil "~a~a"(string s-symb) "!-SYMBOL-P")) (symb)
       (and (symbolp symb)
	    (> (length (string symb)) ,(+ (length s-symb) 2))
	    (equal (subseq (string symb) 0 ,(+ (length s-symb) 2))
		   ,(format nil "~a~a" (string s-symb) "!-"))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-*!-symbol-p g)
  (def-*!-symbol-p o)
  (def-*!-symbol-p e)
  (def-*!-symbol-p p))

;; My own version of parse-body, just to not depend on anyone
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-body (body)
    ;; (format t "~a~%" body)
    (let (doc-string)
      (if (and (> (length body) 1)
	       (stringp (car body)))
	  (setf doc-string (car body)
		body (cdr body)))
      (iter (with non-decl-met = nil)
	    (for form in body)
	    ;; (format t "~a~%" form)
	    (if (not non-decl-met)
		(if (and (consp form) (equal (car form) 'declare))
		    (collect form into declarations)
		    (progn (setf non-decl-met t)
			   (collect form into forms)))
		(collect form into forms))
	    (finally (return (values forms declarations doc-string)))))))

(defmacro grep-spec-syms ((var type lst) &body body)
  `(let ((,var (remove-duplicates
		(remove-if-not #',(intern (concatenate 'string (string type) "-SYMBOL-P")
					  (find-package "DEFMACRO-ENHANCE"))
			       (alexandria:flatten ,lst)))))
     ,@body))

(defmacro transforming-body-when-syms (&body body)
  `(append (if doc (list doc))
	   decls
	   (if syms
	       (list ,@body)
	       forms)))
      
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun declare-g!-syms-as-gsyms (body env)
    (multiple-value-bind (forms decls doc) (parse-body body)
      (grep-spec-syms (syms g! (cdr (assoc :variables (find-undefs `(progn ,@forms) :env env))))
	(transforming-body-when-syms
	  `(let ,(mapcar (lambda (s)
			   `(,s (gensym ,(subseq (symbol-name s) 3))))
			 syms)
	     ,@forms))))))

(defmacro define-feature-adder (name extra-args &body body)
  (let ((def-name (intern (concatenate 'string "DEFINE-/" (string name))))
	(add-name (intern (concatenate 'string "ADD-/" (string name)))))
    `(progn (defmacro ,def-name (src-name dst-name ,@extra-args)
	      ,@body)
	    (defmacro ,add-name (dst-name &optional (args nil args-p) &body body)
	      ,(if (equal '(args &body body) extra-args)
		   `(when (not args-p)
		      (setf args '(name args &body body)
			    body '((append (list name args) body))))
		   `(declare (ignore args-p args)))
	      (let ((src-name (intern (concatenate 'string (string dst-name) "/" ,(string name)))))
		`(,',def-name ,src-name ,dst-name
		   ,@,(if extra-args '(list args))
		   ,@body))))))

(define-feature-adder g! (args &body body)
  "Arg list of DST-NAME should contain BODY. All g!-symbols in BODY are transformed to gensyms."
  (let ((g!-env (gensym "G!-ENV")))
    `(defmacro ,src-name ,(append args `(&environment ,g!-env))
       (let ((,(intern "BODY") (declare-g!-syms-as-gsyms body ,g!-env)))
	 (append `(,',dst-name)
		 (progn ,@body))))))

(defmacro add-to-last (feature object &rest extras)
  "This should be DEFUN because it does produce side-effects, which should not be the case for macros.
But let's see where it leads."
  (let ((src-name (or (gethash object *last-versions*)
		      (setf (gethash object *last-versions*) object)))
	(add-name (intern (concatenate 'string "ADD-/" (string feature)))))
    `(progn (,add-name ,src-name ,@extras)
	    (setf (gethash ',object *last-versions*)
		  ',(intern (concatenate 'string (string src-name) "/" (string feature)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last g! defmacro)
  (add-to-last g! defmacro-driver (clause-template &body body)
	       `(,clause-template ,@body))
  (add-to-last g! defun)
  (add-to-last g! define-feature-adder))

(define-feature-adder/g! gm! ()
  `(defmacro ,src-name (definitions &body body &environment ,g!-env)
     `(,',dst-name
       ,(mapcar (lambda (x)
		  (destructuring-bind (name args &body body) x
		    `(,name ,args
			    ,@(declare-g!-syms-as-gsyms body ,g!-env))))
		definitions)
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last gm! macrolet)
  (add-to-last gm! flet)
  (add-to-last gm! labels))

;; ONCE-ONLY just not to depend on RUTILS, which I plan to enhance
;; Limited version, to use once only for my needs.
(defun once-only (specs body)
  (if specs
      (let ((gensyms (mapcar (lambda (sym) (gensym (string sym))) specs)))
	`(let ,(mapcar (lambda (g s)
			  `(,g (gensym ,(string s))))
			gensyms specs)
	    `(let (,,@(mapcar (lambda (g s)
				``(,,g ,,s))
			      gensyms specs))
	       ,(let ,(mapcar (lambda (s g)
				`(,s ,g))
			      specs gensyms)
		     ,@body))))
      body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-o!-once-only (args body)
    (multiple-value-bind (forms decls doc) (parse-body body)
      (grep-spec-syms (syms o! args)
	(transforming-body-when-syms
	  (once-only syms forms))))))

(define-feature-adder/g! o! (args &body body)
  "All o!-symbols in the ARGS variable of the DST-NAME macro are
treated as once-only symbols. BODY variable is parsed accordingly."
  `(defmacro ,src-name ,args
     (let ((,(intern "BODY") (make-o!-once-only args body)))
       (append `(,',dst-name)
	       (progn ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last o! defmacro))

(define-feature-adder om! ()
  `(defmacro ,src-name (definitions &body body)
     `(,',dst-name ,(mapcar (lambda (x)
			      (destructuring-bind (name args &body body) x
				`(,name ,args
					,@(make-o!-once-only args body))))
			    definitions)
		   ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last om! macrolet))

(defun make-e!-internable (body env)
  (multiple-value-bind (forms decls doc) (parse-body body)
    (grep-spec-syms (syms e! (cdr (assoc :variables (find-undefs `(progn ,@forms) :env env))))
      (transforming-body-when-syms
	`(let ,(mapcar (lambda (sym)
			 `(,sym (intern ,(subseq (string sym) 3))))
		       syms)
	   ,@forms)))))

(define-feature-adder/g! e! (args &body body)
  "Deformation, in which E!-symbols are interned in package,
where macro is expanded, not where it is defined.
Useful for writing anaphoric macros."
  (let ((g!-env (gensym "G!-ENV")))
    `(defmacro ,src-name ,(append args `(&environment ,g!-env))
       (let ((,(intern "BODY") (make-e!-internable body ,g!-env)))
	 (append `(,',dst-name)
		 (progn ,@body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last e! defmacro)
  (add-to-last e! defmacro-driver (clause-template &body body)
	       `(,clause-template ,@body))
  (add-to-last e! defun))

(define-feature-adder/g! em! ()
  `(defmacro ,src-name (definitions &body body &environment ,g!-env)
     `(,',dst-name
       ,(mapcar (lambda (x)
		  (destructuring-bind (name args &body body) x
		    `(,name ,args
			    ,@(make-e!-internable body ,g!-env))))
		definitions)
       ,@body)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last em! macrolet)
  (add-to-last em! flet)
  (add-to-last em! labels))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gensym-p (sym)
    (not (symbol-package sym)))
  (defun progn-flatten-p!-syms (args body lexenv-sym)
    (multiple-value-bind (forms decls doc) (parse-body body)
      (when (gensym-p lexenv-sym)
	(push `(declare (ignorable ,lexenv-sym)) decls))
      (grep-spec-syms (syms p! args)
	(transforming-body-when-syms
	  `(let ,(mapcar (lambda (s)
			   `(,s (expand-progns ,s ,lexenv-sym)))
			 syms)
	     ,@forms))))))

(defun expand-progns (expr lexenv)
  (let (res)
    (labels ((rec (expr inside)
	       (if (atom expr)
		   (push expr res)
		   (if (eq 'progn (car expr))
		       (dolist (elt (cdr expr))
			 (rec (macroexpand elt lexenv) t))
		       (if inside
			   (push expr res)
			   (dolist (elt expr)
			     (rec (macroexpand elt lexenv) t)))))))
      (rec expr nil))
    (nreverse res)))

(defun ensure-environment-in-args (args)
  (let (env)
    (iter (for elt on args)
	  (if (eq '&environment (car elt))
	      (setf env (cadr elt))))
    (if env
	(values env args)
	(let ((it (gensym "ENV")))
	  (values it (append args (list '&environment it)))))))
	    

(define-feature-adder p! ()
  "Define macro SRC-NAME on top of DST-NAME. Arg list of DST-NAME should
contain ARGS and BODY. All p!-symbols in BODY are transformed to gensyms."
  `(defmacro ,src-name (name args &body body)
     (multiple-value-bind (env args) (ensure-environment-in-args args)
       (let ((body (progn-flatten-p!-syms args body env)))
	 `(,',dst-name ,name ,args ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last p! defmacro))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun crunch-in-internal-def (body package)
    (multiple-value-bind (forms decls doc) (parse-body body)
      (append (if doc (list doc))
	      decls
	      `((flet ((intern-def (name)
			 (intern name ,package)))
		  ,@forms))))))

(define-feature-adder/g! i! (args &body body)
  "Add a INTERN-DEF form, that interns a symbol into the package, where
the macro/function was defined, not where it is expanded/called"
  `(defmacro ,src-name ,args
     (let ((,(intern "BODY") (crunch-in-internal-def body *package*)))
       (append `(,',dst-name)
	       (progn ,@body)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last i! defmacro)
  (add-to-last i! defun))

(defmacro add-to-last-/sampling! (object)
  (let* ((dst-name (or (gethash object *last-versions*)
		       (setf (gethash object *last-versions*) object)))
	 (src-name (intern (concatenate 'string (string dst-name) "/SA!"))))
    `(progn (define-/sampling! ,src-name ,dst-name)
	    (setf (gethash ',object *last-versions*) ',src-name))))

(defmacro add-to-last-/splicing! (object)
  (let* ((dst-name (or (gethash object *last-versions*)
		       (setf (gethash object *last-versions*) object)))
	 (src-name (intern (concatenate 'string (string dst-name) "/SP!"))))
    `(progn (define-/splicing! ,src-name ,dst-name)
	    (setf (gethash ',object *last-versions*) ',src-name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (add-to-last-/sampling! defmacro)
  #+sbcl
  (add-to-last-/splicing! defmacro))

;; If some new defmacro/'s will be added, then change the following alias macroexpansion

(defmacro extract-last-definition! (object &rest args-body)
  (let ((exclam-name (intern (concatenate 'string (string object) "!"))))
    `(defmacro ,exclam-name ,(or (car args-body)
				 '(name args &body body))
       (append (list ',(or (gethash object *last-versions*)
			   object))
	       ,(or (cadr args-body)
		    ``(,name ,args ,@body))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (extract-last-definition! defmacro)
  (extract-last-definition! defmacro-driver (clause-template &body body)
			    `(,clause-template ,@body))
  (extract-last-definition! defun))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((frob (x)
	       `(extract-last-definition! ,x (definitions &body body)
					  `(,definitions ,@body))))
    (frob macrolet)
    (frob flet)
    (frob labels)))

(defun hash->assoc (hash)
  (iter (for (key val) in-hashtable hash)
	(collect (cons key val))))
