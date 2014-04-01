;;;; defmacro-enhance.lisp
;;;;
;;;; This file is part of the defmacro-enhance library, released under GPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Alexander Popolitov

(in-package #:defmacro-enhance)

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
    (let (doc-string)
      (if (and (> (length body) 1)
	       (stringp (car body)))
	  (setf doc-string (car body)
		body (cdr body)))
      (iter (with non-decl-met = nil)
	    (for form in body)
	    (if (not non-decl-met)
		(if (equal (car form) 'declare)
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
  (defun declare-g!-syms-as-gsyms (body)
    (multiple-value-bind (forms decls doc) (parse-body body)
      (grep-spec-syms (syms g! forms)
	(transforming-body-when-syms
	  `(let ,(mapcar (lambda (s)
			   `(,s (gensym ,(subseq (symbol-name s) 3))))
			 syms)
	     ,@forms))))))

(defmacro define-/g! (src-name dst-name args &body body)
  "Define macro SRC-NAME on top of DST-NAME. Arg list of DST-NAME should
contain BODY. All g!-symbols in BODY are transformed to gensyms."
  `(defmacro ,src-name ,args
     (let ((,(intern "BODY") (declare-g!-syms-as-gsyms body)))
       (append `(,',dst-name)
	       (progn ,@body)))))

(define-/g! defmacro/g! defmacro (name args &body body)
  `(,name ,args ,@body))

(define-/g! defmacro-driver/g! defmacro-driver (clause-template &body body)
  `(,clause-template ,@body))

(define-/g! defun/g! defun (name args &body body)
  `(,name ,args ,@body))

(macrolet ((frob (src-name dst-name)
	     `(defmacro ,src-name (definitions &body body)
		`(,',dst-name
		  ,(mapcar (lambda (x)
			     (destructuring-bind (name args &body body) x
			       `(,name ,args
				       ,@(declare-g!-syms-as-gsyms body))))
			   definitions)
		  ,@body))))
  (frob macrolet/g! macrolet)
  (frob flet/g! flet)
  (frob labels/g! labels))


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

(defmacro/g! define-/o! (src-name dst-name args &body body)
  "All o!-symbols in the ARGS variable of the DST-NAME macro are
treated as once-only symbols. BODY variable is parsed accordingly."
  `(defmacro ,src-name ,args
     (let ((,(intern "BODY") (make-o!-once-only args body)))
       (append `(,',dst-name)
	       (progn ,@body)))))

(define-/o! defmacro/g!/o! defmacro/g! (name args &body body)
  `(,name ,args ,@body))

(defmacro macrolet/g!/o! (definitions &body body)
  `(macrolet/g! ,(mapcar (lambda (x)
			   (destructuring-bind (name args &body body) x
			     `(,name ,args
				     ,@(make-o!-once-only args body))))
			 definitions)
     ,@body))

(defun make-e!-internable (body)
  (multiple-value-bind (forms decls doc) (parse-body body)
    (grep-spec-syms (syms e! forms)
      (transforming-body-when-syms
	`(let ,(mapcar (lambda (sym)
			 `(,sym (intern ,(subseq (string sym) 3))))
		       syms)
	   ,@forms)))))

(defmacro/g! define-/e! (src-name dst-name args &body body)
  "Deformation, in which E!-symbols are interned in package,
where macro is expanded, not where it is defined.
Useful for writing anaphoric macros."
  `(defmacro ,src-name ,args
     (let ((,(intern "BODY") (make-e!-internable body)))
       (append `(,',dst-name)
	       (progn ,@body)))))


(define-/e! defmacro/g!/o!/e! defmacro/g!/o! (name args &body body)
  `(,name ,args ,@body))

(define-/e! defmacro-driver/g!/e! defmacro-driver/g!
    (clause-template &body body)
  `(,clause-template ,@body))

(define-/e! defun/g!/e! defun/g! (name args &body body)
  `(,name ,args ,@body))

(macrolet ((frob (src-name dst-name)
	     `(defmacro ,src-name (definitions &body body)
		`(,',dst-name
		  ,(mapcar (lambda (x)
			     (destructuring-bind (name args &body body) x
			       `(,name ,args
				       ,@(make-e!-internable body))))
			   definitions)
		  ,@body))))
  (frob macrolet/g!/o!/e! macrolet/g!/o!)
  (frob flet/g!/e! flet/g!)
  (frob labels/g!/e! labels/g!))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun progn-flatten-p!-syms (args body lexenv-sym)
    (multiple-value-bind (forms decls doc) (parse-body body)
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

(defmacro define-/p! (src-name dst-name)
  "Define macro SRC-NAME on top of DST-NAME. Arg list of DST-NAME should
contain ARGS and BODY. All p!-symbols in BODY are transformed to gensyms."
  `(defmacro ,src-name (name args &body body)
     (let ((g!-env (gensym "ENV")))
       (let ((body (progn-flatten-p!-syms args body g!-env)))
	 `(,',dst-name ,name ,(append args (list '&environment g!-env))
		       ,@body)))))

(define-/p! defmacro/g!/o!/e!/p! defmacro/g!/o!/e!)


(define-/sampling! defmacro/g!/o!/e!/p!/sa! defmacro/g!/o!/e!/p!)

#+sbcl
(define-/splicing! defmacro/g!/o!/e!/p!/sa!/sp! defmacro/g!/o!/e!/p!/sa!)

;; If some new defmacro/'s will be added, then change the following alias macroexpansion

(defmacro defmacro! (name args &body body)
  "Like defmacro, but with some extra perks."
  `(eval-when (:compile-toplevel :load-toplevel :execute) ; this is present in DEFMACRO, and hence here.
     (#+sbcl defmacro/g!/o!/e!/p!/sa!/sp!
	     #-sbcl defmacro/g!/o!/e!/p!/sa!
	     ,name ,args ,@body)))

(defmacro defmacro-driver! (clause-template &body body)
  "Like defmacro, but with some extra perks."
  `(defmacro-driver/g!/e! ,clause-template ,@body))
 
(defmacro defun! (name args &body body)
  `(defun/g!/e! ,name ,args ,@body))

(macrolet ((frob (src dst)
	     `(defmacro ,src (definitions &body body)
		`(,',dst ,definitions ,@body))))
  (frob macrolet! macrolet/g!/o!/e!)
  (frob flet! flet/g!/e!)
  (frob labels! labels/g!/e!))
