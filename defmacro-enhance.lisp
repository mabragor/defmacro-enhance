;;;; defmacro-enhance.lisp
;;;;
;;;; This file is part of the defmacro-enhance library, released under GPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Alexander Popolitov

(in-package #:defmacro-enhance)

(defmacro def-*!-symbol-p (symb)
  (let ((s-symb (string symb)))
    `(defun ,(intern (format nil "~a~a"(string s-symb) "!-SYMBOL-P")) (symb)
       (and (symbolp symb)
	    (> (length (string symb)) ,(+ (length s-symb) 2))
	    (equal (subseq (string symb) 0 ,(+ (length s-symb) 2))
		   ,(format nil "~a~a" (string s-symb) "!-"))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def-*!-symbol-p g)
  (def-*!-symbol-p o)
  (def-*!-symbol-p e))

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
      

(defmacro defmacro/g! (name args &body body)
  (multiple-value-bind (forms decls doc) (parse-body body)
    (let ((syms (remove-duplicates
		 (remove-if-not #'g!-symbol-p
				(alexandria:flatten forms)))))
      `(defmacro ,name ,args
	 ,@(if doc `(,doc))
	 ,@decls
	 ,@(if syms
	       `((let ,(mapcar (lambda (s)
				 `(,s (gensym ,(subseq (symbol-name s) 3))))
			       syms)
		   ,@forms))
	       forms)))))

;; ONCE-ONLY just not to depend on RUTILS, which I plan to enhance
;; Limited version, to use once only for my needs.
(defmacro once-only ((&rest specs) &body body)
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
		,@body)))))

(defmacro defmacro/g!/o! (name args &body body)
  (multiple-value-bind (forms decls doc) (parse-body body)
    (let ((syms (remove-duplicates
		 (remove-if-not #'o!-symbol-p (alexandria:flatten args)))))
      `(defmacro/g! ,name ,args
	 ,@(if doc `(,doc))
	 ,@decls
	 (once-only ,syms
	   ,@forms)))))

(defmacro defmacro/g!/o!/e! (name args &body body)
  (multiple-value-bind (forms decls doc) (parse-body body)
    (let ((syms (remove-duplicates
		 (remove-if-not #'e!-symbol-p (alexandria:flatten forms)))))
      `(defmacro/g!/o! ,name ,args
	 ,@(if doc `(,doc))
	 ,@decls
	 (let ,(mapcar (lambda (sym)
			 `(,sym (intern ,(subseq (string sym) 3))))
		       syms)
	   ,@forms)))))

;; If some new defmacro/'s will be added, then change the following alias macroexpansion
(defmacro defmacro! (name args &body body)
  `(defmacro/g!/o!/e! ,name ,args ,@body))
