;;;; defmacro-enhance.lisp

(in-package #:defmacro-enhance)

(defmacro def-*!-symbol-p (symb)
  (let ((s-symb (string symb)))
    `(defun ,(sb-int:symbolicate s-symb "!-SYMBOL-P") (symb)
       (and (symbolp symb)
	    (> (length (string symb)) ,(+ (length s-symb) 2))
	    (equal (subseq (string symb) 0 ,(+ (length s-symb) 2))
		   ,(rutils.string:strcat s-symb "!-"))))))

(rutils.symbol:eval-always
  (def-*!-symbol-p g)
  (def-*!-symbol-p o)
  (def-*!-symbol-p e))

;; Now I want defmacro/g!, that can correctly process docstrings and declarations.
;; Portability will be considered later.

(defmacro defmacro/g! (name args &body body)
  (multiple-value-bind (forms decls doc) (sb-int:parse-body body)
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

(defmacro defmacro/g!/o! (name args &body body)
  (multiple-value-bind (forms decls doc) (sb-int:parse-body body)
    (let ((syms (remove-duplicates
		 (remove-if-not #'o!-symbol-p (alexandria:flatten args)))))
      `(defmacro/g! ,name ,args
	 ,@(if doc `(,doc))
	 ,@decls
	 (rutils.symbol:once-only ,syms
	   ,@forms)))))

(defmacro defmacro/g!/o!/e! (name args &body body)
  (multiple-value-bind (forms decls doc) (sb-int:parse-body body)
    (let ((syms (remove-duplicates
		 (remove-if-not #'e!-symbol-p (alexandria:flatten forms)))))
      `(defmacro/g!/o! ,name ,args
	 ,@(if doc `(,doc))
	 ,@decls
	 (let ,(mapcar (lambda (sym)
			 `(,sym (intern ,(subseq (string sym) 3))))
		       syms)
	   ,@forms)))))

(defmacro defmacro! (name args &body body)
  `(defmacro/g!/o!/e! ,name ,args ,@body))
