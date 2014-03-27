;;;; package.lisp

(defpackage #:defmacro-enhance
  (:use #:cl #:iterate)
  (:shadowing-import-from #:cl-splicing-macro #:define-/sampling! #:testing-expansion
			  #+sbcl #:define-/splicing!
			  #+sbcl #:enable-splicing-macro
			  #+sbcl #:disable-splicing-macro
			  #+sbcl #:sprogn)
  (:export #:defmacro! #:defmacro-driver! #:defun!
	   #:define-/g! #:define-/o! #:define-/e!
	   #:macrolet! #:labels! #:flet! #:testing-expansion
	   #:define-/sampling!
	   #+sbcl #:define-/splicing!
	   #+sbcl #:enable-splicing-macro
	   #+sbcl #:disable-splicing-macro
	   #+sbcl #:sprogn))
  

