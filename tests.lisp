
(defpackage #:defmacro-enhance-test
  (:use #:cl #:defmacro-enhance #:rt))

(in-package #:defmacro-enhance-test)
	    
;; Tests are not direct but rather indirect

(defmacro! aif (test then &optional else)
  `(let ((,e!-it ,test))
     (if ,e!-it
	 ,then
	 ,else)))

(deftest e!.0
    (aif (+ 1 2)
	 3
	 6)
  3)

(deftest e!.1
    (macrolet! ((my-aif (test then &optional else)
			`(let ((,e!-it ,test))
			   (if ,e!-it
			       ,then
			       ,else))))
      (my-aif (+ 1 2)
	      3
	      6))
  3)
      

(defmacro! square (o!-x)
  `(* ,o!-x ,o!-x))

(deftest o!.0
    (let ((x 0))
      (values (square x) x))
  1 1)
