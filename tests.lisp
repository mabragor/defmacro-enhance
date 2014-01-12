(in-package #:cl-user)

(defpackage #:defmacro-enhance-tests
  (:use #:cl #:defmacro-enhance #:eos)
  (:export #:run-tests))

(in-package #:defmacro-enhance-tests)

(def-suite defmacro-enhance)
(in-suite defmacro-enhance)

(defun run-tests ()
  (let ((results (run 'defmacro-enhance)))
    (eos:explain! results)
    (unless (eos:results-status results)
      (error "Tests failed."))))
	    
;; Tests are not direct but rather indirect

(defmacro! aif (test then &optional else)
  `(let ((,e!-it ,test))
     (if ,e!-it
	 ,then
	 ,else)))

(test externalization
  (is (equal 3 (aif (+ 1 2) 3 6)))
  (is (equal 3 (macrolet! ((my-aif (test then &optional else)
				   `(let ((,e!-it ,test))
				      (if ,e!-it
					  ,then
					  ,else))))
		 (my-aif (+ 1 2)
			 3
			 6))))
  )
      

(defmacro! square (o!-x)
  `(* ,o!-x ,o!-x))

(test once-only
  (is (equal '(1 1) (let ((x 0))
		      `(,(square (incf x)) ,x)))))

(defmacro! foo (a b &sample (1 2))
  `(+ ,a ,b))

(test sampling
  (is (equal '(let ()
	       (+ 1 2))
	     (testing-expansion foo))))
