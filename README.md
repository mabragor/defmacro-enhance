defmacro-enhance
================

Enhancement of defmacro in spirit of [let-over-lambda](http://www.letoverlambda.com/index.cl/toc).

Following features are supported:

1.  g!-symbols in body of a macro become gensyms:

        CL-USER> (defmacro! foo (bar)
                  `(let ((,g!-bar ,bar))
                     asdf))
        FOO          
        CL-USER> (macroexpand-1 '(foo (+ 1 2)))
        (LET ()         
          (LET ((#:BAR1115 (+ 1 2)))
            ASDF))                                                                                                                                 
        T                                                                                                                                                   
        CL-USER> 

2.  o!-symbols in the lambda-list of a macro become once-only arguments

        CL-USER> (defmacro! square (o!-x)
                   `(* ,o!-x ,o!-x))
        SQUARE                                                                                                                            
        CL-USER> (defparameter a 1)
        A
        CL-USER> (square (incf a))
        4
        CL-USER> (square (incf a))
        9
        CL-USER>

3.  e!-symbols in the body of the macro are interned in the package,
where macro is *expanded*, not where it is defined

        CL-USER> (in-package defmacro-enhance)
        #<PACKAGE "DEFMACRO-ENHANCE"> 
        DEFMACRO-ENHANCE> (defmacro! aif (test then &optional else)
                            `(let ((,e!-it ,test)) 
                               (if ,e!-it
                                   ,then
                                   ,@(if else `(,else)))))
        AIF                                                                                                   
        DEFMACRO-ENHANCE> (in-package cl-user)
        #<PACKAGE "COMMON-LISP-USER">
        CL-USER> (macroexpand-1 '(defmacro-enhance::aif (+ 1 2) it 6))
        (LET () 
          (LET ((IT (+ 1 2)))
            (IF IT
                IT
		6)))
        T
        CL-USER> (defmacro-enhance::aif (+ 1 2) it 6)
        3
        CL-USER>

As is noted in the header of let-over-lambda code, all the differences from that
version should be clearly noted.
Here they are:

1.  letter-bang-hyphen symbols are used instead of letter-bang symbols to denote symbols,
    for which something special should be done (E.g g!-foo instead of g!foo)

2.  In once-only functionality name of a symbol in body of a macro is the same as in the
    lambda-list of arguments. I.e., instead of let-over-lambda's

        (defmacro square (o!x)
          `(* ,g!x ,g!x)) ; why in the world did g! appeared here? And is o! still accessible?

    we have

        (defmacro square (o!-x)
          `(* ,o!-x ,o!-x))

3.  New, e!-symbols are added, that allow to express anaphoric macros more conveniently.


Few code-duplication was necessary, to make code portable (and not SBCL specific) and to be
able not to depend on RUTILS, since the plan is to rewrite anaphoric utilities there through e!- construction.