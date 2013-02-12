defmacro-enhance
================

Enhancement of defmacro in spirit of let-over-lambda.

As is noted in the header of let-over-lambda code, all the differences from that
version should be clearly noted.
Here they are:
1. Special symbols are denoted <letter>!-<name> instead of <letter>!<name>. I think, this is more readable -
g!x is perceived as g separated from x by something, but in g!-x hyphen ties two parts together.

2. No reuse of g!- construction is made in o!- construction. That is, instead of obscure LOL's notation
(defmacro square (o!x)
  `(* ,g!x ,g!x)) ; why in the world did g! appeared here? And is o! still accessible?
one should write
(defmacro squate (o!-x)
  `(* ,o!-x ,o!-x))

3. Finally, extra deformation of defmacro construction is made with help of e!- symbols. Namely, problem with,
e.g. anaphoric macro AIF, written as
(defmacro aif (test then &optional else)
  (let ((it ,test))
    (if it
        ,then
        ,@(if else `(,else)))))
is, that it here belongs to the package, where macro is defined, not the package, where it is expanded.
If you are like me, and constantly fixing your package.lisp file annoys you, this is the aid.
(defmacro aif (test then &optional else)
  (let ((,e!-it ,test))
    (if ,e!-it
        ,then
        ,@(if else `(,else)))))
I.e. e!-it symbol is automatically assigned a value (intern "IT"), which happens to be a symbol living in
the package where macro is expanded.

Few code-duplication was necessary, to make code portable (and not SBCL specific) and to be
able not to depend on RUTILS, since the plan is to rewrite anaphoric utilities there through e!- construction.