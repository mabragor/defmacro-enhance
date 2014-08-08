;;;; defmacro-enhance.asd

(defpackage :defmacro-enhance-system
  (:use :cl :asdf))

(in-package defmacro-enhance-system)

(defsystem :defmacro-enhance
  :serial t
  :description "Deformation of defmacro to defmacro!, macrolet to macrolet! etc. in spirit of let-over-lambda."
  :version "0.3"
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  ;; do not depend ot rutils, since I plan to rewrite anaphoric macros there with help of this package
  :depends-on (:alexandria :iterate :cl-splicing-macro :hu.dwim.walker #:cl-indeterminism)
  :components ((:file "package")
               (:file "defmacro-enhance")))

(defsystem :defmacro-enhance-tests
  :description "Tests for DEFMACRO-ENHANCE."
  :licence "GPL"
  :depends-on (:defmacro-enhance :eos)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :defmacro-enhance))))
  (load-system :defmacro-enhance-tests)
  (funcall (intern "RUN-TESTS" :defmacro-enhance-tests)))
