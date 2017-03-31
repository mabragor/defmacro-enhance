;;;; defmacro-enhance.asd

(defsystem "defmacro-enhance"
  :serial t
  :description "Deformation of defmacro to defmacro!, macrolet to macrolet! etc. in spirit of let-over-lambda."
  :version "0.5"
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  ;; do not depend ot rutils, since I plan to rewrite anaphoric macros there with help of this package
  :depends-on ("alexandria" "iterate" "cl-splicing-macro" "hu.dwim.walker" "cl-indeterminism")
  :components ((:file "package")
               (:file "defmacro-enhance"))
  :in-order-to ((test-op (test-op "defmacro-enhance/tests"))))

(defsystem "defmacro-enhance/tests"
  :description "Tests for DEFMACRO-ENHANCE."
  :licence "GPL"
  :depends-on ("defmacro-enhance" "eos")
  :components ((:file "tests"))
  :perform (test-op (o c) (symbol-call :defmacro-enhance-tests :run-tests)))
