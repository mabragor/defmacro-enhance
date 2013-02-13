;;;; defmacro-enhance.asd

(asdf:defsystem #:defmacro-enhance
  :serial t
  :description "Promotion of defmacro to defmacro! in spirit of let-over-lambda."
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  ;; do not depend ot rutils, since I plan to rewrite anaphoric macros there with help of this package
  :depends-on (#:alexandria #:iterate)
  :components ((:file "package")
               (:file "defmacro-enhance")))

