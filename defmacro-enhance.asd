;;;; defmacro-enhance.asd

(asdf:defsystem #:defmacro-enhance
  :serial t
  :description "Promotion of defmacro to defmacro! in spirit of let-over-lambda."
  :author "Alexander Popolitov <popolit@itep.ru>"
  :license "GPL"
  :depends-on (#:rutils #:alexandria)
  :components ((:file "package")
               (:file "defmacro-enhance")))

