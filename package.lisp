
;;;;    package.lisp


(in-package #:common-lisp-user)

(defpackage #:slurp
  (:documentation "Slurp implementation.")
  (:use #:common-lisp)
  (:export #:checkout
           #:checkout-all
           #:update
           #:update-all
           #:update-all-starting-with))
