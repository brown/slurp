
;;;;    slurp.asd

(cl:in-package #:common-lisp-user)

(defpackage #:slurp-system
  (:documentation "System definition for Slurp.")
  (:use #:common-lisp #:asdf))

(in-package #:slurp-system)

(defsystem slurp
  :name "Slurp"
  :description "Check out public Common Lisp source code repositories."
  :long-description "Slurp lets you easily check out public Common Lisp
source code repositories and keep your local copies up to date when the
repositories change."
  :version "0.3"
  :author "Robert Brown"
  :license "See file COPYING and the copyright messages in individual files."
  #+sbcl :depends-on #+sbcl (sb-posix)
  :components
  ((:cl-source-file "package")
   (:cl-source-file "slurp" :depends-on ("package"))))
