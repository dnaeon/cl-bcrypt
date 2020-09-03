(defpackage :cl-bcrypt-system
  (:use :cl :asdf))
(in-package :cl-bcrypt-system)

(defsystem "cl-bcrypt"
  :name "cl-bcrypt"
  :long-name "cl-bcrypt"
  :description "Common Lisp system for generating and parsing of bcrypt password hashes"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :homepage "https://github.com/dnaeon/cl-bcrypt"
  :bug-tracker "https://github.com/dnaeon/cl-bcrypt"
  :source-control "https://github.com/dnaeon/cl-bcrypt"
  :depends-on (:binascii
               :ironclad)
  :components ((:module "core"
                :pathname #P"src/"
                :components ((:file "package")
                             (:file "bcrypt" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "cl-bcrypt.test"))))
