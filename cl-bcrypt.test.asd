(defpackage cl-bcrypt-test-system
  (:use :cl :asdf))
(in-package :cl-bcrypt-test-system)

(defsystem "cl-bcrypt.test"
  :name "cl-bcrypt.test"
  :long-name "cl-bcrypt.test"
  :description "Test suite for cl-bcrypt system"
  :version "0.1.0"
  :author "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :maintainer "Marin Atanasov Nikolov <dnaeon@gmail.com>"
  :license "BSD 2-Clause"
  :homepage "https://github.com/dnaeon/cl-bcrypt"
  :bug-tracker "https://github.com/dnaeon/cl-bcrypt"
  :source-control "https://github.com/dnaeon/cl-bcrypt"
  :depends-on (:cl-bcrypt
               :rove)
  :components ((:module "tests"
                :pathname #P"t/"
                :components ((:file "test-suite"))))
  :perform (test-op (op c) (uiop:symbol-call :rove :run-suite :cl-bcrypt.test)))
