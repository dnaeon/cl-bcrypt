#!/usr/bin/env sh

set -e

sbcl --eval '(ql:quickload :cl-bcrypt.test)' \
     --eval '(setf rove:*enable-colors* nil)' \
     --eval '(asdf:test-system :cl-bcrypt.test)' \
     --eval '(quit)'
