;; Copyright (c) 2020 Marin Atanasov Nikolov <dnaeon@gmail.com>
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;;
;;  1. Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer
;;     in this position and unchanged.
;;  2. Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR(S) ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR(S) BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(in-package :cl-user)
(defpackage :cl-bcrypt.test
  (:use :cl :rove)
  (:import-from :cl-bcrypt))
(in-package :cl-bcrypt.test)

(deftest bcrypt-2a
  (testing "Decode bcrypt 2a password hashes"
    (let* ((hash-1 "$2a$08$6j7SQUbgKFkos8Q0tWgKW.SYIOz6fNRltZChW5DvRvvyJdZ/ldXEO")
           (hash-2 "$2a$12$9639BJoFec8B.H0ZggqpIuwcxGXnB9lGk9xvXGYHlA8zKT6JbbGpC")
           (hash-3 "$2a$16$gShh6Rp/pFmAYJpS8WvgEexRfLUIaXy0cVtOAIyjXoIGlJoCQoV/i")
           (pass-1 (bcrypt:decode hash-1))
           (pass-2 (bcrypt:decode hash-2))
           (pass-3 (bcrypt:decode hash-3))
           (parsed-1 (bcrypt:parse-hash-or-lose hash-1))
           (parsed-2 (bcrypt:parse-hash-or-lose hash-2))
           (parsed-3 (bcrypt:parse-hash-or-lose hash-3)))
      (ok (string= "2a" (bcrypt:algorithm-identifier pass-1))
          "Algorithm identifier matches for pass-1")
      (ok (string= "2a" (bcrypt:algorithm-identifier pass-2))
          "Algorithm identifier matches for pass-2")
      (ok (string= "2a" (bcrypt:algorithm-identifier pass-3))
          "Algorithm identifier matches for pass-3")
      (ok (= 8 (bcrypt:cost-factor pass-1))
          "Cost factor for pass-1 is 8")
      (ok (= 12 (bcrypt:cost-factor pass-2))
          "Cost factor for pass-2 is 12")
      (ok (= 16 (bcrypt:cost-factor pass-3))
          "Cost factor for pass-3 is 16")
      (ok (string= "6j7SQUbgKFkos8Q0tWgKW." (getf parsed-1 :salt))
          "Salt for hash-1 matches")
      (ok (string= "9639BJoFec8B.H0ZggqpIu" (getf parsed-2 :salt))
          "Salt for hash-2 matches")
      (ok (string= "gShh6Rp/pFmAYJpS8WvgEe" (getf parsed-3 :salt))
          "Salt for hash-3 matches")
      (ok (string= "SYIOz6fNRltZChW5DvRvvyJdZ/ldXEO"
                   (getf parsed-1 :password-hash))
          "Password hash for hash-1 matches")
      (ok (string= "wcxGXnB9lGk9xvXGYHlA8zKT6JbbGpC"
                   (getf parsed-2 :password-hash))
          "Password hash for hash-2 matches")
      (ok (string= "xRfLUIaXy0cVtOAIyjXoIGlJoCQoV/i"
                   (getf parsed-3 :password-hash))
          "Password hash for hash-3 matches")
      (ok (bcrypt:password= "foo" hash-1)
          "Password matches for hash-1")
      (ok (bcrypt:password= "bar" hash-2)
          "Password matches for hash-2")
      (ok (bcrypt:password= "baz" hash-3)
          "Password matches for hash-3")))

  (testing "Create bcrypt 2a passwords"
    (let* ((pass-1 (bcrypt:make-password "foo" :cost 8 :identifier "2a"))
           (pass-2 (bcrypt:make-password "bar" :cost 12 :identifier "2a"))
           (pass-3 (bcrypt:make-password "baz" :cost 16 :identifier "2a"))
           (encoded-1 (bcrypt:encode pass-1))
           (encoded-2 (bcrypt:encode pass-2))
           (encoded-3 (bcrypt:encode pass-3)))
      (ok (string= "2a" (bcrypt:algorithm-identifier pass-1))
          "Algorithm identifier matches for pass-1")
      (ok (string= "2a" (bcrypt:algorithm-identifier pass-2))
          "Algorithm identifier matches for pass-2")
      (ok (string= "2a" (bcrypt:algorithm-identifier pass-3))
          "Algorithm identifier matches for pass-3")
      (ok (= 8 (bcrypt:cost-factor pass-1))
          "Cost factor for pass-1 is 8")
      (ok (= 12 (bcrypt:cost-factor pass-2))
          "Cost factor for pass-2 is 12")
      (ok (= 16 (bcrypt:cost-factor pass-3))
          "Cost factor for pass-3 is 16")
      (ok (= 60 (length encoded-1))
          "Length of encoded pass-1 is 60")
      (ok (= 60 (length encoded-2))
          "Length of encoded pass-2 is 60")
      (ok (= 60 (length encoded-3))
          "Length of encoded pass-3 is 60")
      (ok (bcrypt:password= "foo" encoded-1)
          "Password matches for pass-1")
      (ok (bcrypt:password= "bar" encoded-2)
          "Password matches for pass-2")
      (ok (bcrypt:password= "baz" encoded-3)
          "Password matches for pass-3"))))

(deftest invalid-bcrypt-hashes
  (let ((hash-1 "$2x$12$5HSmyomJzLwKZJ0GUycGOO/8c7IGhJbGMzIhM3nk3IsP/iXIDlW6i") ;; Invalid identifier: 2x
        (hash-2 "$2y$12$rQAkuoCwuOdCWuzjjJZZyeC3h/kI4.fgds2.GjaOX9ID1di7j3BEm") ;; Invalid identifier: 2y
        (hash-3 "$2$12$lUzKoIuxFUtK3br2FXiviedgh/uUufKwYqpwWZHh3BCSF6HTXhBDy")) ;; Invalid identifier: 2
    (ok (signals (bcrypt:decode hash-1))
        "Decode invalid bcrypt hash-1")
    (ok (signals (bcrypt:decode hash-2))
        "Decode invalid bcrypt hash-2")
    (ok (signals (bcrypt:decode hash-3))
        "Decode invalid bcrypt hash-3")))
