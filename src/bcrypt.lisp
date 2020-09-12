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
(defpackage :cl-bcrypt
  (:use :cl)
  (:nicknames :bcrypt)
  (:import-from :binascii)
  (:import-from :ironclad)
  (:import-from :cl-ppcre)
  (:export
   :*default-cost-factor*
   :generate-salt
   :password
   :algorithm-identifier
   :cost-factor
   :salt
   :password-hash
   :bcrypt-error
   :bcrypt-error-description
   :make-password
   :b64-encode
   :b64-decode
   :encode
   :parse-hash
   :parse-hash-or-lose
   :decode
   :password=))
(in-package :cl-bcrypt)

(defparameter *alphabet*
  "./ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
  "Alphabet used for base64 encoding and decoding of bcrypt password hashes")

(defparameter *b64-encode-table*
  (coerce *alphabet* 'simple-base-string)
  "Table used for base64 encoding of a password hash")

(defparameter *b64-decode-table*
  (binascii::make-decode-table *alphabet*)
  "Table used for base64 decoding of a password hash")

(defparameter *supported-algorithm-identifiers*
  '("2a" "2b")
  "Supported algorithm identifiers")

(defparameter *bcrypt-hash-regex-scanner*
  (cl-ppcre:create-scanner "^\\$(2[ab])\\$(\\d{2})\\$([a-zA-Z0-9\./]{22})([a-zA-Z0-9\./]{31})$")
  "Regex used to match bcrypt hashes")

(defparameter *default-cost-factor*
  12
  "The default cost factor")

(defconstant +raw-hash-size+
  24
  "Number of bytes in the raw password hash")

(defconstant +raw-hash-bytes-to-encode+
  23
  "Number of bytes from the raw hash to be encoded")

(defconstant +encoded-hash-size+
  31
  "Number of characters in the encoded password hash")

(defconstant +raw-salt-size+
  16
  "Number of bytes in the raw salt")

(defconstant +encoded-salt-size+
  22
  "Number of characters that represent an encoded salt")

(defconstant +max-plain-text-password-size+
  72
  "Maximum number of characters of a plain-text password")

(define-condition bcrypt-error (simple-error)
  ((description
    :initarg :description
    :reader bcrypt-error-description))
  (:documentation "Bcrypt error condition")
  (:report (lambda (condition stream)
             (format stream "~a" (bcrypt-error-description condition)))))

(defclass password ()
  ((algorithm-identifier
    :initarg :algorithm-identifier
    :initform (error "Must specify hash algorithm identifier")
    :reader algorithm-identifier
    :documentation "The hash algorithm identifier")
   (cost-factor
    :initarg :cost-factor
    :initform (error "Must specify cost factor")
    :reader cost-factor
    :documentation "The password cost factor")
   (salt
    :initarg :salt
    :initform (error "Must specify password salt")
    :reader salt
    :documentation "16 bytes size salt")
   (password-hash
    :initarg :password-hash
    :initform (error "Must specify password hash")
    :reader password-hash
    :documentation "The hashed password"))
  (:documentation "Class which represents a bcrypt password"))

(defun generate-salt ()
  "Generates a random 16 bytes size salt"
  (ironclad:random-data +raw-salt-size+))

(defun b64-encode (octets)
  "base64 encodes the given octets using our alphabet"
  (let ((binascii::*base64-encode-table* *b64-encode-table*))
    (binascii:encode-base64 octets)))

(defun b64-decode (octets)
  "base64 decodes the given octets using our alphabet"
  (let ((binascii::*base64-decode-table* *b64-decode-table*))
    (binascii:decode-base64 octets)))

(defun make-password (password &key
                                 (salt (generate-salt))
                                 (cost *default-cost-factor*)
                                 (identifier "2b"))
  "Creates a new bcrypt password instance.
The PASSWORD should be no more than 72 characters long.
The COST should be a number between 4 and 31. The SALT is a random 16
bytes sequence, which will be generated, unless explicitely provided.
Supported IDENTIFIER values are 2a and 2b."
  (declare (type (simple-array (unsigned-byte 8) (*)) salt)
           (type fixnum cost)
           (type string password identifier))
  (unless (<= (length password) +max-plain-text-password-size+)
    (error 'bcrypt-error :description "Password size is more than 72 characters"))
  (unless (= (length salt) +raw-salt-size+)
    (error 'bcrypt-error :description "Salt should be exactly 16 bytes"))
  (unless (<= 4 cost 31)
    (error 'bcrypt-error :description "Cost factor should be between 4 and 31"))
  (unless (member identifier *supported-algorithm-identifiers*
                  :test #'equal)
    (error 'bcrypt-error :description "Algorithm identifier is not supported"))
  (let* ((passphrase (ironclad:ascii-string-to-byte-array password))
         (iterations (expt 2 cost))
         (kdf (ironclad:make-kdf :bcrypt))
         (key (ironclad:derive-key kdf passphrase salt iterations +raw-hash-size+)))
    ;; See the comments in ENCODE function about why we truncate the
    ;; raw hash (represented by KEY here) to +RAW-HASH-BYTES-TO-ENCODE+.
    ;; In order to have the same number of bytes in the raw hash when
    ;; decoding a password hash (using DECODE) and creating a new one
    ;; when using MAKE-PASSWORD we are truncating it here to keep both
    ;; byte sequences the same in terms of length and values.
    (make-instance 'password
                   :algorithm-identifier identifier
                   :cost-factor cost
                   :salt salt
                   :password-hash (subseq key 0 +raw-hash-bytes-to-encode+))))

(defun encode (password)
  "Encodes the given PASSWORD instance into its text representation"
  (let* ((stream (make-string-output-stream))
         (identifier (algorithm-identifier password))
         (cost (cost-factor password))
         (salt (salt password))
         (salt-encoded (b64-encode salt))
         (hash (password-hash password))
         (hash-encoded (b64-encode (subseq hash 0 +raw-hash-bytes-to-encode+))))
    ;; In order to be compatible with other bcrypt implementations the
    ;; raw hash that gets encoded is truncated to the 23rd byte,
    ;; before the actual encoding.  The resulting hash is 32
    ;; characters long and needs to be truncated to the 31st
    ;; character. The salt is 16 bytes long, which when encoded is 24
    ;; characters long, which is also truncated to the 22nd character.
    (write-string (format nil "$~a$~2,'0d$" identifier cost) stream)
    (write-string (subseq salt-encoded 0 +encoded-salt-size+) stream)
    (write-string (subseq hash-encoded 0 +encoded-hash-size+) stream)
    (get-output-stream-string stream)))

(defun parse-hash (hash-string)
  "Parses an encoded bcrypt hash from the given HASH-STRING"
  (cl-ppcre:register-groups-bind (identifier cost salt hash)
      (*bcrypt-hash-regex-scanner* hash-string)
    (list :algorithm-identifier identifier
          :cost-factor cost
          :salt salt
          :password-hash hash)))

(defun parse-hash-or-lose (hash-string)
  (let ((parsed (parse-hash hash-string)))
    (unless parsed
      (error 'bcrypt-error :description "Invalid bcrypt hash"))
    parsed))

(defun decode (hash-string)
  "Decodes the given HASH-STRING into a PASSWORD instance"
  (let* ((parsed (parse-hash-or-lose hash-string))
         (identifier (getf parsed :algorithm-identifier))
         (cost-factor (parse-integer (getf parsed :cost-factor)))
         (salt (b64-decode (getf parsed :salt)))
         (password-hash (b64-decode (getf parsed :password-hash))))
    (make-instance 'password
                   :algorithm-identifier identifier
                   :cost-factor cost-factor
                   :salt salt
                   :password-hash password-hash)))

(defun password= (password-string hash-string)
  "Test whether the PASSWORD-STRING is equal to HASH-STRING when encoded"
  (declare (type string password-string hash-string))
  (let* ((decoded-password (decode hash-string))
         (new-password (make-password password-string
                                      :salt (salt decoded-password)
                                      :cost (cost-factor decoded-password)
                                      :identifier (algorithm-identifier decoded-password)))
         (encoded-hash (encode new-password)))
    (string= encoded-hash hash-string)))
