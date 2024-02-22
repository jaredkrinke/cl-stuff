(defpackage :halp/crypto
  (:use :cl)
  (:export #:fixed-xor
	   #:decrypt-single-byte-xor
	   #:score-english
	   #:score-english-byte))

(in-package :halp/crypto)

(defun fixed-xor (a b)
  "XORs A with B"
  (map 'vector #'logxor a b))

(defparameter *english-character-score*
  (let ((scores (make-array 256 :initial-element -2))) ; Default to uncommon
    (setf (aref scores 0) 0) ; NUL is neutral
    ;; Unprintable characters are very uncommon
    (loop for code from 1 upto (char-code #\Tab) do
      (setf (aref scores code) -20))
    ;; Non-ASCII is very uncommon
    (loop for code from 128 below 256 do
      (setf (aref scores code) -20))
    ;; Letters are very common
    (loop for code from (char-code #\a) upto (char-code #\z) do
      (setf (aref scores code) 10))
    (loop for code from (char-code #\A) upto (char-code #\Z) do
      (setf (aref scores code) 10))
    ;; Numbers are pretty common
    (loop for code from (char-code #\0) upto (char-code #\9) do
      (setf (aref scores code) 5))
    ;; Spaces are very common
    (setf (aref scores (char-code #\Space)) 20)
    ;; Some letters are *very* common
    (loop for (character score) in '((#\e 30)
				     (#\t 20)
				     (#\a 20)
				     (#\i 15)
				     (#\n 15))
	  do (setf (aref scores (char-code character)) score)
	     (setf (aref scores (char-code (char-upcase character))) score))
    ;; Punctuation is somewhat common
    (loop for character across ",.'\":;!-?/$%()&" for code = (char-code character) do
      (setf (aref scores code) 1))
    scores))

(defun score-english-byte (byte)
  "Scores a possibly English byte (higher means more English-like)"
  (aref *english-character-score* byte))

(defun score-english (bytes)
  "Scores a possibly-English string encoded as BYTES (higher score means more English-like)"
  ;; Trivial heuristic: letters, numbers, and punctuation are good; others are not
  (loop for byte across bytes
	sum (score-english-byte byte)))

(defun decrypt-single-byte-xor (bytes)
  "Decrypts a single byte-XOR'd sequence of bytes using SCORE-ENGLISH, returning (VALUES DECRYPTED-BYTES ENCRYPTION-KEY BEST-SCORE)"
  (loop with best-score = nil
	with result = nil
	for byte from 0 below 256
	for cipher = (make-array (length bytes) :initial-element byte)
	for decrypted-message = (fixed-xor bytes cipher)
	for score = (score-english decrypted-message)
	do (when (or (not best-score)
		     (> score best-score))
	     (setf best-score score)
	     (setf result decrypted-message))
	finally (return (values result
				byte
				best-score))))
