(defpackage :cryptopals1
  (:use :cl))

(in-package :cryptopals1)

;;; Challenge 1
(defun char-range (start end)
  "Returns all characters beginning with START and ending with END, in code order"
  (let ((start-code (char-code start))
	(end-code (char-code end)))
    (loop for code from start-code upto end-code collect (code-char code))))

(defparameter *base64-characters*
  (coerce (nconc (char-range #\A #\Z)
		 (char-range #\a #\z)
		 (char-range #\0 #\9)
		 (list #\+
		       #\/))
	  'vector)
  "Vector of Base64 encoding characters")

(defparameter *base64-padding-character* #\= "Character used for padding Base64 strings")

(defun hex->bytes (hex-string)
  "Convert HEX-STRING from a string of two-digit hexadecimal bytes to a byte vector"
  (let* ((hex-string-length (length hex-string))
	 (bytes (make-array (/ hex-string-length 2)
			    :element-type '(unsigned-byte 8)
			    :fill-pointer 0)))
    (loop for index from 0 below hex-string-length by 2
	  do (vector-push (parse-integer hex-string
					 :start index
					 :end (+ index 2)
					 :radix 16)
			  bytes))
    bytes))

(defun bytes->base64 (bytes &key (pad t))
  "Encodes a vector of BYTES as a Base64 string"
  (let ((stream (make-string-output-stream))
	(padding 0))
    ;; Read in bytes and write out Base64 digits as we go
    (loop with bytes-length = (length bytes)
	  with bits = 0
	  with x = 0
	  for index from 0 upto bytes-length
	  for reading = (< index bytes-length)
	  do (if reading
		 (progn
		   ;; Shift in the next byte
		   (setf x (logior (ash x 8) (aref bytes index)))
		   (incf bits 8))
		 (when (> bits 0)
		   ;; Add padding bits
		   (setf padding (- 6 bits))
		   (setf x (ash x padding))
		   (incf bits padding)))
	     ;; Write out any complete chunks of 6 bits (or any remaining bits when done reading)
	     (loop while (if reading (>= bits 6) (> bits 0))
		   for index = (logand #2r111111 (ash x (- 6 bits)))
		   do (write-char (aref *base64-characters* index) stream)
		      (decf bits 6)))
    (when pad
      (loop repeat (/ padding 2) do
	(write-char *base64-padding-character* stream)))
    (get-output-stream-string stream)))

(defun hex->base64 (hex-string)
  "Encodes hexadecimal bytes from HEX-STRING as Base64"
  (bytes->base64 (hex->bytes hex-string)))

;;; Challenge 2
(defun bytes->hex (bytes)
  "Prints BYTES as a hexadecimal string"
  (let ((stream (make-string-output-stream)))
    (loop for byte across bytes do
      (format stream "~2,'0x" byte))
    (get-output-stream-string stream)))

(defun fixed-xor (a b)
  "XORs A with B"
  (map 'vector #'logxor a b))

(defun hex-xor (a b)
  "XORs the byte buffers A and B, represented in hexadecimal string format"
  (bytes->hex (fixed-xor (hex->bytes a)
			 (hex->bytes b))))

;;; Challenge 3
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

(defun bytes->string (bytes)
  "Prints BYTES as a string"
  (let ((stream (make-string-output-stream)))
    (loop for byte across bytes
	  for character = (code-char byte)
	  do (write-char character stream))
    (get-output-stream-string stream)))

(defun score-english-byte (byte)
  "Scores a possibly English byte (higher means more English-like)"
  (aref *english-character-score* byte))

(defun score-english (bytes)
  "Scores a possibly-English string encoded as BYTES (higher score means more English-like)"
  ;; Trivial heuristic: letters, numbers, and punctuation are good; others are not
  (loop for byte across bytes
	sum (score-english-byte byte)))

(defun decrypt-single-byte-xor (bytes)
  "Decrypts a single byte-XOR'd sequence of bytes using SCORE-ENGLISH"
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
	finally (return (bytes->string result))))
