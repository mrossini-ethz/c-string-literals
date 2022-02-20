(in-package :c-string-literals/test)

;; Enable C string literals
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-c-string-literals))

;; Helper function
(defun cat (&rest items)
  (apply #'concatenate 'string (mapcar (lambda (item) (if (characterp item) (list item) item)) items)))

(test basic-tests
      ;; Valid strings
      (is (string= "" #""))
      (is (string= "a" #"a"))
      (is (string= "abcd" #"abcd"))
      ;; Incomplete string
      (signals error (read-from-string "#\"a")))

(test (escape-character-tests :depends-on basic-tests)
      ;; ASCII values
      (is (string= (cat (code-char 7)) #"\a"))
      (is (string= (cat (code-char 8)) #"\b"))
      (is (string= (cat (code-char 12)) #"\f"))
      (is (string= (cat (code-char 10)) #"\n"))
      (is (string= (cat (code-char 13)) #"\r"))
      (is (string= (cat (code-char 9)) #"\t"))
      (is (string= (cat (code-char 11)) #"\v"))
      (is (string= (cat (code-char 92)) #"\\"))
      (is (string= (cat (code-char 0)) #"\0"))
      ;; Incomplete escape sequence
      (signals error (read-from-string "#\"\\\"")))

(test (octal-escape-sequence-tests :depends-on basic-tests)
      ;; Valid codes
      (is (string= (cat (code-char 0)) #"\000"))
      (is (string= (cat (code-char 83)) #"\123"))
      (is (string= (cat (code-char 255)) #"\377"))
      ;; Invalid codes
      (signals simple-error (read-from-string "#\"\\400\""))
      (signals simple-error (read-from-string "#\"\\108\""))
      (signals simple-error (read-from-string "#\"\\109\""))
      ;; Incomplete sequences
      (signals simple-error (read-from-string "#\"\\11\""))
      (signals simple-error (read-from-string "#\"\\1\"")))

(test (hex-escape-sequence-tests :depends-on basic-tests)
      ;; Valid codes
      (is (string= (cat (code-char 0)) #"\x00"))
      (is (string= "A" #"\x41"))
      (is (string= "+" #"\x2b"))
      (is (string= "+" #"\x2B"))
      (is (string= (cat (code-char 255)) #"\xff"))
      (is (string= (cat (code-char 255)) #"\xFf"))
      (is (string= (cat (code-char 255)) #"\xfF"))
      (is (string= (cat (code-char 255)) #"\xFF"))
      ;; Invalid codes
      (signals simple-error (read-from-string "#\"\\xfg\""))
      (signals simple-error (read-from-string "#\"\\xgf\""))
      ;; Incomplete sequences
      (signals simple-error (read-from-string "#\"\\x1\"")))

(test combined-tests :depends-on '(escape-character-tests octal-escape-sequence-tests hex-escape-sequence-tests)
      (is (string= (cat "abcd" #\Newline "efgh" #\Return "ijkl" #\Tab "mnop+qrst" (code-char 0) "uvwx") #"abcd\nefgh\rijkl\011mnop\x2Bqrst\0uvwx"))
      (is (string= (cat #\Newline #\Newline #\Newline #\Newline #\Newline #\Newline) #"\n\n\n\n\n\n"))
      (is (string= (cat #\Return #\Newline #\Return #\Newline #\Return #\Newline) #"\r\n\r\n\r\n")))

(defun c-string-literals-test ()
  (run!))
