(in-package :c-string-literals)

(defmacro char-case (character &body cases)
  (let ((ch (gensym)))
    `(let ((,ch ,character))
       (cond
         ,@(loop for c in cases collect `((char-equal ,(first c) ,ch) ,@(rest c)))))))

(defun ascii-implementation-p ()
  (and (= (char-code #\A) 65)
       (= (char-code #\a) 97)
       (= (char-code #\0) 48)
       (= (char-code #\Newline) 10)
       (= (char-code #\Space) 32)
       (= (char-code #\.) 46)
       (= (char-code #\~) 126)))

(defun oct-to-int (c1 c2 c3)
  (+ (ash (digit-char-p c1 8) 6) (ash (digit-char-p c2 8) 3) (digit-char-p c3)))

(defun hex-to-int (c1 c2)
  (+ (ash (digit-char-p c1 16) 4) (digit-char-p c2 16)))

(defun read-string (stream delim)
  (let (backslash lst oct hex)
    (loop named read-loop for c = (read-char stream t nil nil)
          do
             (cond
               ;; Octal code
               (oct
                (cond
                  ((and (= (length oct) 2) (digit-char-p c 8)) (push (code-char (oct-to-int (cadr oct) (car oct) c)) lst) (setf oct nil))
                  ((and (< (length oct) 2) (digit-char-p c 8)) (push c oct))
                  ((and (= (length oct) 1) (char= (car oct) #\0)) (push (code-char 0) lst) (unread-char c stream) (setf oct nil))
                  (t (error "Illegal octal code in c-style string escape sequence!"))))
               ;; Hex code
               (hex
                (cond
                  ((and (= (length hex) 2) (digit-char-p c 16)) (push (code-char (hex-to-int (car hex) c)) lst) (setf hex nil))
                  ((and (< (length hex) 2) (digit-char-p c 16)) (push c hex))
                  (t (error "Illegal hex code in c-style string escape sequence!"))))
               ;; Escaped characters
               (backslash
                (setf backslash nil)
                (cond
                  ((char= c delim) (push c lst))
                  ((char= c #\\) (push c lst))
                  ((char= c #\a) (push (code-char 7) lst))
                  ((char= c #\b) (push (code-char 8) lst))
                  ((char= c #\f) (push (code-char 12) lst))
                  ((char= c #\n) (push #\Newline lst))
                  ((char= c #\r) (push #\Return lst))
                  ((char= c #\t) (push #\Tab lst))
                  ((char= c #\v) (push (code-char 11) lst))
                  ((digit-char-p c 8) (when (> (digit-char-p c 8) 3) (error "Illegal octal code in c-style string escape sequence!")) (push c oct))
                  ((char= c #\x) (push c hex))
                  (t (push c lst))))
               ;; Non-escaped characters
               (t
                (cond
                  ((char= c delim) (return-from read-loop))
                  ((char= c #\\) (setf backslash t))
                  (t (push c lst))))))
    (concatenate 'string (reverse lst))))

(defun read-string-dispatch (stream c n)
  (declare (ignore n))
  (read-string stream c))

(defparameter *backup-dispatch-string-reader* (get-dispatch-macro-character #\# #\" nil))
(defparameter *backup-string-reader* (get-macro-character #\" nil))

(defun restore-string-literals (&optional (dispatch t))
  (if dispatch
      (set-dispatch-macro-character #\# #\" *backup-dispatch-string-reader*)
      (set-macro-character #\" *backup-string-reader*)))

(defun enable-c-string-literals (&optional (dispatch t))
  (if dispatch
    (progn
     (setf *backup-dispatch-string-reader* (get-dispatch-macro-character #\# #\"))
     (set-dispatch-macro-character #\# #\" #'read-string-dispatch))
    (progn
     (setf *backup-string-reader* (get-macro-character #\"))
     (set-macro-character #\" #'read-string))))
