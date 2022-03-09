(in-package :c-string-literals)

(defmacro char-case (character &body cases)
  (let ((ch (gensym)))
    `(let ((,ch ,character))
       (cond
         ,@(loop for c in cases collect `((char-equal ,(first c) ,ch) ,@(rest c)))))))

(defun oct-p (c)
  (or (char= c #\0)
      (char= c #\1)
      (char= c #\2)
      (char= c #\3)
      (char= c #\4)
      (char= c #\5)
      (char= c #\6)
      (char= c #\7)))

(defun hex-p (c)
  (or (oct-p c)
      (char= c #\8)
      (char= c #\9)
      (char-equal c #\a)
      (char-equal c #\b)
      (char-equal c #\c)
      (char-equal c #\d)
      (char-equal c #\e)
      (char-equal c #\f)))

(defun oct-digit (c)
  (char-case c
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)))

(defun hex-digit (c)
  (char-case c
    (#\0 0)
    (#\1 1)
    (#\2 2)
    (#\3 3)
    (#\4 4)
    (#\5 5)
    (#\6 6)
    (#\7 7)
    (#\8 8)
    (#\9 9)
    (#\a 10)
    (#\b 11)
    (#\c 12)
    (#\d 13)
    (#\e 14)
    (#\f 15)))

(defun oct-to-int (c1 c2 c3)
  (+ (ash (oct-digit c1) 6) (ash (oct-digit c2) 3) (oct-digit c3)))

(defun hex-to-int (c1 c2)
  (+ (ash (hex-digit c1) 4) (hex-digit c2)))

(defun read-string (stream delim)
  (let (backslash lst oct hex)
    (loop named read-loop for c = (read-char stream t nil nil)
          do
             (cond
               ;; Octal code
               (oct
                (cond
                  ((and (= (length oct) 2) (oct-p c)) (push (code-char (oct-to-int (cadr oct) (car oct) c)) lst) (setf oct nil))
                  ((and (< (length oct) 2) (oct-p c)) (push c oct))
                  ((and (= (length oct) 1) (char= (car oct) #\0)) (push (code-char 0) lst) (unread-char c stream) (setf oct nil))
                  (t (error "Illegal octal code in c-style string escape sequence!"))))
               ;; Hex code
               (hex
                (cond
                  ((and (= (length hex) 2) (hex-p c)) (push (code-char (hex-to-int (car hex) c)) lst) (setf hex nil))
                  ((and (< (length hex) 2) (hex-p c)) (push c hex))
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
                  ((oct-p c) (when (> (oct-digit c) 3) (error "Illegal octal code in c-style string escape sequence!")) (push c oct))
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
