(defun line1 (s) 
  (format nil "~a" (read-from-string s)))

(defun longest-string (l)
  (let ((max -1) tmp)
    (dolist (x l max)
      (let ((tmp (length (format nil "~a" x))))
	(if (> tmp max)
	    (setf max tmp))))))

(defun whiteout (seq)
  (remove-if #'whitespacep  seq))

(defun whitespacep (char)
  (member char '(#\Space #\Tab #\Newline #\Page) :test #'char=))

(defun samep (form string)
  (string= (whiteout (format nil "~a" form)) (whiteout string)))

(defun lt (x y)
  (if (string-lessp (format nil "~a" x) (format nil "~a" y))
      t
      nil))

(defmacro list2string ((item list stream) &body body)
  `(with-output-to-string (,stream)
     (dolist (,item ,list) ,@body)))

(defun nchars (n &optional (char #\Space))
  (with-output-to-string (s)
    (dotimes (i n)
      (format s "~a" char ))))