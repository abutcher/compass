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

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
   is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))
