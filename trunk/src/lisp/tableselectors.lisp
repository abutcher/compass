(defun isa (x tbl)  (nth (table-class tbl) x))

(defun table-feature-names (tbl)
  (let (features
        (classi (table-class tbl)))
    (doitems (feature i (counted-all (table-columns tbl))
                      (reverse features))
      (unless (= i classi)
        (push (header-name feature) features)))))

(defun table-width (tbl)
  (counted-n (table-columns tbl)))

(defun table-height (tbl)
  (counted-n (table-all tbl)))

(defun table-rows (tbl)
  (counted-all (table-all tbl)))

(defun table-egs (tbl)
  (mapcar #'eg-features (table-rows tbl)))

(defun table-majority-class (tbl &optional (egs (table-egs tbl)))
  (let ((counts (make-hash-table))
        (max most-negative-fixnum)
        majority)
    (dolist (eg egs majority)
      (let* ((class (isa eg tbl))
             (new   (incf (gethash class counts 0))))
        (if (> new max)
            (setf max new
                  majority class))))))

(defun numeric-cols (tbl)
  "Gets all the numeric columns from a table."
  (let ((cols))
    (dolist (it (columns-header (table-columns tbl)))
      (if (numericp it)
          (progn
            (setf cols (append cols (list it)))
            (format t "~A is numeric~%" it))))
    cols))

(defun target-class (tbl &optional index?)
  "Gets the target class from a table."
  (let ((i 0)
        (target))
    (dolist (item (columns-header (table-columns tbl)))
      (if (eql i (table-class tbl))
	  (setf target (nth i (columns-header (table-columns tbl)))))
      (incf i))
    target))
