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
