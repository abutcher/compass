(defun split (&optional (n 0.3) &rest func)
  ;Splits any number of data sets between a percentage of train and test set
  (let (returnTrain
        returnTest
        columns)
  (dolist (per-func func (values (make-train-data columns returnTrain) (make-test-data columns returnTest)))
;(format t "~A~%" (table-name (funcall per-func)))
    (multiple-value-bind (train test) (if (tablep per-func)
                                          (seg-dataset per-func n)
                                          (funcall per-func n))
      (let ((trainSet (egs train))
            (testSet (egs test)))
      (dolist (per-train trainSet)
        (push (eg-features per-train) returnTrain))
      (dolist (per-test testSet)
        (push (eg-features per-test) returnTest))
    (setf columns (columns-header (table-columns train))))))))

(defun seg-dataset (table &optional (n 0.3))
  (let* (train
         test
         (k (* n (length (table-all table)))))
    (dolist (per-instance (shuffle (table-all table)))
      (if (>= (decf k) 0)
          (push (eg-features per-instance) train)
          (push (eg-features per-instance) test)
          ))
    (values (make-train-data (columns-header (table-columns table)) train) (make-test-data (columns-header (table-columns table)) test))))

(deftest test-nvalues()
  (multiple-value-bind (train test) (nvalues 0.3 (ar3))
    (check
     (equal
      (floor (* 0.3 (length (table-all (ar3)))))
      (length (table-all train))))))

(defun make-train-data (cols eg)
  (data
   :name 'train-data
   :columns cols
   :egs eg))

(defun make-test-data (cols eg)
  (data
   :name 'test-data
   :columns cols
   :egs eg))
