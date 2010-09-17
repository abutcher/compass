(defun split (table &optional (n 0.3))
  (let* (train
         test
         (k (* n (length (table-all table)))))
    (dolist (per-instance (shuffle (table-all table)))
      (if (>= (decf k) 0)
          (push (eg-features per-instance) train)
          (push (eg-features per-instance) test)
          ))
    (values train test)))
