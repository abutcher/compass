(defun n-way (n tbl &key trainer tester
              (reporter  #'default-reporter)
              (finale    #'default-finale)
              (verbose t))
  (let ((folds (folds (table-egs tbl) n :pred 'lt :key
                      #'(lambda (x) (isa x tbl))))
        reports
        cautions (make-caution))
    (dolist (fold folds)
      (let* ((training (counted-all (fold-train fold)))
             (tests    (counted-all (fold-test fold)))
             (tbl1     (table-copy tbl  training))
             (model    (funcall trainer tbl1 cautions training)))  
       (dolist (test tests)
         (let* ((tested   (funcall tester   tbl1 cautions model test))
                (reported (funcall reporter tbl1 cautions tested)))
           (push reported reports)))))
    (funcall finale tbl cautions reports verbose)))

(defun default-reporter (tbl cautions test-result)
  (declare (ignore tbl cautions))
  test-result)

(defun default-finale (tbl cautions reports verbose)
  (declare (ignore tbl cautions))
  (abcd-stats reports :verbose verbose)) 
