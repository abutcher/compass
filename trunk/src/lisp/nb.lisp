(defun naiveBayes  (n tbl &key (verbose t))
  (labels ((trainer (tbl cautions egs)
             (cross-index tbl))
           (tester (tbl cautions xindex one)
             (cons (isa one tbl)
                   (bayes-classify one xindex))))
   (n-way n tbl
          :verbose verbose
          :trainer  #'trainer
          :tester   #'tester)))
          
(defun bayes-classify (one x &optional (m 2) (k 1))
  (let* ((classes        (xindex-classes-all x))
         (nclasses       (xindex-classes-n   x))
         (n              (xindex-ns x))
         (classi         (xindex-classi x))
         (like           most-negative-fixnum)
         (classification (first classes)))
    (dolist (class classes)
      (let* ((prior (/ (+ (f x class) k)
                       (+  n (* k nclasses))))
             (tmp   (log prior)))
        (doitems (feature i one)
          (unless (= classi i)
            (unless (unknownp feature)
              (let ((delta (/ (+ (f x class i feature)
                                 (* m prior))
                              (+ (f x class) m))))
                (incf tmp (log delta))))))
        (when (> tmp like)
          (setf like tmp
                classification class))))
    classification))

;(deftest test-nb ()
;  (let ((tmp (naiveBayes 1 (weather2) :verbose nil)))
;    (check
;      (samep tmp "
;    (#(ABCD :FOR NO
;       :A 9 :B 2 :C 0 :D 3
;       :ACC .86 :PD .60 :PF .00 :PREC 1.00
;       :F .00 :BAL .72)
;    #(ABCD :FOR YES
;       :A 3 :B 0 :C 2 :D 9
;       :ACC .86 :PD 1.00 :PF .40 :PREC .82
;       :F .57 :BAL .72))"))))
