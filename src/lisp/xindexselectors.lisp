(defun xindex-classi (x)
  (table-class (xindex-table x)))

(defun xindex-classes-n (x)
  (counted-n
   (aref (xindex-ranges x)
         (table-class
          (xindex-table x)))))

(defun xindex-features (x)
  (xindex-ranges x))

(defun xindex-feature (i x)
  (counted-all (aref (xindex-ranges x) i)))

(defun xindex-feature-names (x)
  (table-feature-names (xindex-table x)))
  
(defun xindex-ns (x)
  (counted-n (table-all (xindex-table x))))

(defun xindex-classes-all (x)
  (counted-all
   (aref (xindex-ranges x)
         (table-class
          (xindex-table x)))))

(defun xindex-unique-n (x feature range)
  (gethash `(,feature ,range) (xindex-uniques x) 0))

(defun xindex-width (x)
  (table-width (xindex-table x)))

(defun xindex-majority-class (x)
  (let* ((classes       (xindex-classes-all x))
         (max           -1)
         most-frequent)
    (dolist (class classes  most-frequent)
      (let* ((count  (f x class)))
        (if (> count max)
            (setf max           count
                  most-frequent class))))))

;(deftest test-xindex-majority-class ()
;  (or (fboundp 'weather) (loaddata 'weather))
;  (check
;    (= 2 (xindex-majority-class (cross-index (weather))))))

