;;;;Peter Santiago, Started June 16th 2008
;;;;Wilcoxon algorithm and information shamelessly stolen from
;;;;Richard Lowry:
;;;;http://faculty.vassar.edu/lowry/ch12a.html

(defun wilcoxon (x.a x.b &optional (signum 95))
  "Runs a wilcoxon signed-rank test"
  (labels ((combine (a &rest elements) `(,a ,@ elements))
           (as-ranks (l r) (mapcar #'(lambda (x) (gethash x r)) l))
           (simple-quad (c) (/ (+ -1 (sqrt (- 1 (* -1 8 c)))) 2)))
    
    (let* ((x.dif (mapcar #'- x.a x.b)) ;x.a-x.b
           (x.abs (mapcar #'abs x.dif)) ;absolute value of x.dif
           (x (sort (mapcar #'combine x.abs x.dif x.b x.a)
                    #'< :key #'first)) ;sorts by absolute value
           (ranks (as-ranks (mapcar #'first x)
                            (wilcoxon-rank (mapcar #'first x))))
                                        ;gets the ranks
           (x (mapcar #'cons ranks x)) ;adds absolute ranks to x
           (x (mapcar #'cons
                      (mapcar #'*
                              (mapcar #'first x)
                              (mapcar #'signum
                                      (mapcar #'third x))) x))
                                        ;adds signed ranks
           (n (simple-quad
               (loop for item in ranks
                     summing item))) ;number of ranks used
           (w (loop for item in (mapcar #'first x)
                    summing item)) ;total weight of all signed ranks
                          
           (z (/ (- w .5) (sqrt (/ (* n (+ n 1) (+ (* 2 n) 1)) 6)))))
                                        ;z-value
      
      (test-significance signum n w z))))

;(deftest test-wilcoxon ()
;  "runs wilcoxon on data with known results."
;  (check
;   (= 1 (wilcoxon '(72 90 40 84 22 78 50 64 50 30 52 64 45 64 24 78)
;                  '(32 58 20 68 36 68 40 56 44 25 56 68 48 62 24 78)
;                  95)
;            )))
