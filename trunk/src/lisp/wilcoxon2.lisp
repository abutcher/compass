(defun test-significance (signum total.ranks weight z)
  "Tests for significance: 0 if not, 1 if"
  (let ((val 0))
    (cond ((< total.ranks 5))          
          ((< total.ranks 10)
           (< weight (get-wcrit signum total.ranks))
           (if (> weight (get-wcrit signum total.ranks))
               (setf val 1)))          
          ((< 10 total.ranks)
           (< z (get-zcrit signum))
           (if (> z (get-zcrit signum))
               (setf val 1))))
    val))
(defun get-zcrit (signum)
  "for tests with greater than 10 ranks, use this table"
  (let ((table '((90   1.645)
                 (95   1.960)
                 (98   2.326)
                 (99   2.576)
                 (99.9 3.291))))
    (dotimes (i (list-length table))
      (if (= (first (nth i table)) signum)
          (return (second (nth i table)))))))
(defun get-wcrit (signum total.ranks)
  "for tests run with 5-9 ranks, you must use this table"
  (let* ((inf most-positive-fixnum)
         (table `((90   (5 15)    (6 17)    (7 22)   (8 26)  (9 29))
                  (95   (5 ,inf)  (6 21)    (7 24)   (8 30)  (9 35))
                  (98   (5 ,inf)  (6 ,inf)  (7 28)   (8 34)  (9 39))
                  (99   (5 ,inf)  (6 ,inf)  (7 ,inf) (8 36)  (9 43))
                  (99.9 (5 ,inf)  (6 ,inf)  (7 ,inf) (8 ,inf)(9 ,inf)))))
    (dotimes (i (list-length table))
      (if (= (first (nth i table)) signum)
          (dotimes (j 5)
            (if (= total.ranks (first (nth (+ j 1) (nth i table))))
                (return-from get-wcrit
                  (second (nth (+ j 1) (nth i table))))))))))

(defun wilcoxon-rank (l &optional (ranks (make-hash-table)) (n 0))
  "Returns a hash of the ranks in a sorted list. All numbers in a
   run of repeated entries get the average rank of that run."
  (if (null l)
      ranks
      (let (repeats sum now)
	(labels ((walk () (incf n) (pop l))
		 (new  () (setf repeats 1) (setf sum n))
		 (same () (incf sum n) (incf repeats))
		 (spin () (when (eql now (car l))
			    (walk) (same) (spin))))
	  (setf now (walk))
	  (new)
	  (spin)
          (cond ((= now 0)
                 (setf repeats 1 sum 0 now 0 n 0)))
	  (setf (gethash now ranks) (/ sum repeats))
	  (wilcoxon-rank l ranks n)))))
