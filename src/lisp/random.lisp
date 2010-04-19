(defparameter *seed0* 10013)
(defparameter *seed*  *seed0*)

(defun reset-seed () (setf *seed* *seed0*))

(defun park-miller-randomizer ()
   (let ((multiplier 
	 16807.0d0);16807 is (expt 7 5)
	(modulus 
	 2147483647.0d0)) ;2147483647 is (- (expt 2 31) 1)
    (let ((temp (* multiplier *seed*)))
      (setf *seed* (mod temp modulus))
      (/ *seed* modulus))))

 (defun my-random (n)
  (let ((random-number (park-miller-randomizer)))
    (* n (- 1.0d0 random-number))))

(defun my-random-int (n)
  (let ((random-number (/ (my-random 1000.0) 1000)))
    (floor (* n random-number))))

(defun random-demo (&optional (resetp t))
  (let (counts out)
    (labels 
	((sorter (x y) (< (car x) (car y)))
         (zap    ()    (setf out nil)
                       (if resetp (reset-seed)) 
                       (setf counts (make-hash-table)))
	 (inc    (n)   (setf (gethash n counts) 
			     (1+  (gethash n counts 0)))) 
	 (cache  (k v) (push (list k v) out)))
      (zap)
      (dotimes (i 10000)              ; 10000 times do
            (inc  (my-random-int 5))) ; generate a num 0..4
      (maphash #'cache counts)        ; hash key/buckets ==> lists 
      (sort out #'sorter))))          ; sort and print  list
