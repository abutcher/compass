(defstruct normal 
  (max (* -1 most-positive-single-float))
  (min       most-positive-single-float)
  (n 0)
  (sum 0)
  (sumSq 0))

(defmethod add ((n normal) x)
  (incf (normal-n     n) 1)
  (incf (normal-sum   n) x)
  (incf (normal-sumSq n) (square x))
  (setf (normal-max   n) (max (normal-max n) x))
  (setf (normal-min   n) (min (normal-min n) x))
  x)

(defmethod mean ((n normal))
  (/  (normal-sum n) (normal-n n)))

(defmethod stdev ((n normal))
  (let ((sum   (normal-sum n)) 
        (sumSq (normal-sumSq n))
        (n     (normal-n n)))
    (sqrt (/ (- sumSq(/ (square sum) n)) (- n 1)))))

(defmethod pdf ((n normal) x)
  (let ((mu     (mean n))
        (sigma  (stdev n)))
    (* (/ (* (sqrt (* 2 pi)) sigma))
       (exp (* (- (/ (* 2 (square sigma)))) (square (- x mu)))))))

(defun square (x)
  (expt x 2))

(defun make-normal-from-list (l)
  (let ((n (make-normal)))
    (dolist (x l)
      (add n x))
    n))