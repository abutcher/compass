(defun distance (this that)
  "Distance between two discrete lists"
  (let ((d 0))
    (dolist (one this)
      (if (eql one (nth (position one this) that))
	  (incf d)))
    d))

(defun ndistance (this that)
  (let ((d 0))
    (dolist (one this)
      (dolist (two that)
	(if (and (numberp one) (numberp two))
	    (setf d (+ d (expt (- two one) 2)))
	    (if (eql one two)
		(incf d)))))
    (sqrt d)))

; Distance functions for Dan Brook's kmeans implementation

(defun num-dist (x y range)
  (if (and (numberp x) (numberp y) (numberp range))
      (progn 
	(if (= range 0) (setf range 1))
	(/ (abs (- x y)) range))
      range))

(defun xy-dist (x y)
  (sqrt (+
	 (expt (abs (- (cdr x) (car y))) 2)
	 (expt (abs (- (cadr x) (cadr y))) 2) )))

(defun discrete-dist (x y)
  (if (or (unknownp x) (unknownp y))
      1
      (if (and (numberp x) (numberp y))
	  (if (= x y) 0 1)
	  (if (string= (symbol-name x) (symbol-name y)) 0 1))))

(defun ?-dist () 0)

(defun row-dist (a b col-types col-ranges)
  (let (	(temp-dist nil)
	(temp-mes nil))
    (setf temp-dist nil)
    (dotimes (n (length col-types))
      (setf temp-mes (case (nth n col-types)
		       (#\$ (num-dist 
			     (nth n a)
			     (nth n b)
			     (abs (- (cadr (nth n col-ranges)) (car (nth n col-ranges))))))
		       (#\? (?-dist))
		       (#\d (discrete-dist 
			     (nth n a)
			     (nth n b))) ))
      (push (expt temp-mes 2) temp-dist))
    (sqrt (apply #'+ temp-dist)) ))
