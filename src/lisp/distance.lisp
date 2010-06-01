(defun discrete-distance (this that)
  "Distance between two discrete lists"
  (let ((d 0))
    (dolist (one this)
      (if (eql one (nth (position one this) that))
	  (incf d)))
    d))

(defun euclidean-distance (this that)
  "Ignores the class variable -- effort in our case."
  (let ((d 0))
    (dotimes (n (1- (length this)))
      (if (numberp (nth n this))
	  (setf d (+ d (expt (- (nth n that) (nth n this)) 2)))
	  (if (eql (nth n this) (nth n that))
	      (incf d))))
    (sqrt d)))