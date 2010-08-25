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

(defun farthest-from (this those &key (distance-func 'cosine-similarity))
  "Give me the farthest thing from this in those"
  (let ((max-distance 0) temporary)
    (dolist (that those)
      (let ((d (funcall distance-func this that)))
	(if (> d max-distance)
	    (setf temporary that))))
    temporary))

(defun closest-to (this those &key (distance-func 'cosine-similarity))
  "Give me the farthest thing from this in those"
  (let ((min-distance 999999) temporary)
    (dolist (that those)
      (let ((d (funcall distance-func this that)))
	(if (< d min-distance)
	    (setf temporary that))))
    temporary))
