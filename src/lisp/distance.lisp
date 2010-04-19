(defun distance (this that)
  "Distance between two discrete lists"
  (let ((d 0))
    (dolist (one this)
      (if (eql one (nth (position one this) that))
	  (incf d)))
    d))
