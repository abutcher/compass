(defun mre (actual predicted)
  (if (= actual predicted)
      0
      (/ (abs (- actual predicted))
	 actual)))