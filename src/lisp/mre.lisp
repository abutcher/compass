(defun mre (actual predicted)
  (if (= actual predicted)
      0
      (if (= actual 0)
	  (setf actual .00000000001)
	  (/ (abs (- actual predicted))
	     actual))))