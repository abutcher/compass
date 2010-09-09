(defun mre (actual predicted)
  (/ (abs (- predicted actual)) actual))