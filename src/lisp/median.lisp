(defun median (l)
  (let ((n (make-normal-from-list l)))
    (/ (normal-sum n) (normal-n n))))