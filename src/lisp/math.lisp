(defun avg (these)
  (let ((n (make-normal-from-list these)))
    (/ (normal-sum n) (normal-n n))))