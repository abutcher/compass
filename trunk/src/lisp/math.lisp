(defun avg (these)
  (let ((n (make-normal-from-list these)))
    (/ (normal-sum n) (normal-n n))))

(defun sum (these)
  (let ((n (make-normal-from-list these)))
    (normal-sum n)))

(defun find-min-max (numbers)
  "(find-min-max '(1 0 10 4)) returns (0 10)"
  (let ((min most-positive-single-float) (max (* -1 most-positive-single-float)))
    (dolist (i numbers (list min max))
      (if (> i max) (setf max i))
      (if (< i min) (setf min i)))))
