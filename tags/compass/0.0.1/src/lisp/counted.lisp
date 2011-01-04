(defstruct counted all (n 0) sorted)

(defun counted+ (one c)
  (incf (counted-n c))
  (push one (counted-all c))
  (setf (counted-sorted c) nil)
  c)

(defun counted-sort (c pred key)
  (unless (counted-sorted c)
    (setf (counted-all c) (sort (counted-all c) pred :key key))
    (setf (counted-sorted c) t))
  c)