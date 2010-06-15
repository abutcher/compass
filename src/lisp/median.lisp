(defun median (l)
  (if (= (length l) 1)
      (first l)
      (let ((n (make-normal-from-list l)))
	(/ (normal-sum n) (normal-n n)))))

(defun condense-lists (l)
  (let (big-list)
    (dolist (v l)
      (dolist (e v)
	(push e big-list)))
    big-list))
      