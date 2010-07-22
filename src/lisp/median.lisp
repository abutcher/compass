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
      
(defun generic-median (l)
  "median calculated as cieling of ( (max - min) / 2 )"
  (let ((l (sort l #'<)))
    (ceiling (/ (- (nth (- (length l) 1) l) (nth 0 l)) 2))))
