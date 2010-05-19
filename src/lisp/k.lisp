(defun k-predict (data k)
  "Use the k nearest neighbors to predict"
  (let* ((representative (random-element data))
	 (data (remove representative data))
	 closest-list)
    (dotimes (n k)
      (let ((closest (closest-to representative data)))
	(push closest closest-list)
	(setf data (remove closest data))))
    (let ((predicted (median (mapcar #'first (mapcar #'last closest-list)))))
      (mre (first (last representative)) predicted))))

(defun best-k-predict (data)
  "Loop until we have a best-k mre result"
  (let* ((representative (random-element data))
	 (data (remove representative data))
	 closest-list
	 (best-so-far 9999))
    (loop while (not (null data)) do
	 (let ((closest (closest-to representative data)))
	   (push closest closest-list)
	   (let ((current-mre (mre (first (last representative))
				   (median 
				    (mapcar #'first 
					    (mapcar #'last closest-list))))))
	     (setf data (remove closest data))	     
	     (if (< current-mre
		    best-so-far)
		 (setf best-so-far current-mre)))))
    best-so-far))
  