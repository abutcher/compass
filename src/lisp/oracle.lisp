(defun data-oracle (data n)
  (let* ((data-copy (copy-list data))
	 (interesting (an-interesting-instance-1 data-copy 8))
	 closest-list)
    (dotimes (i  n)
      (push (closest-to interesting data-copy) closest-list)
      (setf data-copy (remove (first closest-list) (copy-list data-copy))))
    closest-list))

(defun an-interesting-instance-1 (data k)
  ;; Find the two clusters with the most similar centroid.
  (let ((clusters (meat-processor k (k-means k data)))
	closest-two
	(best-dist 9999999))
    (dolist (cluster clusters)
      (let ((other-clusters (remove cluster (copy-list clusters))))
	(dolist (other-cluster other-clusters)
	  (if (> best-dist (euclidean-distance
			    (centroid cluster) 
			    (centroid other-cluster)))
	      (progn
		(setf closest-two (list cluster other-cluster))
		(setf best-dist (euclidean-distance 
				 (centroid cluster) 
				 (centroid other-cluster))))))))
    (centroid (squash closest-two))))