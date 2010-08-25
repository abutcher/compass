(defun intra-cluster-distance (cluster &key (distance-func 'cosine-similarity))
  "Used for within' cluster comparison."
  (let* ((sumSq 0)
	 (center (centroid cluster))
	 (cluster (remove center (copy-list cluster))))
    (dolist (instance cluster)
      (setf sumSq (+ sumSq (square (funcall distance-func instance center)))))
    sumSq))

(defun intra-cluster-measure (clusters &key (distance-func 'cosine-similarity))
  (let ((s 0))
    (dolist (cluster clusters)
      (setf s (+ s (intra-cluster-distance cluster :distance-func distance-func))))
    (* (/ 1 (length (squash clusters))) s)))

(defun inter-cluster-measure (clusters &key (distance-func 'cosine-similarity))
  "We want to maximize the smallest distance between clusters to prove validity."
  (let (distances)
    (dolist (cluster clusters)
      (dolist (other-cluster (remove cluster (copy-list clusters)))
	(push (square (funcall distance-func
			       (centroid cluster :distance-func distance-func)
			       (centroid other-cluster :distance-func distance-func)) )
	      distances)))
    (normal-min (make-normal-from-list distances))))

(defun validity (clusters &key (distance-func 'cosine-similarity))
  "Clusters with the smallest validity score are ideal."
  (/ (intra-cluster-measure clusters :distance-func distance-func)
     (inter-cluster-measure clusters :distance-func distance-func)))

(defun centroid (cluster &key (distance-func 'cosine-similarity))
  (let ((best-dist 99999999)
	centroid)
    (dolist (instance cluster)
      (let ((this-dist 0))
	(dolist (other-instance (remove instance (copy-list cluster)))
	  (setf this-dist (+ this-dist (funcall distance-func instance other-instance))))
	(if (> best-dist this-dist)
	    (progn
	      (setf best-dist this-dist)
	      (setf centroid instance)))))
    centroid))

(defun closest-between-clusters (first-cluster second-cluster 
				 &key (distance-func 'cosine-similarity))
  "Find me the two closest instances between clusters."
  (let ((best-dist 999999999)
	closest-pair)
    (dotimes (i (length first-cluster))
      (dotimes (j (length second-cluster))
	(if (> best-dist (funcall distance-func 
				  (nth i first-cluster)
				  (nth j second-cluster)))
	    (progn 
	      (setf best-dist (funcall distance-func
				       (nth i first-cluster)
				       (nth j second-cluster)))
	      (setf closest-pair (list (nth i first-cluster)
				       (nth j second-cluster)))))))
    closest-pair))
