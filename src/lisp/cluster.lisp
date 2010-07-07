(defun intra-cluster-distance (cluster &key (distance-func 'cosine-similarity))
  "Used for within' cluster comparison."
  (let* ((sumSq 0)
	 (centroid (split-the-tree cluster))
	 (cluster (remove centroid (copy-list cluster))))
    (dolist (instance cluster)
      (setf sumSq (+ sumSq (square (funcall distance-func instance centroid)))))
    sumSq))

(defun intra-cluster-measure (clusters &key (distance-func 'cosine-similarity))
  (let ((s 0))
    (dolist (cluster clusters)
      (setf s (+ s (intra-cluster-distance cluster :distance-func distance-func))))
    (* (/ 1 (length (squash clusters))) s)))

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

(defun inter-cluster-measure (clusters &key (distance-func 'cosine-similarity))
  "We want to maximize the smallest distance between clusters to prove validity."
  (let (distances)
    (dolist (cluster clusters)
      (dolist (other-cluster (remove cluster (copy-list clusters)))
	(push (square (funcall distance-func
			       (split-the-tree cluster)
			       (split-the-tree other-cluster))) distances)))
    (normal-min (make-normal-from-list distances))))

(defun validity (clusters &key (distance-func 'cosine-similarity))
  "Clusters with the smallest validity score are ideal."
  (/ (intra-cluster-measure clusters :distance-func distance-func)
     (inter-cluster-measure clusters :distance-func distance-func)))
