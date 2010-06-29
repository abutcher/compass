"""
Plan for experiment:

1. Sample randomly from the space and score.

2. Cluster inputs with compass
 
3. Intelligently sample from compass clusters.

"""

(defun random-sampling-score (ddp-model k)
  (let* ((mitigations (generator :n 100 :s (length (ddp-model-m-cost ddp-model))))
	 (samples (subseq (shuffle mitigations) 0 k))
	 costs)
    (dolist (sample samples)
      (push (second (model ddp-model sample)) costs))
    (avg costs)))

(defun intelligent-sampling-score (ddp-model k &key (distance-func 'cosine-similarity))
  (let ((ctree (compass (generator :n 100 :s (length (ddp-model-m-cost ddp-model)))))
	samples
	costs)
    (labels ((walk (c-node)
	       (if (< (node-variance c-node)
		      (weighted-variance c-node))
		   (let ((center (split-the-tree (node-contents c-node))))
		     (setf (node-contents c-node) (remove center (node-contents c-node)))
		     center)
		   (if (or (null (node-right c-node))
			   (null (node-left c-node)))
		       (progn
			 (unless (null (node-right c-node))
			   (walk (node-right c-node)))
			 (unless (null (node-left c-node))
			   (walk (node-left c-node))))
		       (if (> (weighted-variance (node-right c-node))
			      (weighted-variance (node-left c-node)))
			   (walk (node-left c-node))
			   (walk (node-right c-node)))))))
      (dotimes (n k)
	(push (walk ctree) samples)))
    (dolist (sample samples)
      (push (second (model ddp-model sample)) costs))
    (format t "There are ~A samples.~%" (length samples))
    (avg costs)))