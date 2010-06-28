"""
Plan for experiment:

1. Sample randomly from the space and score.

2. Cluster inputs with compass
 
3. Intelligently sample from compass clusters.

"""

(defun random-vs-compass (ddp-model k)
  (let* ((mitigations (generator :n 100 :s (length (ddp-model-m-cost ddp-model))))
	 (samples (subseq (shuffle mitigations) 0 k))
	 costs)
    (dolist (sample samples)
      (push (second (model ddp-model sample)) costs))
    (avg costs)))

