(defun score (mitigations this-model)
  (dolist (mitigation mitigations)
    (model (copy-ddp-model this-model) mitigation)))

"
N   	   Size   t
100 	   31	   0.007
250 	   31	   0.014
500 	   31	   0.026
1000	   31	   0.057
5000	   31	   0.362
10000	   31	   0.500
"