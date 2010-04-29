(defun variance-prune (c-tree &key (aplha 0) (beta 0))
  (let ((max (max-variance c-tree)))
    (labels ((walk (c-node)
	       (unless (null (node-right c-node))
		 (if (or 
		      (< (* alpha (node-variance c-node))
			 (node-variance (node-right c-node)))
		      (< (* beta max)
			 (node-variance (node-right c-node)))
		      (< (* (my-random-int 1) max)
			 (node-variance (node-right c-node))))
		     (setf (node-right c-node) nil))
		 (if (or 
		      (< (* alpha (node-variance c-node))
			 (node-variance (node-left c-node)))
		      (< (* beta max)
			 (node-variance (node-left c-node)))
		      (< (* (my-random-int 1) max)
			 (node-variance (node-left c-node))))
		     (setf (node-left c-node) nil)))
	       (unless (null (node-right c-node))
		 (walk (node-right c-node)))
	       (unless (null (node-left c-node))
		 (walk (node-left c-node)))))
      
      (walk c-tree))
    c-tree))
		   

			    