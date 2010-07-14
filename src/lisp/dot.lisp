(defun dot-compass (c-tree file)
  (with-open-file (stream file
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    (format stream "DIGRAPH G {~%")
    (labels ((walk (c-node &optional (level 0))
	       (unless (null (node-right c-node))
		 (format stream "\"~A\" -> \"~A\" L=~A;~%"
			 (format nil "Size: ~A\\nVariance: ~5,2f\\nMedian: ~5,2f"
				 (length (node-contents c-node))
				 (node-variance c-node)
				 (median 
				  (mapcar #'first
					  (mapcar #'last 
						  (node-contents c-node)))))
			 (format nil "Size: ~A\\nVariance: ~5,2f\\nMedian: ~5,2f"
				 (length (node-contents (node-right c-node)))
				 (node-variance (node-right c-node))
				 (median 
				  (mapcar #'first
					  (mapcar #'last
						  (node-contents (node-right c-node))))))
			 level))
	       (unless (null (node-left c-node))
		 (format stream "\"~A\" -> \"~A\" L=~A;~%"
			 (format nil "Size: ~A\\nVariance: ~5,2f\\nMedian: ~5,2f"
				 (length (node-contents c-node))
				 (node-variance c-node)
				 (median 
				  (mapcar #'first
					  (mapcar #'last 
						  (node-contents c-node)))))
			 (format nil "Size: ~A\\nVariance: ~5,2f\\nMedian: ~5,2f"
				 (length (node-contents (node-left c-node)))
				 (node-variance (node-left c-node))
				 (median 
				  (mapcar #'first
					  (mapcar #'last
						  (node-contents (node-left c-node))))))
			 level))
	       (unless (null (node-right c-node))	     
		 (walk (node-right c-node) (1+ level)))
	       (unless (null (node-left c-node))
		 (walk (node-left c-node) (1+ level)))))
      (walk c-tree))
    (format stream "}~%")))
	     