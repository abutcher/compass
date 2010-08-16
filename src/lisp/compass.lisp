(defstruct node
  contents
  variance
  right
  left
  )

(defun compass (mitigations &key (distance-func 'cosine-similarity) (variance-func 'variance))
  (let ((tree (make-node
	       :variance (funcall variance-func mitigations)
	       :contents mitigations))
	; Square root min cluster sizeor
	(min-cluster-size (round (sqrt (length mitigations)))))

    ; Recursive node-walker
    (labels ((walk (node)
	       (let ((node-split (separate (node-contents node))))
		 (if (> (length (first node-split)) 1)
		   (setf (node-left node)
			 (make-node
			  :variance (funcall variance-func (first node-split))
			  :contents (first node-split))))
		 (if (> (length (second node-split)) 1)
		   (setf (node-right node)
			 (make-node
			  :variance (funcall variance-func (second node-split))
			  :contents (second node-split))))
		 (unless (null (node-left node))
		   (if (> (length (node-contents (node-left node))) min-cluster-size)
		       (walk (node-left node))))
		 (unless (null (node-right node))
		   (if (> (length (node-contents (node-right node))) min-cluster-size)
		       (walk (node-right node)))))))
      
      ; Recursively build upon each node
      (walk tree)
      tree)))

(defun print-nodes (tree &optional (stream *standard-output*))
  "Pretty print built nodes"
  (labels ((walk (node &optional (level 0))
	     (format stream "LEVEL: ~A~%" level)
	     (format stream "VARIANEC: ~A~%" (node-variance node))
	     (format stream "CONTENTS:~%")
	     (dolist (element (node-contents node))
	       (format stream "~A~%" element))
	     (format stream "~%~%")
	     (if (not (null (node-right node)))
		 (walk (node-right node) (1+ level)))
	     (if (not (null (node-left node)))
		 (walk (node-left node) (1+ level)))))
    (walk tree)))

(defun test-compass (file mitigations)
  "Sample run"
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create )
    (let* ((compass-tree (compass mitigations :distance-func 'cosine-similarity)))
      (format stream "TEST SIZE: ~A~%~%" (length mitigations))
      (print-nodes compass-tree stream))))

(defun compass-teak (this projects alpha beta &key (distance-func 'cosine-similarity) (variance-func 'variance))
  (let* ((test this)
	 (projects (remove test projects))
	 (compass-tree (compass projects 
				:distance-func distance-func 
				:variance-func variance-func))
	 (pruned-tree (variance-prune compass-tree :alpha alpha :beta beta))
	 (actual (first (last test)))
	 (predicted 0))
    
    (labels ((walk (c-node)
	       (if (< (node-variance c-node)
		      (weighted-variance c-node))
		   (setf predicted (median (mapcar #'first (mapcar #'last (node-contents c-node)))))
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
      (walk pruned-tree))
    (mre actual predicted)))

(defun tree-leaves (c-tree)
  (let (leaves)
    (labels ((walk (c-node)
	       (if (and (null (node-right c-node))
			(null (node-left c-node)))
		     (push (node-contents c-node) leaves))
	       (unless (null (node-right c-node))
		 (walk (node-right c-node)))
	       (unless (null (node-left c-node))
		 (walk (node-left c-node)))))
      (walk c-tree))
    leaves))

(defun max-leaf-size (c-node)
  (let ((maxs 0))
    (labels ((walk (c-node)
	       (if (and (null (node-right c-node))
			(null (node-left c-node)))
		   (if (> (length (node-contents c-node))
			  maxs)
		       (setf maxs (length (node-contents c-node))))
		   (progn
		     (unless (null (node-right c-node))
		       (walk (node-right c-node)))
		     (unless (null (node-left c-node))
		       (walk (node-left c-node)))))))
      (walk c-node))
    maxs))

(defun max-variance (c-tree)
  (let ((max 0))
    (labels ((walk (c-node)
	       (if (< max (realpart (node-variance c-node)))
		   (setf max (realpart (node-variance c-node))))
	       (unless (null (node-right c-node))
		 (walk (node-right c-node)))
	       (unless (null (node-left c-node))
		 (walk (node-left c-node)))))
      (walk c-tree))
    max))

(defun max-leaf-variance (c-tree)
  (let ((max 0))
    (labels ((walk (c-node)
	       (if (and 
		    (and (null (node-right c-node))
			 (null (node-left c-node)))
		    (< max (realpart (node-variance c-node))))
		   (setf max (realpart (node-variance c-node))))
	       (unless (null (node-right c-node))
		 (walk (node-right c-node)))
	       (unless (null (node-left c-node))
		 (walk (node-left c-node)))))
      (walk c-tree))
    max))
