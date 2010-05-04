(defstruct node
  rootp
  contents
  variance
  right
  left
  )

(defun compass (mitigations &key (min-cluster-size 4) (distance-func 'distance) (variance-func 'variance))
  (let ((tree (make-node
	       :rootp t
	       :variance (funcall variance-func mitigations)
	       :contents mitigations)))

    ; Recursive node-walker
    (labels ((walk (node)
	       (let ((node-split (separate (node-contents node))))
		 (if (>= (length (cdr (first node-split))) min-cluster-size)
		     (setf (node-left node)
			   (make-node
			    :variance (funcall variance-func (first node-split))
			    :contents (first node-split))))
		 (if (>= (length (cdr (second node-split))) min-cluster-size)
		     (setf (node-right node)
			   (make-node
			    :variance (funcall variance-func (second node-split))
			    :contents (second node-split))))
		 (if (not (null (node-left node)))
		     (walk (node-left node)))
		 (if (not (null (node-right node)))
		     (walk (node-right node))))))
    
      ; Recursively build upon each node
      (walk tree)
      tree)))

(defun print-nodes (tree &optional (stream *standard-output*))
  "Pretty print built nodes"
  (labels ((walk (node &optional (level 0))
	     (format stream "LEVEL: ~A~%" level)
	     (if (node-rootp node)
		 (format stream "LOOKING AT ROOT NODE~%"))
	     (format stream "ENTROPY: ~A~%" (entropy (node-contents node)))
	     (format stream "CONTENTS:~%")
	     (dolist (element (node-contents node))
	       (format stream "~A~%" element))
	     (format stream "~%~%")
	     (if (not (null (node-right node)))
		 (walk (node-right node) (1+ level)))
	     (if (not (null (node-left node)))
		 (walk (node-left node) (1+ level)))))
    (walk tree)))

(defun test-compass (file n s)
  "Sample run"
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create )
    (let* ((mitigations (generator :n n :s s))
	   (tree (compass mitigations)))
      (format stream "TEST SIZE: ~A~%~%" (length mitigations))
      (print-nodes tree stream))))

(defun separate (these &key (distance-func 'distance))
  "Turn one list into two lists using euclidean distance and farthest
   neighbors"
  (let (this that this-group that-group)
    ; Pick one at random, this
    (setf this (random-element these))
    ; Remove this from these
    (setf these (remove this these))
    ; Find the farthest thing from this, that
    (setf that (farthest-from this these))
    ; Remove that from these
    (setf these (remove that these))
    ; Put this back
    (push this these)
    ; Now this is the farthest thing from that
    (setf this (farthest-from that these))

    ; Put them in their group
    (push this this-group)
    (push that that-group)

    ; Determine which group the elements of these belong to
    (dolist (element these)
      (let ((d-from-this (funcall distance-func element this))
	    (d-from-that (funcall distance-func element that)))
	(if (> d-from-this d-from-that)
	    (push element this-group)
	    (push element that-group))))

    ; Give em back
    (list (reverse this-group) (reverse that-group))))

(defun farthest-from (this those &key (distance-func 'distance))
  "Give me the farthest thing from this in those"
  (let ((max-distance 0)(temporary))
    (dolist (that those)
      (let ((d (funcall distance-func this that)))
	(if (> d max-distance)
	    (setf temporary that))))
    temporary))

(defun weighted-variance (c-node)
  (/ (+ (* (node-variance (node-right c-node))
	   (length (node-contents (node-right c-node))))
	(* (node-variance (node-left c-node))
	   (length (node-contents (node-left c-node)))))
     (+ (length (node-contents (node-right c-node)))
	(length (node-contents (node-left c-node))))))

(defun experiment (projects)
  (let* ((test (random-element projects))
	 (projects (remove test projects))
	 (compass-tree (compass projects :distance-func 'cosine-similarity))
	 (actual (first (last test)))
	 (predicted 0))

    (labels ((walk (c-node)
	       (if (> (node-variance c-node)
		      (weighted-variance c-node))
		   (median (node-contents c-node))
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

      (setf predicted (walk compass-tree)))
    (mre actual predicted)))
