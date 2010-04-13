(defstruct node
  rootp
  head
  contents
  right
  left
  )

(defun compass (mitigations &key (min-cluster-size 4))
  (let ((left (make-node :rootp T))
	(right (make-node :rootp T)))
    
    ; Make initial split
    (let ((initial-split (separate mitigations)))
      ; Set up left node with a header and it's contents
      (setf (node-head left) (car (first initial-split)))
      (setf (node-contents left) (cdr (first initial-split)))
      ; Set up right node with a header and it's contents
      (setf (node-head right) (car (second initial-split)))
      (setf (node-contents right) (cdr (second initial-split))))

    ; Recursive node-walker
    (labels ((walk (node)
	       (let ((node-split (separate (node-contents node))))
		 (if (>= (length (cdr (first node-split))) min-cluster-size)
		     (setf (node-left node)
			   (make-node
			    :head (car (first node-split))
			    :contents (cdr (first node-split)))))
		 (if (>= (length (cdr (second node-split))) min-cluster-size)
		     (setf (node-right node)
			   (make-node
			    :head (car (second node-split))
			    :contents (cdr (second node-split)))))
		 (if (not (null (node-left node)))
		     (walk (node-left node)))
		 (if (not (null (node-right node)))
		     (walk (node-right node))))))
    
      ; Recursively build upon each node
      (walk right)
      (walk left))
    
    (list right left)))

(defun print-nodes (left right &optional (stream *standard-output*))
  "Pretty print built nodes"
  (labels ((walk (node &optional (level 0))
	     (format stream "LEVEL: ~A~%" level)
	     (if (node-rootp node)
		 (format stream "LOOKING AT ROOT NODE~%"))
	     (format stream "HEAD: ~A~%" (node-head node))
	     (format stream "CONTENTS:~%")
	     (dolist (element (node-contents node))
	       (format stream "~A~%" element))
	     (format stream "~%~%")
	     (if (not (null (node-right node)))
		 (walk (node-right node) (1+ level)))
	     (if (not (null (node-left node)))
		 (walk (node-left node) (1+ level)))))
    (walk left)
    (walk right)))

(defun test-compass (file)
  "Sample run"
  (with-open-file (stream file
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create )
    (let* ((mitigations (generator))
	   (tree (compass mitigations)))
      (format stream "TEST SIZE: ~A~%~%" (length mitigations))
      (print-nodes (first tree) (second tree) stream))))

(defun separate (these)
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
      (let ((d-from-this (distance element this))
	    (d-from-that (distance element that)))
	(if (> d-from-this d-from-that)
	    (push element this-group)
	    (push element that-group))))

    ; Give em back
    (list (reverse this-group) (reverse that-group))))

(defun farthest-from (this those)
  "Give me the farthest thing from this in those"
  (let ((max-distance 0)(temporary))
    (dolist (that those)
      (let ((d (distance this that)))
	(if (> d max-distance)
	    (setf temporary that))))
    temporary))
  
