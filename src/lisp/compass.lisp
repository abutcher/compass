(defstruct node
  rootp
  contents
  variance
  right
  left
  )

(defun compass (mitigations &key (min-cluster-size 4) (distance-func 'cosine-similarity) (variance-func 'variance))
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

(defun separate (these &key (distance-func 'cosine-similarity))
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

(defun farthest-from (this those &key (distance-func 'cosine-similarity))
  "Give me the farthest thing from this in those"
  (let ((max-distance 0) temporary)
    (dolist (that those)
      (let ((d (funcall distance-func this that)))
	(if (> d max-distance)
	    (setf temporary that))))
    temporary))

(defun closest-to (this those &key (distance-func 'cosine-similarity))
  "Give me the farthest thing from this in those"
  (let ((min-distance 999999) temporary)
    (dolist (that those)
      (let ((d (funcall distance-func this that)))
	(if (< d min-distance)
	    (setf temporary that))))
    temporary))

(defun weighted-variance (c-node)
  (if (or (null (node-right c-node)) (null (node-left c-node)))
      (if (null (node-right c-node))
	  (node-variance (node-left c-node))
	  (node-variance(node-right c-node)))
      (/ (+ (* (node-variance (node-right c-node))
	       (length (node-contents (node-right c-node))))
	    (* (node-variance (node-left c-node))
	       (length (node-contents (node-left c-node)))))
	 (+ (length (node-contents (node-right c-node)))
	    (length (node-contents (node-left c-node)))))))

(defun compass-teak (projects alpha beta)
  (let* ((test (random-element projects))
	 (projects (remove test projects))
	 (compass-tree (compass projects :distance-func 'cosine-similarity))
	 (pruned-tree (variance-prune compass-tree :alpha alpha :beta beta))
	 (actual (first (last test)))
	 (predicted 0))

    (labels ((walk (c-node)
	       (if (> (node-variance c-node)
		      (weighted-variance c-node))
		   (median (mapcar #'first (mapcar #'last (node-contents c-node))))
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
      (setf predicted (walk pruned-tree)))
    (mre actual predicted)))

(defun win-loss-tie (data pred-function)
  (let* ((data (shuffle-n data 20))
	 
	 )))

(defparameter *DATASETS*
  (list 'cocomo81
	'cocomo81e
	'cocomo81o
	'nasa93
	'nasa93-center-2
	'nasa93-center-5
	'desharnais-all
	'sdr))

(defparameter *PREDICTORS*
  (list 'compass
	'best-k
	'k=16
	'k=8
	'k=4
	'k=2
	'k=1))

(defun sets+preds ()
  (let ((sets (copy-list *DATASETS*))
	(preds (copy-list *PREDICTORS*)))
    (dolist (set sets)
      (setf (nth (position set sets) sets) (cons preds set)))
    sets))

(defun run-tests ()
  (let ((sets (copy-list *DATASETS*))
	compass best-k k=16 k=8 k=4 k=2 k=1)
    (dolist (set sets)
      (let ((projects (table-egs (funcall set))))
	
	;; Compass
	(format t "Compass vs. ~A~%" set)
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (compass-teak projects 1.9 1.9) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp compass))

	;; Best-k
	(let (tmp big-tmp)
	(format t "Best-k vs. ~A~%" set)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (best-k-predict projects) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp best-k))

	;; k=16
	(format t "k=16 vs. ~A~%" set)
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict projects 16) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=16))

	;; k=8
	(format t "k=8 vs. ~A~%" set)
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict projects 8) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=8))
	
	;; k=4
	(format t "k=4 vs. ~A~%" set)
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict projects 4) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=4))

	;; k=2
	(format t "k=2 vs. ~A~%" set)
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict projects 2) tmp))
	    (push tmp big-tmp)	
	    (setf tmp nil))
	  (push big-tmp k=2))

	;; k=1
	(format t "k=1 vs. ~A~%" set)
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict projects 1) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=1))
	))
    ))