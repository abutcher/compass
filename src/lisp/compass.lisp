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
  (let (this that this-group that-group left-right)
    ; Pick one at random, this
    (setf this (random-element these))
    ; Remove this from these
    (setf these (remove this these))
    ; Find the farthest thing from this, that
    (setf that (farthest-from this these :distance-func distance-func))
    ; Remove that from these
    (setf these (remove that these))
    ; Put this back
    (push this these)
    ; Now this is the farthest thing from that
    (setf this (farthest-from that these :distance-func distance-func))
    ; Take the new this out
    (setf these (remove this these))

    ; Put them in their group
    (push this this-group)
    (push that that-group)

    (setf left-right (funcall distance-func this that))

    ; Using lemma 1 from elkan03 determine which group each element
    ; belongs to
;    (dolist (element these)
;      (let ((d-from-that (funcall distance-func element that)))
;	(if (>= left-right (* 2 d-from-that))
;	    (push element that-group)
;	    (push element this-group))))

    (dolist (element these)
      (let ((d-from-this (funcall distance-func element this))
	    (d-from-that (funcall distance-func element that)))
	(if (> d-from-this d-from-that)
	    (push element that-group)
	    (push element this-group))))
    
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
  (if (and (null (node-right c-node)) (null (node-left c-node)))
      (node-variance c-node)
      (if (or (null (node-right c-node)) (null (node-left c-node)))
	  (if (null (node-right c-node))
	      (node-variance (node-left c-node))
	      (node-variance (node-right c-node)))
	  (/ (+ (* (node-variance (node-right c-node))
		   (length (node-contents (node-right c-node))))
		(* (node-variance (node-left c-node))
		   (length (node-contents (node-left c-node)))))
	     (+ (length (node-contents (node-right c-node)))
		(length (node-contents (node-left c-node))))))))
  
(defun compass-teak (this projects alpha beta &key (distance-func 'cosine-similarity))
  (let* ((test this)
	 (projects (remove test projects))
	 (compass-tree (compass projects :min-cluster-size 4 :distance-func distance-func))
	 (pruned-tree (variance-prune compass-tree :alpha alpha :beta beta))
	 (actual (first (last test)))
	 (predicted 0))
    
    (labels ((walk (c-node)
	       (if (< (realpart (node-variance c-node))
		      (realpart (weighted-variance c-node)))
		   (setf predicted (median (mapcar #'first (mapcar #'last (node-contents c-node)))))
		   (if (or (null (node-right c-node))
			   (null (node-left c-node)))
		       (progn 
			 (unless (null (node-right c-node))
			   (walk (node-right c-node)))
			 (unless (null (node-left c-node))
			   (walk (node-left c-node))))
		       (if (> (realpart (weighted-variance (node-right c-node)))
			      (realpart (weighted-variance (node-left c-node))))
			   (walk (node-left c-node))
			   (walk (node-right c-node)))))))
      (walk pruned-tree))
    (mre actual predicted)))

(defparameter *DATASETS*
  '(albrecht
    china
    cocomo81
    cocomo81e
    cocomo81o
    cocomo81s
    desharnais-all
    desharnais-l1
    desharnais-l2
    desharnais-l3
    finnish
    kemerer
    maxwell
    nasa93-center-1
    nasa93-center-2
    nasa93-center-5
    nasa93
    sdr
    telecom))

(defun run-tests (&optional (datasets *DATASETS*) &key (distance-func 'cosine-similarity) (normalize? NIL))
  (let ((sets (copy-list datasets))
	compass best-k k=16 k=8 k=4 k=2 k=1 bisectk=4 bisectk=6 bisectk=8 variants)
    (dolist (set sets)
      (let ((projects (table-egs (funcall set))))

	(if normalize?
	    (setf projects (normalize projects)))
	
	;; Compass
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (compass-teak (nth k projects) projects 1.1 1.1 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp compass))
	
	;; Best-k
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (best-k-predict (nth k projects) projects :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp best-k))
	
	;; k=16
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict (nth k projects) projects 16 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=16))

	;; k=8
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict (nth k projects) projects 8 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=8))
	
	;; k=4
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict (nth k projects) projects 4 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=4))

	;; k=2
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict (nth k projects) projects 2 :distance-func distance-func) tmp))
	    (push tmp big-tmp)	
	    (setf tmp nil))
	  (push big-tmp k=2))

	;; k=1
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (k-predict (nth k projects) projects 1 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=1))

	;; k=6 bisecting k-means
;	(let (tmp big-tmp)
;	  (dotimes (n 20)
;	    (dotimes (k (length projects))
;	      (push (k=?-bisecting-test 4 projects) tmp))
;	    (push tmp big-tmp)
;	    (setf tmp nil))
;	  (push big-tmp bisectk=4))

	;; k=6 bisecting k-means
;	(let (tmp big-tmp)
;	  (dotimes (n 20)
;	    (dotimes (k (length projects))
;	      (push (k=?-bisecting-test 6 projects) tmp))
;	    (push tmp big-tmp)
;	    (setf tmp nil))
;	  (push big-tmp bisectk=6))
	
	;; k=8 bisecting k-means
;	(let (tmp big-tmp)
;	  (dotimes (n 20)
;	    (dotimes (k (length projects))
;	      (push (k=?-bisecting-test 6 projects) tmp))
;	    (push tmp big-tmp)
;	    (setf tmp nil))
;	  (push big-tmp bisectk=8))
	))
    

;    (push (reverse bisectk=8) variants)
;    (push (reverse bisectk=6) variants)
;    (push (reverse bisectk=4) variants)
    (push (reverse k=1) variants)
    (push (reverse k=2) variants)
    (push (reverse k=4) variants)
    (push (reverse k=8) variants)
    (push (reverse k=16) variants)
    (push (reverse best-k) variants)
    (push (reverse compass) variants)

    (dolist (set sets)
      (let* ((applicable-variants (mapcar #'(lambda (x) (nth (position set sets) x)) variants)))
	
	(format t "~A~%" set)
	
	(dotimes (n (length applicable-variants))
	  (let* ((current-variant (nth n applicable-variants))
		 (other-variants 
		  (remove (nth n applicable-variants) (copy-list applicable-variants)))
		 (win 0)(tie 0)(loss 0))
	    (dolist (variant other-variants)
	      (dotimes (k (length variant))
		(let ((wilcox (wilcoxon (nth k current-variant) (nth k variant))))
		  (if (= wilcox 1)
		      (incf tie)
		      (let ((cur-med (median (nth k current-variant)))
			    (var-med (median (nth k variant))))
			(if (< cur-med var-med)
			    (incf win)
			    (incf loss)))))))
	    (format t "~A " (if (= n 0) "Compass"
				(if (= n 1) "BestK"
				    (if (= n 2) "K=16"
					(if (= n 3) "K=8"
					    (if (= n 4) "K=4"
						(if (= n 5) "K=2"
						    (if (= n 6) "K=1"))))))))
;	    (format t "~A " (if (= n 0) "COMPASS"
;				(if (= n 1) "BestK"
;				    (if (= n 2) "K=16"
;					(if (= n 3) "K=8"
;					    (if (= n 4) "K=4"
;						(if (= n 5) "K=2"
;						    (if (= n 6) "K=1"
;							(if (= n 7) "BISECT4"
;							    (if (= n 8) "BISECT6"
;								(if (= n 9) "BISECT8")))))))))))
	    (format t "WIN: ~A TIE: ~A LOSS: ~A MDMRE: ~5,4f~%" win tie loss (median (condense-lists current-variant)))
	    ))))))

