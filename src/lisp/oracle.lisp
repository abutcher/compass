(defun data-oracle (data)
  (let* ((data (shuffle-n data 20)) ;; Randomize 20x 
	 ;; Build a compass tree with the first half of the data to
	 ;; serve as the oracle.
;	 (compass-oracle (variance-prune
;			  (compass (first-half data) 
;				   :distance-func 'euclidean-distance)
;			  :alpha 1.1 :beta 1.1))

	 ;; Use the second half as the incoming data.
;	 (data (second-half data))
	 (eras (era data :n 5)) ;; Which era size is best?

	 ;; The first X eras will be used to make an initial compass
	 ;; tree.
	 (compass-tree (compass (condense-lists (subseq eras 0 2))
				:distance-func 'euclidean-distance))
	 
	 ;; Remove the starter eras from the list of eras.
	 (eras (subseq eras 2)))
    
    ;; Incremental insertion procedure which puts each new instance
    ;; where it belongs in the scheme of things.
    (labels ((insert (c-node instance)
	       ;; Put the new instance into c-node's contents.
	       (setf (node-contents c-node) 
		     (push instance (node-contents c-node)))
	       ;; Update variance.
	       (setf (node-variance c-node)
		     (variance (node-contents c-node)))
	       ;; Determine which child the new instance belongs to,
	       ;; if c-node has children at all.
	       (if (and (null (node-right c-node)) (null (node-left c-node)))
		   ;; c-node has no children, return with what we've got.
		   (return-from insert)
		   (if (or (null (node-right c-node)) (null (node-left c-node)))
		       (if (null (node-right c-node))
			   (insert (node-left c-node) instance)
			   (insert (node-right c-node) instance))
		       ;; Parents node has two children, so decide
		       ;; which to give the new child.
		       (if (> (euclidean-distance 
			       instance
			       (centroid (node-contents (node-right c-node))))
			      (euclidean-distance
			       instance
			       (centroid (node-contents (node-left c-node)))))
			   (insert (node-left c-node) instance)
			   (insert (node-right c-node) instance))))))

      ;; Walk through each era, while slowly building a second compass
      ;; tree. Once per era, greedily pick the most interesting one,
      ;; label it using the first compass tree and reposition it.      
      
      (dolist (this-era eras)
	;; Push the whole era into the tree.
	(dolist (instance this-era)
	  (insert compass-tree instance))
	;; Using some heuristic within, re-compass certain parts of
	;; the tree internally.
	(re-compass compass-tree))
      compass-tree)))

(defun re-compass (ctree-node)
  (let ((maxv (max-leaf-variance ctree-node))
	(maxs (max-leaf-size ctree-node))
	(preverse (copy-node ctree-node)))
    (labels ((walk (c-node)
	       ;; Based on difference in children variance.
	       (if (and 
		    (and (not (null (node-right c-node)))
			 (not (null (node-left c-node))))
		    (> (abs (- (node-variance (node-left c-node))
			       (node-variance (node-right c-node))))
		       500)) ;; Some value?
	       ;; Based on max variance.
;	       (if (= (node-variance c-node) maxv)
	       ;; Based on max size.
;	       (if (= (length (node-contents c-node)) maxs)
		   (let ((new-node (compass (node-contents c-node)
					    :distance-func 'euclidean-distance)))
		     (setf c-node new-node))
		   (progn
		     (unless (null (node-right c-node))
		       (walk (node-right c-node)))
		     (unless (null (node-left c-node))
		       (walk (node-left c-node)))))))
      (walk ctree-node)))
  ctree-node)

;(defun convert-tree-to-list (c-node)
;  (let (l)
;    (labels ((walk (c-node)
;	       (push c-node l)
;	       (unless (null (node-right c-node))
;		 (walk (node-right c-node)))
;	       (unless (null (node-left c-node))
;		 (walk (node-left c-node)))))
;      (walk c-node))
;    l))

;(defun data-oracle (data n)
;  (let* ((data-copy (copy-list data))
;	 (interesting (an-interesting-instance-1 data-copy 8))
;	 closest-list)
;    (dotimes (i  n)
;      (push (closest-to interesting data-copy) closest-list)
;      (setf data-copy (remove (first closest-list) (copy-list data-copy))))
;    closest-list))

(defun an-interesting-instance-1 (data k)
  ;; Find the two clusters with the most distant centroids.
  (let ((clusters (meat-processor k (k-means k data)))
	farthest-two
	(best-dist 0))
    (dolist (cluster clusters)
      (let ((other-clusters (remove cluster (copy-list clusters))))
	(dolist (other-cluster other-clusters)
	  (if (< best-dist (euclidean-distance
			    (centroid cluster) 
			    (centroid other-cluster)))
	      (progn
		(setf farthest-two (list cluster other-cluster))
		(setf best-dist (euclidean-distance 
				 (centroid cluster) 
				 (centroid other-cluster))))))))
    (centroid (condense-lists farthest-two))))

(defun oracle-test (&optional (datasets *DATASETS*) 
		    &key (distance-func 'euclidean-distance) (normalize? NIL))
  (let ((sets (copy-list datasets))
	compass oracle variants)
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

	;; Oracle
	(let (tmp big-tmp)
	  (dotimes (n 20)
	    (dotimes (k (length projects))
	      (push (oracle-teak (nth k projects) projects 1.1 1.1 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp oracle))
	))
    
    (push (reverse oracle) variants)
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
	    (format t "~A " (if (= n 0) "ORACLE"
				(if (= n 1) "COMPASS")))
	    (format t "WIN: ~A TIE: ~A LOSS: ~A MDMRE: ~5,4f~%" win tie loss (median (condense-lists current-variant)))))))))

(defun oracle-teak (this projects alpha beta &key (distance-func 'cosine-similarity))
  (let* ((test this)
	 (projects (remove test projects))
	 (oracle-tree (data-oracle projects))
	 (pruned-tree (variance-prune oracle-tree :alpha alpha :beta beta))
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

