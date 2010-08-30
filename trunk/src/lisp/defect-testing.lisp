(defparameter *DEFECT-DATASETS*
  '(jm1
    kc1
    mc1
    pc1))

(defun run-defect-tests (&optional (datasets *DEFECT-DATASETS*) &key (distance-func 'cosine-similarity) (normalize? NIL))
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
	      (push (compass-defect (nth k projects) projects 1.1 1.1 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp compass))

	;; Best-k
	;(let (tmp big-tmp)
	;  (dotimes (n 20)
	;    (dotimes (k (length projects))
	;      (push (best-k-predict (nth k projects) projects :distance-func distance-func) tmp))
	;    (push tmp big-tmp)
	;    (setf tmp nil))
	;  (push big-tmp best-k))

	;; k=16
	;(let (tmp big-tmp)
	;  (dotimes (n 20)
	;    (dotimes (k (length projects))
	;      (push (k-predict (nth k projects) projects 16 :distance-func distance-func) tmp))
	;    (push tmp big-tmp)
	;    (setf tmp nil))
	;  (push big-tmp k=16))

	;; k=8
	;(let (tmp big-tmp)
	;  (dotimes (n 20)
	;    (dotimes (k (length projects))
	;      (push (k-predict (nth k projects) projects 8 :distance-func distance-func) tmp))
	;    (push tmp big-tmp)
	;    (setf tmp nil))
	;  (push big-tmp k=8))

	;; k=4
	;(let (tmp big-tmp)
	;  (dotimes (n 20)
	;    (dotimes (k (length projects))
	;      (push (k-predict (nth k projects) projects 4 :distance-func distance-func) tmp))
	;    (push tmp big-tmp)
	;    (setf tmp nil))
	;  (push big-tmp k=4))

	;; k=2
	;(let (tmp big-tmp)
	;  (dotimes (n 20)
	;    (dotimes (k (length projects))
	;      (push (k-predict (nth k projects) projects 2 :distance-func distance-func) tmp))
	;    (push tmp big-tmp)
	;    (setf tmp nil))
	;  (push big-tmp k=2))

	;; k=1
	;(let (tmp big-tmp)
	;  (dotimes (n 20)
	;    (dotimes (k (length projects))
	;      (push (k-predict (nth k projects) projects 1 :distance-func distance-func) tmp))
	;    (push tmp big-tmp)
	;    (setf tmp nil))
	;  (push big-tmp k=1))

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
;    (push (reverse k=1) variants)
;    (push (reverse k=2) variants)
;    (push (reverse k=4) variants)
;    (push (reverse k=8) variants)
;    (push (reverse k=16) variants)
;    (push (reverse best-k) variants)
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
	    (format t "~A " (if (= n 0) "COMPASS"
				(if (= n 1) "BestK"
				    (if (= n 2) "K=16"
					(if (= n 3) "K=8"
					    (if (= n 4) "K=4"
						(if (= n 5) "K=2"
						    (if (= n 6) "K=1"))))))))
	    (format t "WIN: ~A TIE: ~A LOSS: ~A MDMRE: ~5,4f~%" win tie loss (median (condense-lists current-variant)))
	    ))))))
