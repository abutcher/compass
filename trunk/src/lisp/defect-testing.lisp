(defparameter *DEFECT-DATASETS*
  '(jm1
    kc1
    mc1
    pc1))

(defun leave-one-out-defect-tests (&key (datasets *DEFECT-DATASETS*) (distance-func 'cosine-similarity) (repeat 20))
  (let ((sets (copy-list datasets))
	best-k k=16 k=8 k=4 k=2 k=1 bisectk=4 bisectk=6 bisectk=8
	vanilla-compass compass-1up)
    (dolist (set sets)
      (let ((data (table-egs (funcall set))))

	;;Compass with plain stopping rule
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (compass-defect-plain (nth i data) data 1.1 1.1 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp vanilla-compass))

	;;Compass with 1up stopping rule
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (compass-defect-1up (nth i data) data 1.1 1.1 :distance-func distance-func) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp compass-1up))

	
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-defect 1 (nth i data) data) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=1))

	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-defect 2 (nth i data) data) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=2))

	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-defect 4 (nth i data) data) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=4))
	
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-defect 8 (nth i data) data) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=8))
	
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-defect 16 (nth i data) data) tmp))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp k=16))

	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-bisecting-defect 4 (nth i data) data) tmp ))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp bisectk=4))
	
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-bisecting-defect 6 (nth i data) data) tmp ))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp bisectk=6))
	
	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (k=?-bisecting-defect 8 (nth i data) data) tmp ))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp bisectk=8))

	(let (tmp big-tmp)
	  (dotimes (n repeat)
	    (dotimes (i (length data))
	      (push (best-k-defect (nth i data) data) tmp ))
	    (push tmp big-tmp)
	    (setf tmp nil))
	  (push big-tmp best-k))

))))