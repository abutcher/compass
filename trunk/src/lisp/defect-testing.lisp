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

	(print "Running Compass-vanilla")

	;;Compass with plain stopping rule
	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (compass-defect-plain (nth i data) data 1.1 1.1 :distance-func distance-func)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) vanilla-compass)))

	(print "Running Compass-1up")

	;;Compass with 1up stopping rule
	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (compass-defect-1up (nth i data) data 1.1 1.1 :distance-func distance-func)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) compass-1up)))

	(print "running k-means 1")
	
	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-defect 1 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) k=1)))

	(print "Running k-means 2")

	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-defect 2 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) k=2)))

	(print "running k-means 4")

	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-defect 4 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) k=4)))

	(print "running k-means 8")

	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-defect 8 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) k=8)))

	(print "running k-means 16")
	
	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-defect 16 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) k=16)))

	(print "running bisect k-means 4")

	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-bisecting-defect 4 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) bisectk=4)))

	(print "running bisect k-means 6")
	
	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-bisecting-defect 6 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) bisectk=6)))

	(print "running bisect k-means 8")

	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (k=?-bisecting-defect 8 (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) bisectk=8)))

	(print "running best-k")
	
	(dotimes (n repeat)
	  (let* ((true-grid (list 0 0 0 0))
		 (false-grid (list 0 0 0 0)))
	    (dotimes (i (length data))
	      (let* ((want (last (first (nth i data))))
		     (got (best-k-defect (nth i data) data)))
		(if (equal want got)
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 4 true-grid))
			  (incf (nth 1 false-grid))) 
			(progn
			  (incf (nth 1 true-grid))
			  (incf (nth 4 false-grid))))
		    (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 2 false-grid)))
			(progn
			  (incf (nth 2 true-grid))
			  (incf (nth 3 false-grid)))))))
	    (push (list true-grid false-grid) best-k)))

	


))))

(defun balance(pd pf)
  (- 1 (/
    (sqrt
         (+
          (square
           (-
            0
            pf))
          (square
           (-
            1
            pd))))
        (sqrt 2))))

(defun prec(a b c d)
  (/
   d
   (if (and
        (eql c 0)
        (eql d 0))
       1
       (+ c d))))

(defun acc(a b c d)
  (/
   (+ a d)
   (if (and
        (eql a 0)
        (eql b 0)
        (eql c 0)
        (eql d 0))
       1
       (+ a b c d))))

(defun pd(a b c d)
  (/
   d
   (if
    (and
     (eql b 0)
     (eql d 0))
    1
    (+ b d))))

(defun pf (a b c d)
  (/
   c
   (if (and
        (eql a 0)
        (eql c 0))
       1
       (+ a c))))

(defun f-calc (a b c d)
(let* ((a (if (eql a 0)
              1
              a))
       (b (if (eql b 0)
              1
              b))
       (c (if (eql c 0)
              1
              c))
       (d (if (eql d 0)
              1
              d)))
  (/ (* 2 (prec a b c d) (acc a b c d)) (+ (prec a b c d) (acc a b c d)))))

(defun g (a b c d)
(let* ((a (if (eql a 0)
              1
              a))
       (b (if (eql b 0)
              1
              b))
       (c (if (eql c 0)
              1
              c))
       (d (if (eql d 0)
              1
              d)))
  (/
   (*
    2
    (- 1 (pf a b c d))
    (pd a b c d))
   (+
    (- 1 (pf a b c d))
    (pd a b c d)))))

