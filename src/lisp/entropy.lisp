(defun entropy (population)
  "Return the uncertainty value assigned to a given population of
   binary inputs"
  (let ((columns (transpose population))
	(frequencies)
	(total-entropy 0))
    ; Make a frequency table for each column
    (dolist (col columns)
      (push (make-hash-table :test #'equal) frequencies)
      (dolist (element col)
	(if (null (gethash element (first frequencies)))
	    (setf (gethash element (first frequencies)) 1)
	    (incf (gethash element (first frequencies))))))
    ; Walk down each member of the population and assign an entropy
    ; score.  If this were a regular table it would be more efficient
    ; to keep a cross index at every level of the tree for all of the
    ; miniature tables.
    (dolist (piece population)
      (let ((e 0))
	(dolist (element piece)
	  (let ((p-num 0)(p-denom 0))
	    (dohash (key value 
			 (nth (position element piece) 
			      (reverse frequencies)))
	      (if (= element key)
		  (progn 
		    (setf p-num (+ p-num value))
		    (setf p-denom (+ p-denom value)))
		  (setf p-denom (+ p-denom value))))
	    (setf e (+ e (* (/ p-num p-denom) (log (/ p-num p-denom) 2))))))
	(format t "Entropy for ~A: ~A~%" piece (* -1 e))
	(setf total-entropy (+ total-entropy (* -1 e)))))
    total-entropy))