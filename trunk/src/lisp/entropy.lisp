(defun entropy-variance(population &optional (p NIL))
  (let* ((InputPopulation (mapcar #'last population)))
    (entropy InputPopulation p)))

(defun entropy (population &optional (p NIL))
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
	(dotimes (i (length piece))
	  (let ((p-num 0)(p-denom 0))
	    (dohash (key value
			 (nth i (reverse frequencies)))
	      (if (eql (nth i piece) key)
		  (progn 
		    (setf p-num (+ p-num value))
		    (setf p-denom (+ p-denom value)))
		  (setf p-denom (+ p-denom value))))
	    (setf e (+ e (* (/ p-num p-denom) (log (/ p-num p-denom) 2))))))
	(setf total-entropy (+ total-entropy (* -1 e)))
	(unless (null p)
	    (format t "~A H:~A~%" piece (* -1 e)))
	))
    (/ total-entropy (length population))))

; If nothing changes between each member of our population then our
; level of uncertainty is zero.

"CL-USER> (entropy (list (list 'a 'b 'c) (list 'a 'b 'c)))
 Entropy for (A B C): -0.0
 Entropy for (A B C): -0.0
 0.0"
