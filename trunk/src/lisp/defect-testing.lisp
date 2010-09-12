(defparameter *DEFECT-DATASETS*
  '(small))

(defun leave-one-out-defect-tests (&key (datasets *DEFECT-DATASETS*) (distance-func 'cosine-similarity) (repeat 20))
  (let ((sets (copy-list datasets))
	FinalList)

    (dolist (set sets)
      (let ((data (table-egs (funcall set))))

	(print "Running Compass-vanilla")

	;;Compass with plain stopping rule
	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (compass-defect-plain (nth i data) data 1.1 1.1 :distance-func distance-func)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list 
		     "vanilla-compass"
		     (list 
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
			 tmp)))
	  (push tmp FinalList))

	(print "Running Compass-1up")

	;;Compass with 1up stopping rule
	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (compass-defect-1up (nth i data) data 1.1 1.1 :distance-func distance-func)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "Compass-1up"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid)))) 
		  tmp)))
	  (push tmp FinalList))

	(print "running k-means 1")
	
	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-defect 1 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "K=1"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		  tmp)))
	  (push tmp FinalList))

	(print "Running k-means 2")

	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-defect 2 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "K=2"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		  tmp)))
	  (push tmp FinalList))

	(print "running k-means 4")

	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-defect 4 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			(progn
			  (incf (nth 3 true-grid))
			  (incf (nth 0 false-grid))) 
			(progn
			  (incf (nth 0 true-grid))
			  (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "K=4"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid)))) 
		  tmp)))
	  (push tmp FinalList))

	(print "running k-means 8")

	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-defect 8 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "K=8"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		  tmp)))
	  (push tmp FinalList))

	(print "running k-means 16")
	
	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-defect 16 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list 
		     "K=16"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third  true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid)))) 
		  tmp)))
	  (push tmp FinalList))

	(print "running bisect k-means 4")

	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-bisecting-defect 4 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "BisectK=4"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		    tmp)))

	  (push tmp FinalList))

	(print "running bisect k-means 6")
	
	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-bisecting-defect 6 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "BisectK=6"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		    tmp)))
	  (push tmp FinalList))

	(print "running bisect k-means 8")

	(let* ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (k=?-bisecting-defect 8 (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list
		     "BisectK=8"
		     (list 
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		    tmp)))
	  (push tmp FinalList))

	(print "running best-k")
	
	(let ((tmp))
	  (dotimes (n repeat)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i data))))
		       (got (best-k-defect (nth i data) data)))
		  (if (equal want got)
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 3 true-grid))
			    (incf (nth 0 false-grid))) 
			  (progn
			    (incf (nth 0 true-grid))
			    (incf (nth 3 false-grid))))
		      (if (equal want 'TRUE)
			  (progn
			    (incf (nth 2 true-grid))
			    (incf (nth 1 false-grid)))
			  (progn
			    (incf (nth 1 true-grid))
			    (incf (nth 2 false-grid)))))))
	      (push (list 
		     "Best-K"
		     (list
		      (pd (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (pf (first true-grid) (second true-grid) (third true-grid) (fourth true-grid))
		      (harmonic-mean (first true-grid) (second true-grid) (third true-grid) (fourth true-grid)))
		     (list
		      (pd (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (pf (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))
		      (harmonic-mean (first false-grid) (second false-grid) (third false-grid) (fourth false-grid))))
		    tmp)))
	  (push tmp FinalList))))

	(setf FinalList (reverse FinalList))
	(print "create winlosstie table")

	(let* ((WinLossTie (make-list (length FinalList))))
	  (dotimes (Item (length WinLossTie))
					;(nth i WinLossTie) for Cluster
	    (print "dolist")
	    (setf (nth Item WinLossTie) (list
			0
			;;Win-Loss-Tie-(win-loss)%
			;(nth i (nth j WinLossTie)) for i Cluster and J Name, true, or false
			;(nth i (nth j (nth k WinLossTie))) for i Cluster, J Name, true or false, and k for pd/pf/har. mean
			;(nth i (nth j (nth k (nth l WinLosstie)))) for all above and Win Loss Tie and win-loss%
			(list "true"
					;pd
			      (list 0 0 0 0)
					;pf
			      (list 0 0 0 0)
					;harmonic mean
			      (list 0 0 0 0))
			(list "false"
					;pd
			      (list 0 0 0 0)
					;pf
			      (list 0 0 0 0)
					;harmonic mean
			      (list 0 0 0 0)))))
	  ;Set the names of each clusterer
	  (dotimes (num (length FinalList))
	    (setf (first (nth num WinLossTie)) (first (nth num FinalList))))

	  ;Each time we loop, we remove the first so we don't double count stats.
	  (dotimes (i (- (length FinalList) 1))
	    (let* ((RestList (nthcdr (+ i 1) FinalList))
		   (FirstItem (nth i FinalList)))
	      (dotimes (j (length RestList))
		(let* ((SecondItem (nth j RestList)))
		  (dotimes (k (length FirstItem))
		    ;count true pd
		    (print "true pd")
		    (if (equal 1 (wilcoxon (nth k (second (first FirstItem)))
					   (nth k (second (first SecondItem)))))
			(progn
			  (incf (nth i (nth 1 (nth 0 (nth 2 WinLossTie)))))
			  (incf (nth (+ i j 1) (nth 1 (nth 0 (nth 2 WinLossTie)))))
			(if (> (nth k (second (first FirstItem)))
			       (nth k (second (first SecondItem))))
			    (progn
			      (incf (nth i (nth 1 (nth 0 (nth 0 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 1 (nth 0 (nth 1 WinLossTie))))))
			    (progn
			      (incf (nth i (nth 1 (nth 0 (nth 1 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 1 (nth 0 (nth 0 WinLossTie)))))))))
		  
		
		    ;count true pf's
		    (print "true pf")
		    (if (equal 1 (wilcoxon (nth k (second (second FirstItem)))
					   (nth k (second (second SecondItem)))))
			(progn
			  (incf (nth i (nth 1 (nth 1 (nth 2 WinLossTie)))))
			  (incf (nth (+ i j 1) (nth 1 (nth 1 (nth 2 WinLossTie)))))
			(if (> (nth k (second (second FirstItem)))
			       (nth k (second (second SecondItem))))
			    (progn
			      (incf (nth i (nth 1 (nth 1 (nth 0 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 1 (nth 1 (nth 1 WinLossTie))))))
			    (progn
			      (incf (nth i (nth 1 (nth 1 (nth 1 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 1 (nth 1 (nth 0 WinLossTie)))))))))

		    ;true harmonic mean
		    (print "true harmonic mean")
		    (if (equal 1 (wilcoxon (nth k (second (third FirstItem)))
					   (nth k (second (third SecondItem)))))
			(progn
			  (incf (nth i (nth 1 (nth 2 (nth 2 WinLossTie)))))
			  (incf (nth (+ i j 1) (nth 1 (nth 2 (nth 2 WinLossTie)))))
			(if (> (nth k (second (third FirstItem)))
			       (nth k (second (third SecondItem))))
			    (progn
			      (incf (nth i (nth 1 (nth 2 (nth 0 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 1 (nth 2 (nth 1 WinLossTie))))))
			    (progn
			      (incf (nth i (nth 1 (nth 2 (nth 1 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 1 (nth 2 (nth 0 WinLossTie)))))))))



	    ;count false pd's
		    (if (equal 1 (wilcoxon (nth k (third (first FirstItem)))
					   (nth k (third (first SecondItem)))))
			(progn
			  (incf (nth i (nth 2 (nth 0 (nth 2 WinLossTie)))))
			  (incf (nth (+ i j 1) (nth 2 (nth 0 (nth 2 WinLossTie)))))
			(if (> (nth k (third (first FirstItem)))
			       (nth k (third (first SecondItem))))
			    (progn
			      (incf (nth i (nth 2 (nth 0 (nth 0 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 2 (nth 0 (nth 1 WinLossTie))))))
			    (progn
			      (incf (nth i (nth 2 (nth 0 (nth 1 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 2 (nth 0 (nth 0 WinLossTie)))))))))

	    ;count false pf's
		    (if (equal 1 (wilcoxon (nth k (third (second FirstItem)))
					   (nth k (third (second SecondItem)))))
			(progn
			  (incf (nth i (nth 2 (nth 1 (nth 2 WinLossTie)))))
			  (incf (nth (+ i j 1) (nth 2 (nth 1 (nth 2 WinLossTie)))))
			(if (> (nth k (third (second FirstItem)))
			       (nth k (third (second SecondItem))))
			    (progn
			      (incf (nth i (nth 2 (nth 1 (nth 0 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 2 (nth 1 (nth 1 WinLossTie))))))
			    (progn
			      (incf (nth i (nth 2 (nth 1 (nth 1 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 2 (nth 1 (nth 0 WinLossTie)))))))))	

	    ;count false harmonic mean
		    (if (equal 1 (wilcoxon (nth k (third (third FirstItem)))
					   (nth k (third (third SecondItem)))))
			(progn
			  (incf (nth i (nth 2 (nth 2 (nth 2 WinLossTie)))))
			  (incf (nth (+ i j 1) (nth 2 (nth 2 (nth 2 WinLossTie)))))
			(if (> (nth k (third (third FirstItem)))
			       (nth k (third (third SecondItem))))
			    (progn
			      (incf (nth i (nth 2 (nth 2 (nth 0 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 2 (nth 2 (nth 1 WinLossTie))))))
			    (progn
			      (incf (nth i (nth 2 (nth 2 (nth 1 WinLossTie)))))
			      (incf (nth (+ i j 1) (nth 2 (nth 2 (nth 0 WinLossTie))))))))))))))

	  WinLossTie

)))


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

(defun harmonic-mean (a b c d)
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
    (pf a b c d)
    (pd a b c d))
   (+
    (pf a b c d)
    (pd a b c d)))))

