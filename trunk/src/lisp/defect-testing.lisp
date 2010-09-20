(defparameter *DEFECT-DATASETS*
  '(jm1
    kc1
    mc1
   ;small
))

(defun repeat-defect-test (&key (datasets *DEFECT-DATASETS*) (distance-func 'cosine-similarity) (repeat 20) (trainpercent .66))
  (let ((sets (copy-list datasets)))

    (dolist (set sets)
      (let (FinalList
	    WinLossTieResult
	    TimeList)

	(print set)

	;;Compass with plain stopping rule
	(print "compass-vanilla")
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
		   (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		     (let* ((tree (compass train-set :variance-func 'entropy)))
		       (dotimes (i (length test-set)) 
			 (let* ((want (first (last (nth i test-set))))
				(got (compass-defect-plain-tree (nth i test-set) tree 1.1 1.1 :distance-func distance-func)))
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
				     (incf (nth 2 false-grid)))))))))
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
			 tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "Running Compass-1up")

	;;Compass with 1up stopping rule
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((tree (compass train-set :variance-func 'entropy)))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (compass-defect-1up-tree (nth i test-set) tree 1.1 1.1 :distance-func distance-func)))
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
				(incf (nth 2 false-grid)))))))))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 1")
	
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters (meat-processor 1 (k-means 1 train-set))))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) clusters)))
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
				(incf (nth 2 false-grid)))))))))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "Running k-means 2")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters (meat-processor 2 (k-means 2 train-set))))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i train-set) clusters)))
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
				(incf (nth 2 false-grid)))))))))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 4")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters (meat-processor 4 (k-means 4 train-set))))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) train-set)))
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
				(incf (nth 2 false-grid)))))))))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 8")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters (meat-processor 8 (k-means 8 train-set))))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) clusters)))
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
				(incf (nth 2 false-grid)))))))))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 16")
	
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters (meat-processor 16 (k-means 16 train-set))))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) clusters)))
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
				(incf (nth 2 false-grid)))))))))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running bisect k-means 4")

	(let* (tmp
	      ( tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
		   (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		     (let* ((clusters  (bisecting-k-means-defect 4 train-set)))
		       (dotimes (i (length test-set))
			 (let* ((want (first (last (nth i test-set))))
				(got (matching-cluster-majority-vote (nth i test-set) clusters)))
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
				     (incf (nth 2 false-grid)))))))))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running bisect k-means 6")
	
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters  (bisecting-k-means-defect 6 train-set)))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) clusters))) 
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
				(incf (nth 2 false-grid)))))))))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running bisect k-means 8")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters  (bisecting-k-means-defect 8 train-set)))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) clusters)))
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
				(incf (nth 2 false-grid)))))))))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running best-k")
	
	(let* (tmp
	      (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0)))
	      (multiple-value-bind (train-set test-set) (split (funcall set) trainpercent)
		(let* ((clusters (best-k-defect-cluster train-set)))
		  (dotimes (i (length test-set))
		    (let* ((want (first (last (nth i test-set))))
			   (got (matching-cluster-majority-vote (nth i test-set) clusters)))
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
				(incf (nth 2 false-grid)))))))))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(setf FinalList (reverse FinalList)) 
	(setf TimeList (reverse TimeList))

	(let* ((WinLossTie (make-list (length FinalList))))
	  (dotimes (Item (length WinLossTie))
					;(nth i WinLossTie) for Cluster
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
	    (setf (first (nth num WinLossTie)) (first (first (nth num FinalList)))))

	  ;Each time we loop, we remove the first so we don't double count stats.
	  (dotimes (i (- (length FinalList) 1))
	    (let* ((RestList (nthcdr (+ i 1) FinalList))
		   (FirstItem (nth i FinalList)))
	      (dotimes (j (length RestList))
		(let* ((SecondItem (nth j RestList)))
		  (dotimes (k (length FirstItem))
		    ;count true pd
		   
		    (if (or (equal (first (second (nth k FirstItem)))
				   (first (second (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (first (second (nth k FirstItem))))
					       (list (first (second (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 1 (nth 1 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))))
			(if (> (first (second (nth k FirstItem)))
			       (first (second (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 1 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 1 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))))))
		  
		
				       ;count true pf's
		    
		    (if (or (equal (second (second (nth k FirstItem)))
				   (second (second (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (second (second (nth k FirstItem))))
					       (list (second (second (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 2 (nth 1 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))))
			(if (> (second (second (nth k FirstItem)))
			       (second (second (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 2 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 2 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))))))

		    ;true harmonic mean
		    
		    (if (or (equal (third (second (nth k FirstItem)))
				   (third (second (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (third (second (nth k FirstItem))))
					       (list (third (second (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 3 (nth 1 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))))
			(if (> (third (second (nth k FirstItem)))
			       (third (second (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 3 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 3 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))))))



	    ;count false pd's
		    (if (or (equal (first (third (nth k FirstItem)))
				   (first (third (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (first (third (nth k FirstItem))))
					       (list (first (third (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 1 (nth 2 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))))
			(if (> (first (third (nth k FirstItem)))
			       (first (third (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 1 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 1 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))))))

	    ;count false pf's
		    (if (or (equal (second (third (nth k FirstItem)))
				   (second (third (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (second (third (nth k FirstItem))))
					       (list (second (third (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 2 (nth 2 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))))
			(if (> (second (third (nth k FirstItem)))
			       (second (third (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 2 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 2 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))))))

	    ;count false harmonic mean
		    (if (or (equal (third (third (nth k FirstItem)))
				   (third (third (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (third (third (nth k FirstItem))))
					       (list (third (third (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 3 (nth 2 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))))
			(if (> (third (third (nth k FirstItem)))
			       (third (third (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 3 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 3 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie)))))))) 
		    )

		  (setf (nth 3 (nth 1 (nth 1 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 1 (nth 1 (nth i WinLossTie))))
								    (nth 1 (nth 1 (nth 1 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 1 (nth 1 (nth i WinLossTie))))
								     (nth 1 (nth 1 (nth 1 (nth i WinLossTie))))
								     (nth 2 (nth 1 (nth 1 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 1 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 2 (nth 1 (nth i WinLossTie))))
								    (nth 1 (nth 2 (nth 1 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 2 (nth 1 (nth i WinLossTie))))
								     (nth 1 (nth 2 (nth 1 (nth i WinLossTie))))
								     (nth 2 (nth 2 (nth 1 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 1 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 3 (nth 1 (nth i WinLossTie))))
								    (nth 1 (nth 3 (nth 1 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 3 (nth 1 (nth i WinLossTie))))
								     (nth 1 (nth 3 (nth 1 (nth i WinLossTie))))
								     (nth 2 (nth 3 (nth 1 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 1 (nth 2 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 1 (nth 2 (nth i WinLossTie))))
								    (nth 1 (nth 1 (nth 2 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 1 (nth 2 (nth i WinLossTie))))
								     (nth 1 (nth 1 (nth 2 (nth i WinLossTie))))
								     (nth 2 (nth 1 (nth 2 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 2 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 2 (nth 2 (nth i WinLossTie))))
								    (nth 1 (nth 2 (nth 2 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 2 (nth 2 (nth i WinLossTie))))
								     (nth 1 (nth 2 (nth 2 (nth i WinLossTie))))
								     (nth 2 (nth 2 (nth 2 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 2 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 3 (nth 2 (nth i WinLossTie))))
								    (nth 1 (nth 3 (nth 2 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 3 (nth 2 (nth i WinLossTie))))
								     (nth 1 (nth 3 (nth 2 (nth i WinLossTie))))
								     (nth 2 (nth 3 (nth 2 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie)))))))

		  )))
	    (setf WinLossTieResult WinLossTie)
	    ))
      (writeResults (concatenate 'string (symbol-name set) "RepeatResults.txt") WinLossTieResult)
      (print "FinalList")
      (print FinalList)
      (print "")
      (print "TimeList")
      (print TimeList)
))))

(defun writeResults (filename wlt)
  (let* ((FileStream (open filename :direction :output :if-exists :append :if-does-not-exist :create)))
    (print "writing header")
    (format FileStream "~Tcluster~Tclass~|TPDWins~TPDLoss~TPDTies~TPDwinloss|~TPFWins~TPFLosses~TPFTies~TPFwinloss|~THMWins~THMLosses~THMTies~THMwinloss~%")
    (dolist (Item wlt)
      (print "writing line")
      (print Item)
      (format FileStream "~A~T~A~T|~A~T~A~T~A~T~A|~T~A~T~A~T~A~T~A~T|~A~T~A~T~A~T~A~%" 
					;print cluster
	      (first Item)
					;print class
	      (first (second Item))
					;print pd
	      (first (second (second Item)))
	      (second (second (second Item)))
	      (third (second (second Item)))
	      (fourth (second (second Item)))
					;print pf
	      (first (third (second Item)))
	      (second (third (second Item)))
	      (third (third (second Item)))
	      (fourth (third (second Item)))
					;print hm
	      (first (fourth (second Item)))
	      (second (fourth (second Item)))
	      (third (fourth (second Item)))
	      (fourth (fourth (second Item))))
      (format FileStream "~A~T~A|~T~A~T~A~T~A~T~A|~T~A~T~A~T~A~T~A|~T~A~T~A~T~A~T~A~%" 
					;print cluster
	      (first Item) 
					;print class
	      (first (third Item))
					;print pd
	      (first (second (third Item)))
	      (second (second (third Item)))
	      (third (second (third Item)))
	      (fourth (second (third Item)))
					;print pf
	      (first (third (third Item)))
	      (second (third (third Item)))
	      (third (third (third Item)))
	      (fourth (third (third Item)))
					;print hm
	      (first (fourth (third Item)))
	      (second (fourth (third Item)))
	      (third (fourth (third Item)))
	      (fourth (fourth (third Item)))))

    (close FileStream)))

(defun leave-one-out-defect-tests (&key (datasets *DEFECT-DATASETS*) (distance-func 'cosine-similarity) (repeat 20))
  (let ((sets (copy-list datasets)))

    (dolist (set sets)
      (let ((data (table-egs (funcall set)))
	    FinalList
	    WinLossTieResult
	    TimeList)

	(print set)

	;;Compass with plain stopping rule
	(print "compass-vanilla")
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length MixedData)) 
		(let* ((want (first (last (nth i MixedData))))
		       (got (compass-defect-plain (nth i MixedData) MixedData 1.1 1.1 :distance-func distance-func)))
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
			 tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "Running Compass-1up")

	;;Compass with 1up stopping rule
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (compass-defect-1up (nth i MixedData) MixedData 1.1 1.1 :distance-func distance-func)))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 1")
	
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-defect 1 (nth i MixedData) MixedData)))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "Running k-means 2")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-defect 2 (nth i MixedData) MixedData)))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 4")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-defect 4 (nth i MixedData) MixedData)))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 8")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-defect 8 (nth i MixedData) MixedData)))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running k-means 16")
	
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-defect 16 (nth i MixedData) MixedData)))
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
		  tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running bisect k-means 4")

	(let* (tmp
	      (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-bisecting-defect 4 (nth i MixedData) MixedData)))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))

	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running bisect k-means 6")
	
	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-bisecting-defect 6 (nth i MixedData) MixedData)))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running bisect k-means 8")

	(let* (tmp
	       (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (k=?-bisecting-defect 8 (nth i MixedData) MixedData)))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(print "running best-k")
	
	(let* (tmp
	      (tmpTime (list (time-in-seconds))))
	  (dotimes (n repeat)
	    (print n)
	    (let* ((true-grid (list 0 0 0 0))
		   (false-grid (list 0 0 0 0))
		   (MixedData (shuffle data)))
	      (dotimes (i (length data))
		(let* ((want (first (last (nth i MixedData))))
		       (got (best-k-defect (nth i MixedData) MixedData)))
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
		    tmp))
	    (push (time-in-seconds) tmpTime))
	  (push tmp FinalList)
	  (push (reverse tmpTime) TimeList))

	(setf FinalList (reverse FinalList)) 
	(setf TimeList (reverse TimeList))

	(let* ((WinLossTie (make-list (length FinalList))))
	  (dotimes (Item (length WinLossTie))
					;(nth i WinLossTie) for Cluster
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
	    (setf (first (nth num WinLossTie)) (first (first (nth num FinalList)))))

	  ;Each time we loop, we remove the first so we don't double count stats.
	  (dotimes (i (- (length FinalList) 1))
	    (let* ((RestList (nthcdr (+ i 1) FinalList))
		   (FirstItem (nth i FinalList)))
	      (dotimes (j (length RestList))
		(let* ((SecondItem (nth j RestList)))
		  (dotimes (k (length FirstItem))
		    ;count true pd
		   
		    (if (or (equal (first (second (nth k FirstItem)))
				   (first (second (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (first (second (nth k FirstItem))))
					       (list (first (second (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 1 (nth 1 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))))
			(if (> (first (second (nth k FirstItem)))
			       (first (second (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 1 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 1 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))))))
		  
		
				       ;count true pf's
		    
		    (if (or (equal (second (second (nth k FirstItem)))
				   (second (second (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (second (second (nth k FirstItem))))
					       (list (second (second (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 2 (nth 1 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))))
			(if (> (second (second (nth k FirstItem)))
			       (second (second (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 2 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 2 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))))))

		    ;true harmonic mean
		    
		    (if (or (equal (third (second (nth k FirstItem)))
				   (third (second (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (third (second (nth k FirstItem))))
					       (list (third (second (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 3 (nth 1 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))))
			(if (> (third (second (nth k FirstItem)))
			       (third (second (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 3 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 3 (nth 1 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))))))



	    ;count false pd's
		    (if (or (equal (first (third (nth k FirstItem)))
				   (first (third (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (first (third (nth k FirstItem))))
					       (list (first (third (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 1 (nth 2 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))))
			(if (> (first (third (nth k FirstItem)))
			       (first (third (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 1 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 1 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))))))

	    ;count false pf's
		    (if (or (equal (second (third (nth k FirstItem)))
				   (second (third (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (second (third (nth k FirstItem))))
					       (list (second (third (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 2 (nth 2 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))))
			(if (> (second (third (nth k FirstItem)))
			       (second (third (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 2 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 2 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))))))

	    ;count false harmonic mean
		    (if (or (equal (third (third (nth k FirstItem)))
				   (third (third (nth k SecondItem))))
			    (equal 1 (wilcoxon (list (third (third (nth k FirstItem))))
					       (list (third (third (nth k SecondItem)))))))
			(progn
			  (incf (nth 2 (nth 3 (nth 2 (nth i WinLossTie)))))
			  (incf (nth 2 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))))
			(if (> (third (third (nth k FirstItem)))
			       (third (third (nth k SecondItem))))
			    (progn
			      (incf (nth 0 (nth 3 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 1 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))))
			    (progn
			      (incf (nth 1 (nth 3 (nth 2 (nth i WinLossTie)))))
			      (incf (nth 0 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie)))))))) 
		    )

		  (setf (nth 3 (nth 1 (nth 1 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 1 (nth 1 (nth i WinLossTie))))
								    (nth 1 (nth 1 (nth 1 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 1 (nth 1 (nth i WinLossTie))))
								     (nth 1 (nth 1 (nth 1 (nth i WinLossTie))))
								     (nth 2 (nth 1 (nth 1 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 1 (nth 1 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 1 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 2 (nth 1 (nth i WinLossTie))))
								    (nth 1 (nth 2 (nth 1 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 2 (nth 1 (nth i WinLossTie))))
								     (nth 1 (nth 2 (nth 1 (nth i WinLossTie))))
								     (nth 2 (nth 2 (nth 1 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 2 (nth 1 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 1 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 3 (nth 1 (nth i WinLossTie))))
								    (nth 1 (nth 3 (nth 1 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 3 (nth 1 (nth i WinLossTie))))
								     (nth 1 (nth 3 (nth 1 (nth i WinLossTie))))
								     (nth 2 (nth 3 (nth 1 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 3 (nth 1 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 1 (nth 2 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 1 (nth 2 (nth i WinLossTie))))
								    (nth 1 (nth 1 (nth 2 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 1 (nth 2 (nth i WinLossTie))))
								     (nth 1 (nth 1 (nth 2 (nth i WinLossTie))))
								     (nth 2 (nth 1 (nth 2 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 1 (nth 2 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 2 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 2 (nth 2 (nth i WinLossTie))))
								    (nth 1 (nth 2 (nth 2 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 2 (nth 2 (nth i WinLossTie))))
								     (nth 1 (nth 2 (nth 2 (nth i WinLossTie))))
								     (nth 2 (nth 2 (nth 2 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 2 (nth 2 (nth (+ i j 1) WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 2 (nth i WinLossTie)))) (win-loss-percent
								    (nth 0 (nth 3 (nth 2 (nth i WinLossTie))))
								    (nth 1 (nth 3 (nth 2 (nth i WinLossTie))))
								    (+
								     (nth 0 (nth 3 (nth 2 (nth i WinLossTie))))
								     (nth 1 (nth 3 (nth 2 (nth i WinLossTie))))
								     (nth 2 (nth 3 (nth 2 (nth i WinLossTie)))))))

		  (setf (nth 3 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie)))) (win-loss-percent
									    (nth 0 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (nth 1 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									    (+
									     (nth 0 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 1 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie))))
									     (nth 2 (nth 3 (nth 2 (nth (+ i j 1) WinLossTie)))))))

		  )))
	    (setf WinLossTieResult WinLossTie)
	    ))
      (writeResults (concatenate 'string (symbol-name set) "Leave1Out.txt") WinLossTieResult)
      (print "")
      (print FinalList)
      (print TimeList)
))))

(defun time-in-seconds ()
  (/ 
   (get-internal-real-time)
   internal-time-units-per-second))

(defun win-loss-percent(win loss total)
  (*
   100
   (/ (- win loss)
      total)))

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

