(defun demo-8 ()
  "Prune high variance regions from a compass tree."
  (print (variance-prune
	  (compass (table-egs (finnish)) :distance-func 'euclidean-distance)
	  :alpha 1.1
	  :beta 1.1)))
