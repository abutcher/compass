(defun demo-9 ()
  "Run a win-loss-tie for ABE0 vs Compass."
  (run-tests '(cocomo81) :distance-func 'euclidean-distance))
