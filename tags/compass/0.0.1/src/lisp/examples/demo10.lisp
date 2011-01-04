(defun demo-10 ()
  "Output a file you can pass to graphviz to make a png image of the tree."
  (dot-compass  (compass (table-egs (maxwell))
			 :distance-func 'euclidean-distance)
		"maxwell.dot"))
