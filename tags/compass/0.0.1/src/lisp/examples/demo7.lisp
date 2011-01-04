(defun demo-7 ()
  "Create a compass tree from table data."
  (print (compass (table-egs (cocomo81o)) :distance-func 'euclidean-distance)))
