(defun demo-6 ()
  "Generate n k-means clusters from table data."
  (print (meat-processor 8 (k-means 8 (table-egs (kemerer))))))
