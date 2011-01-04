(defun transform (tbl)
  (transpose 
   (append 
    (list 
     (columns-header (table-columns tbl))) 
    (mapcar #'eg-features (egs tbl)))))