;;;; a table stores headers and examples

(defun isa (row tbl)  (nth (table-class tbl) row))

(defun table-width (tbl)
  (length (table-columns tbl)))

(defun table-copy (tbl &optional (new (copy-tree (table-all tbl))))
  (data :egs     new
        :name    (table-name tbl)
        :klass   (table-class tbl)
        :columns (columns-header (table-columns tbl))))

(defun klasses (tbl)
  (discrete-uniques (table-class-header tbl)))

(defun nklasses (tbl)
  (1+ (length (klasses tbl))))

(defun egs (tbl)
  (table-all tbl))

(defun negs (tbl)
  (1+ (length (table-all tbl))))

(defun table-class-header (tbl)
  (nth (table-class tbl) (table-columns tbl)))
