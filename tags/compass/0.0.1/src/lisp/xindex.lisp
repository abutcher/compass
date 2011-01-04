(defstruct (xindex (:print-function xindex-print))
  table
  all
  (n 0)
  ranges
  (counts (make-hash-table :test #'equalp))
  (class-counts (make-hash-table))
  (uniques (make-hash-table :test #'equal)))

(defun xindex-print (x s depth)
  (declare (ignore depth))
  (labels ((show (x y) (showh x :before y :stream s :indent 5 :after "")))
    (format s
            "#(XINDEX~%~T:TABLE <table>~%~T :ALL ~a~%~T :N ~a~%~T :RANGES ~a~%~T" 
            (xindex-all x)
            (xindex-n x)
            (xindex-ranges x))
    (show (xindex-uniques      x)  " :UNIQUES
") (show (xindex-class-counts x) "   :CLASS-COUNTS
") (show (xindex-counts       x) "   :COUNTS
"  ))
  (format s "~T)"))

(defun xindex-new (tbl)
  (make-xindex
   :table    tbl
   :all     (make-array
             (table-height tbl)
             :initial-contents (table-rows tbl))
   :n       (table-height tbl)
   :ranges  (make-array
              (table-width tbl))))

