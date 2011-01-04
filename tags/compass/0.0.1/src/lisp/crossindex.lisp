(defun cross-index (tbl)
  (let* ((x       (xindex-new tbl))
         (uniques (xindex-uniques x))
         (class-counts       (xindex-class-counts x)))
    (dotimes (i (table-width tbl))
      (setf (aref (xindex-ranges x) i) (make-counted)))
    (dotimes (j (xindex-ns x) x)
      (cross-index1 j (aref (xindex-all x) j)  x uniques class-counts))))

(defun cross-index1 (j eg  x uniques class-counts) 
  (let* ((i -1)
         (class (eg-class eg)))
    (incf (gethash class class-counts 0))
    (dolist (range (eg-features eg)) 
      (incf  i)
      (unless (unknownp range)
        (let* ((xkey  `(,class ,i ,range))
               (ukey  `(,i ,range))
               (counted  (gethash xkey (xindex-counts x)
                                  (make-counted))))      
          (setf (gethash xkey (xindex-counts x)) 
                (counted+  j counted))
          (when (= 1 (incf (gethash ukey uniques 0)))
            (setf (aref (xindex-ranges x) i) 
                  (counted+ range
                            (aref (xindex-ranges x) i)))))))))

;(deftest test-index ()
;  (let ((tmp (cross-index (make-data2))))
;    (check
;      (samep tmp "
;#(XINDEX
; :TABLE <table>
;  :ALL #(#S(EG :FEATURES (SUNNY YES) :CLASS YES)
;         #S(EG :FEATURES (RAINY YES) :CLASS YES)
;         #S(EG :FEATURES (SUNNY YES) :CLASS YES))
;  :N 3
;  :RANGES #(#S(COUNTED :ALL (RAINY SUNNY) :N 2 :SORTED NIL)
;            #S(COUNTED :ALL (YES) :N 1 :SORTED NIL))
;  :UNIQUES
;     (0 RAINY) = 1
;     (0 SUNNY) = 2
;     (1 YES) = 3
;   :CLASS-COUNTS
;     YES = 3
;   :COUNTS
;     (YES 0 RAINY) = #S(COUNTED :ALL (1) :N 1 :SORTED NIL)
;     (YES 0 SUNNY) = #S(COUNTED :ALL (2 0) :N 2 :SORTED NIL)
;     (YES 1 YES) = #S(COUNTED :ALL (2 1 0) :N 3 :SORTED NIL)
; )"))))

(defun make-data2 ()
  (data
   :name    'weather
   :columns '($forecast $play)
   :egs     '((sunny   yes)
              (rainy   yes)
              (sunny   yes))))
