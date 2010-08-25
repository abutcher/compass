(defun randomizer (tbl)
  "Randomly re-orders table rows and returns a table"
  (data
   :name (table-name tbl)
   :columns (car (transpose (transform tbl)))
   :klass (table-class tbl)
   :egs (shuffle (mapcar #'eg-features (egs tbl)))
   ))

;(deftest test-random ()
;  (randomizer (weather-numerics)))