;;;; the data function returns a table containing some egs

(defun data (&key name columns egs  (klass -1))
  (let* (tmp-egs
	 (tbl
          (make-table
           :name name
           :columns (columns-new
                     columns
                     (class-index klass (length columns))))))
    (setf (table-class tbl) 
	  (class-index klass (table-width tbl)))
    (dolist (eg egs)
      (if (setf eg (datums eg tbl))
	  (push eg tmp-egs)))
    tbl))

(defun datums (one tbl)
  (let ((oops (table-cautions tbl)))
    (when
	(ok (= (table-width tbl) (length one))
	    oops "~a wrong size" one)
      (mapc #'(lambda(column datum)
		(datum column datum oops))
	    (table-columns tbl) 
	    one)
      (push (make-eg :class (isa one tbl) :features one)
	    (table-all tbl)))))

(defmethod datum ((column discrete) datum oops)
  "things to do when reading a descrete datum"
  (declare (ignore  oops))
  (unless (member datum (discrete-uniques column))
    (push datum (discrete-uniques column)))
  t)

(defmethod datum ((column numeric) datum oops)
  "things to do when reading a numeric datum"
  (ok (numberp datum) oops"~a is not a number" datum)
  t)

(defun class-index (klass width)
  (if (< klass 0) (+ klass width) klass))

(defun make-data1 ()
  (data
   :name   'weather
   :columns '(forecast temp humidty $windy play)
   :egs    '((sunny    hot  high   FALSE no) 
             (sunny    hot  high   TRUE  yes)
             (sunny    hot  high         yes)
             )))

