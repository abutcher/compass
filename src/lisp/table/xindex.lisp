;;;; xindex runs over the data. populates the "counts" of each column header.
;; genrate the indexes

(defun xindex (tbl)
  (unless (table-indexed tbl)
    (setf (table-indexed tbl) t)
    (dolist (row (table-all tbl) tbl) ; for al rows do ...
      (xindex1 (eg-class row) 
	       (eg-features row) 
	       (table-columns tbl))))
  tbl)

(defun xindex1 (class datums columns) ; for all datum in a row do ...
  (mapc #'(lambda (column datum) 
	    (unless (ignorep datum)
	      (xindex-datum column class datum)))
	columns
	datums))

(defmethod xindex-datum ((column discrete) class  datum)
  (let* ((key `(,class ,datum))
	 (hash (header-f column)))
    (incf (gethash key hash 0))))

(defmethod xindex-datum ((column numeric) class  datum)
  (let* ((key      class)
	 (hash     (header-f column))
	 (counter  (gethash  key hash (make-normal))))
    (setf (gethash key hash) counter) ; make sure the hash has the counter
    (add counter datum)))

(defun make-data2 ()
  (data
   :name     'weather
   :columns  '(forecast temp humidty windy play)
   :egs     '((sunny    hot  high   FALSE no) 
              (sunny    hot  high   TRUE  no)
              (rainy    cool normal TRUE  no)
              (rainy    mild high   TRUE   no)
              (sunny    mild high   FALSE no)
              (overcast cool normal TRUE  yes)
              (overcast hot  high   FALSE yes)
              (rainy    mild high   FALSE yes)
              (rainy    cool normal FALSE yes)
              (sunny    cool normal FALSE yes)
              (rainy    mild normal FALSE yes)
              (sunny    mild normal TRUE  yes)
              (overcast mild high   TRUE  yes)
              (overcast hot  normal FALSE yes)
)))

(defun make-data3 ()
  (data
   :name   'weather
   :columns '(forecast temp humidty $wind play)
   :egs    '((sunny    hot  high   20  no) 
	     (sunny    hot  high   10 no) 
	     (sunny    hot  high   30  no) 
             (sunny    hot  high   20.2  yes)
             (sunny    hot  high   20.1  yes)
             (sunny    hot  high   20.7  yes)
             )))

(defun make-some-weather-data ()
  (data
   :name   'weather
   :columns '(forecast $temp $humidty wind play)
   :egs    '((sunny 85 85 FALSE no)
	     (sunny 80 90 TRUE no)
	     (overcast 83 86 FALSE yes)
	     (rainy 70 96 FALSE yes)
	     (rainy 68 80 FALSE yes)
	     (rainy 65 70 TRUE no)
	     (overcast 64 65 TRUE yes)
	     (sunny 72 95 FALSE no)
	     (sunny 69 70 FALSE yes)
	     (rainy 75 80 FALSE yes)
	     (sunny 75 70 TRUE yes)
	     (overcast 72 90 TRUE yes)
	     (overcast 81 75 FALSE yes)
	     (rainy 71 91 TRUE no))))

(defun f (tbl &optional class index range)
  (cond ((null class) 	 ; return number of instances 
	 (length (table-all tbl)))
	((null index)	 ; return number of a certain class 
	 (f1 (nth (table-class tbl) (table-columns tbl))
	     class
	     class))
	(t      	 ; return frequency of a range in a class
	 (f1 (nth index (table-columns tbl))
	     class
	     range))))

(defmethod f1 ((column discrete) class range)
  (gethash `(,class ,range) (header-f  column) 0))
