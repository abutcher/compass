(defun table-egs (tbl)
  (mapcar #'eg-features (table-all tbl)))

(defun sane-numbers (data &optional (cautions (make-caution)) (sortp t))
  (let (numbers)
    (dolist (datum data (if sortp (sort (copy-list numbers) #'<) numbers))
      (unless (unknownp datum)
        (if (ok (numberp datum) cautions "~a not a number" datum)
            (push datum numbers))))))

(defun nbins1 (data n &optional (cautions (make-caution)))
  (let* ((numbers (sane-numbers data cautions))
         (min     (first numbers))
         (max     (first (last numbers)))
         (width   (/ (- max min) n)))
    (labels ((datum (cell)
               (cond ((unknownp cell) cell)
                     ((numberp cell)  (discretize cell))
                     (t               #\?)))
	     (between! (min max n)
	       (cond ((< n min) min)
		     ((> n max) max)
		     (t         n)))
	     (discretize (m)
		 (between! 0 (1- n) (round (/ (- m min) width)))))
      (mapcar #'datum data))))

(defun nbins (tbl &key (n 5) (cautions (make-caution)))
  (let* ((name    (table-name tbl))
	 (headers (table-columns tbl))
	 (columns (mapcar #'discrete! headers))
	 (egs     (discretize-egs (table-columns tbl)
				  (table-egs tbl)
				  #'(lambda (col) (nbins1 col n cautions)))))
    (data :name name :columns columns :egs egs)))

(defmethod discrete! ((header discrete))
  (header-name header))

(defmethod discrete! ((header numeric))
  (intern ; string ==> symbol 
   (remove #\$ 
	   (symbol-name ; symbol ==> string
	    (header-name header)))))

(defun discretize-egs (headers egs how)
  (transpose
   (mapcar #'(lambda (col)
	       (discretize-one-column (first col) (rest col) how))
	   (transpose
	    (cons headers egs)))))

(defmethod discretize-one-column ((header discrete) egs how)
  egs)

(defmethod discretize-one-column ((header numeric) egs how)
  (funcall how egs))
