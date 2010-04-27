(defun variance (these)
  (let ((n (make-normal-from-list
	    (mapcar #'first (mapcar #'last these)))))
    (stdev n)))

;(defun stdev (these)
;  (let ((m (mean these))
;	(sigma-squared 0))
;    (dolist (this these)
;      (setf sigma-squared (+ sigma-squared (expt (- this mean) 2))))
;    (setf sigma-squared (/ sigma-squared (1- (length these))))
;    sigma-squared))

;(defun mean (these)
;  (let ((s (sum these)))
;    (/ s (length these))))

;(defun sum (l)
;  (let ((s 0))
;    (dolist (n l)
;      (setf s (+ s n)))
;    s))
