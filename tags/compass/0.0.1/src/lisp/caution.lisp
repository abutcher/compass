(defstruct (caution (:print-function caution-print))
  all (patience 20) killed)

(defun caution-print (c s depth)
  (declare (ignore depth))
  (format s "#(CAUTION :ALL ~a :PATIENCE ~a)"
          (caution-all c)
          (caution-patience c)))

(defun ok (test cautions format-str &rest args)
  (or test
      (let ((message (apply #'format `(nil ,format-str ,@args))))
        (push message (caution-all cautions))
        (decf (caution-patience cautions)) 
        (format t "% ~a~%" message)
        (when (< (caution-patience cautions) 0)
          (setf (caution-killed cautions) t)
          (error "too many warnings"))
        nil)))

(defun die (cautions format-str &rest args)
  (apply #'ok  `(nil ,cautions ,format-str ,@args))
  (setf (caution-killed cautions) t)
  (error "gasp... wheeze... rosebud... (thud)"))
  
(defun test-out-of-patience ()
  "can't be a defftest cause it crashes on too many errros"
  (let ((c (make-caution :patience 5)))
    (dotimes (i 6) ; patience + 1
      (ok (= 1 2) c "bad ~a ~a" 1 2))))

