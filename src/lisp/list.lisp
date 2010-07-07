(defun shuffle (l)
  (dotimes (i (length l) l)
    (rotatef
     (elt l i)
     (elt l (my-random-int (length l))))))

(defun transpose (x)
   (apply #'mapcar (cons #'list x)))

(defmacro doitems ((one n list &optional out) &body body )
  `(let ((,n -1))
     (dolist (,one ,list ,out)  (incf ,n) ,@body)))

(defun squash (lists)
  (let (squashed)
    (dolist (cluster (meat-processor 5 (k-means 5 (table-egs (nasa93)))))
      (setf squashed (append cluster squashed)))
    squashed))