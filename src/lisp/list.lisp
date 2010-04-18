(defun shuffle (l)
  (dotimes (i (length l) l)
    (rotatef
     (elt l i)
     (elt l (my-random-int (length l))))))

(defun transpose (x)
   (apply #'mapcar (cons #'list x)))
