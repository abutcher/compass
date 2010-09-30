(defstruct point x y)

(defun auc-two-point (p1 p2)
  (let ((area-triangle
	 (* (* 0.5 (abs (- (point-y p1) (point-y p2))))
	    (- (point-x p2) (point-x p1))))
	(area-square
	 (* (max (point-y p1) (point-y p2)) (- (point-x p2) (point-x p1)))))
    (- area-square area-triangle)))

(defun auc (points)
  (let ((total-area 0))
    (dotimes (n (length points))
      (unless (= (1+ n) (length points))
	(setf total-area (+ total-area
			    (auc-two-point (nth n points) (nth (1+ n) points))))))
    (abs total-area)))

(defun tuples-to-points (tuples)
  (let (points)
    (dolist (tuple tuples)
      (push (make-point :x (first tuple) :y (second tuple)) points))
    (reverse points)))