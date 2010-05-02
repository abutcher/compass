(defun variance (these)
  (let* ((classes (mapcar #'first (mapcar #'last these)))
	(n (make-normal-from-list classes)))
    (stdev n)))
    

;(let ((n (make-normal-from-list these)))
; (stdev n)))
; For effort instances pass (mapcar #'first (mapcar #'last these))