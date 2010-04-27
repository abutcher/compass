(defun variance (these)
  (let ((n (make-normal-from-list these)))
    (stdev n)))

; For effort instances pass (mapcar #'first (mapcar #'last these))