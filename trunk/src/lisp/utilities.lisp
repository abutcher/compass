(defun rmnth (n lst)
  (remove (nth n lst) lst :start n :count 1))
