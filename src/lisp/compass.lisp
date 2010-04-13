(defstruct node
  rootp
  contents
  right
  left
  )

(defun compass (mitigations)
  
  )

(defun separate (these)
  (let ((this)(that)(this-group)(that-group))
    ; Pick one at random, this
    (setf this (random-element these))
    ; Remove this from these
    (setf these (remove this these))
    ; Find the farthest thing from this, that
    (setf that (farthest-from this these))
    ; Remove that from these
    (setf these (remove that these))
    ; Put this back
    (push this these)
    ; Now this is the farthest thing from that
    (setf this (farthest-from that these))

    ; Put them in their group
    (push this this-group)
    (push that that-group)

    ; Determine which group the elements of these belong to
    (dolist (element these)
      (let ((d-from-this (distance element this))
	    (d-from-that (distance element that)))
	(if (> d-from-this d-from-that)
	    (push element this-group)
	    (push element that-group))))

    ; Give em back
    (list this-group that-group)))

(defun farthest-from (this those)
  (let ((max-distance 0)(temporary))
    (dolist (that those)
      (let ((d (distance this that)))
	(if (> d max-distance)
	    (setf temporary that))))
    temporary))
  