(defun separate (these &key (distance-func 'cosine-similarity))
  "Turn one list into two lists using euclidean distance and farthest
   neighbors"
  (let (this that this-group that-group left-right)
    ; Pick one at random, this
    (setf this (random-element these))
    ; Remove this from these
    (setf these (remove this these))
    ; Find the farthest thing from this, that
    (setf that (farthest-from this these :distance-func distance-func))
    ; Remove that from these
    (setf these (remove that these))
    ; Put this back
    (push this these)
    ; Now this is the farthest thing from that
    (setf this (farthest-from that these :distance-func distance-func))
    ; Take the new this out
    (setf these (remove this these))

    ; Put them in their group
    (push this this-group)
    (push that that-group)

    (setf left-right (funcall distance-func this that))

    ; Using lemma 1 from elkan03 determine which group each element
    ; belongs to
;    (dolist (element these)
;      (let ((d-from-that (funcall distance-func element that)))
;	(if (>= left-right (* 2 d-from-that))
;	    (push element that-group)
;	    (push element this-group))))

    (dolist (element these)
      (let ((d-from-this (funcall distance-func element this))
	    (d-from-that (funcall distance-func element that)))
	(if (> d-from-this d-from-that)
	    (push element that-group)
	    (push element this-group))))
    
    ; Give em back
    (list (reverse this-group) (reverse that-group))))
