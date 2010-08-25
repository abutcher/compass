(defun generator (&key (n 50) (s 10) (l '(0 1)))
  "Give me 50 of those, size 10, from this list"
  (let (generated)
    (dotimes (i n)
      (let (new)
	(dotimes (j s)
	  (push (random-element l) new))
	(push new generated)))
    generated))

(defun random-element (l)
  (nth (my-random-int (length l)) l))