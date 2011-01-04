(defun era (tbl-egs &key (n 10))
  (let (tbl-eg-list tbl-list (counter 0))
    (loop while (< counter (length tbl-egs)) do
	 (when (< (+ counter n) (length tbl-egs))
	   (push (subseq tbl-egs counter (+ counter n)) tbl-eg-list))
	 (when (> (+ counter n) (length tbl-egs))
	   (push (subseq tbl-egs counter) tbl-eg-list))
	 (setf counter (+ counter n)))
    (reverse tbl-eg-list))) ;; <- List of lists

;; Instead of transferring the lists into their own compacted tables,
;; I'd rather have lists.

;    (dolist (new-egs tbl-eg-list)
;      (push (data
;	     :name (table-name tbl)
;	     :columns (columns-header (table-columns tbl))
;	     :klass (table-class tbl)
;	     :egs new-egs)
;	    tbl-list))
;    tbl-list))