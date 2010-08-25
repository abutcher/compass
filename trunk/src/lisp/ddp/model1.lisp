(defun make-model-1 ()
  (make-ddp-model 
   :m-cost (list 11 22)
   :r-apl (list 1 1)
   :o-weight (list 1 2 3)
   :mr-effects (list
		(make-mr-effect :m 0 :r 0 :effect 0.9)
		(make-mr-effect :m 0 :r 1 :effect 0.3)
		(make-mr-effect :m 1 :r 0 :effect 0.4)
		)
   :ro-impacts (list
		(make-ro-impact :r 0 :o 0 :impact 0.1)
		(make-ro-impact :r 0 :o 1 :impact 0.3)
		(make-ro-impact :r 1 :o 0 :impact 0.2)
		)
   :base-cost 0
   )
)