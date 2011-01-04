(defun make-model-3 ()
  (make-ddp-model
   :o-weight (list 1 2 3)
   :m-cost (list 11 22 0)
   :r-apl (list 0.4 1)
   :r-aggrevated-impact (list 1 1)
   :ro-impacts (list 
		(make-ro-impact :r 0 :o 0 :impact 0.1)
		(make-ro-impact :r 0 :o 1 :impact 0.3)
		(make-ro-impact :r 1 :o 0 :impact 0.2)
		)
   :mr-effects (list
		(make-mr-effect :m 0 :r 0 :effect -0.2)
		(make-mr-effect :m 1 :r 0 :effect 0.1)
		(make-mr-effect :m 2 :r 0 :effect -1.7)
		(make-mr-effect :m 0 :r 1 :effect 0.3)
		)
  :base-cost 0
  )
)