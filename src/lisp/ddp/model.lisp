(defstruct ddp-model
  o-weight
  o-attainment
  o-at-risk-prop
  r-apl
  r-likelihood
  m-cost
  ro-impacts
  mr-effects
  )
  
(defstruct mr-effect m r effect)

(defstruct ro-impact r o impact)

(defun model (this-model mitigations)

  ; Calculate likelihoods
  (let (likelihoods)
    (dotimes (i (length (ddp-model-r-apl this-model)))
      (let ((mr-effects (applicable-mr-effects i this-model))
	    (likelihood 1))
	(dolist (effect mr-effects)
	  (setf likelihood (* likelihood 
			      (- 1 (* (nth (mr-effect-m effect) mitigations)
				      (mr-effect-effect effect))))))
	(push (* likelihood (nth i (ddp-model-r-apl this-model))) likelihoods)))
    (setf (ddp-model-r-likelihood this-model) (reverse likelihoods)))

  ; Calculate at-risk-props
  (let (at-risk-props)
    (dotimes (i (length (ddp-model-o-weight this-model)))
      (let ((at-risk-prop 0))
	(dolist (likelihood (ddp-model-r-likelihood this-model))
	  (setf at-risk-prop (+ at-risk-prop 
				(* likelihood (applicable-ro-impact i this-model))))
	  (push at-risk-prop at-risk-props))))
    (setf (ddp-model-o-at-risk-prop this-model) (reverse at-risk-props)))

  ; Calculate attainments
  (let (o-attainments)
    (dotimes (i (length (ddp-model-o-weight this-model)))
      (push (* (nth i (ddp-model-o-weight this-model))
	       (min 1 (nth i (ddp-model-o-at-risk-prop this-model))))
	    o-attainments))
    (setf (ddp-model-o-attainment this-model) (reverse o-attainments)))
  
  ; Calculate total attainment and cost
  (let ((cost-total 0) (att-total 0))
    (dotimes (i (length (ddp-model-o-attainment this-model)))
      (setf att-total (+ att-total (nth i (ddp-model-o-attainment this-model)))))
    (dotimes (i (length mitigations))
      (setf cost-total (+ cost-total (* (nth i mitigations)
					(nth i (ddp-model-m-cost this-model))))))
    (list att-total cost-total)))

(defun applicable-mr-effects (n this-model)
  (let ((all-mr-effects (ddp-model-mr-effects this-model))
	(applicable-list))
    (dolist (this-mr-effect all-mr-effects)
      (if (= (mr-effect-r this-mr-effect) n)
	  (push this-mr-effect applicable-list)))
    (reverse applicable-list)))

(defun applicable-ro-impact (n this-model)
  (let ((all-ro-impacts (ddp-model-ro-impacts this-model))
	(applicable-ro-impact))
    (dolist (this-impact all-ro-impacts)
      (if (= n (ro-impact-o this-impact))
	  (setf applicable-ro-impact this-impact)))
    applicable-ro-impact))