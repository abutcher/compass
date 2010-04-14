(defstruct ddp-model
  o-weight
  o-attainment
  o-at-risk-prop
  r-apl
  r-likelihood
  m-cost
  ro-impacts
  mr-effects
  base-cost
  )
  
(defstruct mr-effect m r effect)

(defstruct ro-impact r o impact)

(defun model (this-model mitigation)
  "Runs a mitigation through the model at hand and returns total
   attainment and cost for the mitigation"
  ; Calculate likelihoods
  (let (likelihoods)
    (dotimes (i (length (ddp-model-r-apl this-model)))
      (let ((mr-effects (applicable-mr-effects i this-model)))
	(if (null mr-effects)
	    (push (nth i (ddp-model-r-apl this-model)) likelihoods)
	    (let ((likelihood (nth i (ddp-model-r-apl this-model))))
	      (dolist (effect mr-effects)
		(setf likelihood (* likelihood 
				    (- 1 (* (nth (mr-effect-m effect) mitigation)
					    (mr-effect-effect effect))))))
	      (push likelihood likelihoods)))))
    (format t "There are ~A likelihoods~%" (length likelihoods))
    (format t "LIKELIHOODS: ~A~%" (reverse likelihoods))
    (setf (ddp-model-r-likelihood this-model) (reverse likelihoods)))
  
   ; Calculate at-risk-props
  (let (at-risk-props)
    (dotimes (i (length (ddp-model-o-weight this-model)))
      (let ((at-risk-prop 0))
	(dolist (ro-impact (applicable-ro-impacts i this-model))
	  (setf at-risk-prop
		(+ at-risk-prop 
		   (* (nth (ro-impact-r ro-impact) 
			   (ddp-model-r-likelihood this-model))
		      (ro-impact-impact ro-impact)))))
	(push at-risk-prop at-risk-props)))
    (format t "There are ~A at-risk-props~%" (length at-risk-props))
    (format t "AT-RISK-PROPS: ~A" (reverse at-risk-props))
    (setf (ddp-model-o-at-risk-prop this-model) (reverse at-risk-props)))

  ; Calculate attainments
  (let (o-attainments)
    (dotimes (i (length (ddp-model-o-weight this-model)))
      (push (* (nth i (ddp-model-o-weight this-model))
	       (- 1(min 1 (nth i (ddp-model-o-at-risk-prop this-model)))))
	    o-attainments))
    (setf (ddp-model-o-attainment this-model) (reverse o-attainments)))
  
  ; Calculate total attainment and cost
  (let ((cost-total (ddp-model-base-cost this-model)) (att-total 0))
    (dotimes (i (length (ddp-model-o-attainment this-model)))
      (setf att-total (+ att-total (nth i (ddp-model-o-attainment this-model)))))
    (dotimes (i (length mitigation))
      (setf cost-total (+ cost-total (* (nth i mitigation)
					(nth i (ddp-model-m-cost this-model))))))
    (list att-total cost-total)))

(defun test-model ()
  (let ((mit '(1 1 1 0 0 1 0 1 1 0 1 1 1 1 1 0 1 1 1 0 1 1 0 1 1 1 0 1 0 1 1)))
    (format t "MITIGATION: ~A~%" mit)
    (model (make-model-2) mit)))

"(model (make-model-2) (car (generator :n 1 :s 31)))"

(defun applicable-mr-effects (n this-model)
  "Give me the mr-effects that match the risk at hand"
  (let ((all-mr-effects (ddp-model-mr-effects this-model))
	(applicable-list))
    (dolist (this-mr-effect all-mr-effects)
      (if (= n (mr-effect-r this-mr-effect))
	  (push this-mr-effect applicable-list)))
    (reverse applicable-list)))

(defun applicable-ro-impacts (n this-model)
  "Give me the ro-impacts that match the risk at hand"
  (let ((all-ro-impacts (ddp-model-ro-impacts this-model))
	(applicable-ro-impacts))
    (dolist (this-impact all-ro-impacts)
      (if (= n (ro-impact-o this-impact))
	  (push this-impact applicable-ro-impacts)))
    applicable-ro-impacts))