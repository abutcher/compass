(defstruct ddp-model
  o-weight
  o-attainment
  o-at-risk-prop
  r-apl
  r-aggrevated-impact
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
      (push (nth i (ddp-model-r-apl this-model)) likelihoods))
    
    (setf (ddp-model-r-likelihood this-model) (reverse likelihoods)))

  (unless (null (ddp-model-r-aggrevated-impact this-model))
    (let ((likelihoods (ddp-model-r-likelihood this-model))
	  negative-mr-effects)

      ; Collect any mr combinations with negative effects
      (let ((mr-effects (ddp-model-mr-effects this-model))
	    normal-mr-effects)
	(dolist (effect mr-effects)
	  (if (< (mr-effect-effect effect) 0)
	      (push effect negative-mr-effects)
	      (push effect normal-mr-effects)))
	(setf (ddp-model-mr-effects this-model) (reverse normal-mr-effects)))
  
      ; Initially mark any likelihoods with negative values
      (dotimes (i (length likelihoods))
	(let ((first-neg-mr-effect 
	       (first (applicable-mr-effects i negative-mr-effects))))
	  (unless (null first-neg-mr-effect)
	    (setf (nth i likelihoods) 
		  (min 1 (- (nth i likelihoods) 
			    (* (nth (mr-effect-m first-neg-mr-effect) mitigation)
			       (mr-effect-effect first-neg-mr-effect))))))))

      ; Now go through and mark any aggrevated impacts that exist
      (let ((aggrevated-impacts (ddp-model-r-aggrevated-impact this-model)))
	(dotimes (i (length aggrevated-impacts))
	  (let ((remaining-neg-mr-effects (cdr (applicable-mr-effects i negative-mr-effects))))
	    (unless (null remaining-neg-mr-effects)
	      (let ((aggrevated-impact (nth i aggrevated-impacts)))
		(dolist (effect remaining-neg-mr-effects)
		  (setf aggrevated-impact
			(* aggrevated-impact
			   (- 1 (* (nth (mr-effect-m effect) mitigation)
				   (+ 1 (mr-effect-effect effect)))))))
		(setf (nth i aggrevated-impacts) aggrevated-impact)))))
	(setf (ddp-model-r-aggrevated-impact this-model) aggrevated-impacts))

      ; Put the modified likelihoods back
      (setf (ddp-model-r-likelihood this-model) (reverse likelihoods))))
  
  ; Run a normal likelihood loop
  (let (likelihoods)
    (dotimes (i (length (ddp-model-r-apl this-model)))
      (let ((mr-effects (applicable-mr-effects i (ddp-model-mr-effects this-model))))
	(if (null mr-effects)
	    (push (nth i (ddp-model-r-apl this-model)) likelihoods)
	    (let ((likelihood (nth i (ddp-model-r-likelihood this-model))))
	      (dolist (effect mr-effects)
		(setf likelihood (* likelihood 
				    (- 1 (* (nth (mr-effect-m effect) mitigation)
					    (mr-effect-effect effect))))))
	      (push likelihood likelihoods)))))
    (setf (ddp-model-r-likelihood this-model) (reverse likelihoods)))

  ; Calculate at-risk-props 
  (let (at-risk-props)
    (dotimes (i (length (ddp-model-o-weight this-model)))
      (let ((at-risk-prop 0))
	(dolist (ro-impact (applicable-ro-impacts i (ddp-model-ro-impacts this-model)))
	  (if (null (ddp-model-r-aggrevated-impact this-model))
	      (setf at-risk-prop
		    (+ at-risk-prop 
		       (* (nth (ro-impact-r ro-impact) 
			       (ddp-model-r-likelihood this-model))
			  (ro-impact-impact ro-impact))))
	      ; If we have an aggrevated impact case, we'll enter that
	      ; into the product
	      (setf at-risk-prop
		    (+ at-risk-prop
		       (* (nth (ro-impact-r ro-impact)
			       (ddp-model-r-likelihood this-model))
			  (nth (ro-impact-r ro-impact)
			       (ddp-model-r-aggrevated-impact this-model))
			  (ro-impact-impact ro-impact))))))
	(push at-risk-prop at-risk-props)))
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

(defun applicable-mr-effects (n these-effects)
  "Give me the mr-effects that match the risk at hand"
  (let (applicable-list)
    (dolist (this-mr-effect these-effects)
      (if (= n (mr-effect-r this-mr-effect))
	  (push this-mr-effect applicable-list)))
    (reverse applicable-list)))

(defun applicable-ro-impacts (n these-impacts)
  "Give me the ro-impacts that match the risk at hand"
  (let (applicable-ro-impacts)
    (dolist (this-impact these-impacts)
      (if (= n (ro-impact-o this-impact))
	  (push this-impact applicable-ro-impacts)))
    applicable-ro-impacts))