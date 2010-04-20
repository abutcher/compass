(let (likelihoods negative-mr-effects)

  ; Set up likelihoods with r-apl values
  (dotimes (i (length (ddp-model-r-apl this-model)))
    (push (nth i (ddp-model-r-apl this-model)) likelihoods))

  ; Reverse em for sanity's sake
  (setf likelihoods (reverse likelihoods))

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
			(* (nth (mr-effect-m first-neg-mr-effect) mitigations)
			   (mr-effect-effect first-neg-mr-effect))))))))

  ; Now go through and mark any aggrevated impacts that exist
  (let ((aggrevated-impacts (ddp-model-r-aggrevated-impact this-model)))
    (dotimes (i (length aggrevated-impacts))
      (let ((remaining-neg-mr-effects (cdr (applicable-mr-effects i negative-mr-effects))))
	(unless (null remaining-neg-impacts)
	  (let ((aggrevated-impact (nth i aggrevated-impacts)))
	    (dolist (effect remaining-neg-mr-effects)
	      (setf aggrevated-impact
		    (* aggrevated-impact
		       (- 1 (* (nth (mr-effect-m effect) mitigations)
			       (+ 1 (mr-effect-effect effect)))))))
	    (setf (nth i aggrevated-impacts) aggrevated-impact)))))
    (setf (ddp-model-r-aggrevated-impact this-model) aggrevated-impacts))

  (setf (ddp-model-r-likelihood this-model) (reverse likelihoods)))