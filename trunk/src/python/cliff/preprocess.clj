;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BORE FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			

(defn my-best-rest [data]
"data = binned version = a"
  (let [group-it (mygroup data)]
    (loop [g group-it results (transient [])]
      (if (empty? g)
	      (persistent! results)
	      (recur (rest g)
	       (conj! 
		       results 
		       (vector (first g) 
			             (matrix 
			              (apply 
			                concat 
			                (filter #(not= (first g) %) group-it))))))))))
			                
(defn bin-rank [D val col best rest]
"D = binned data set"
  (let [pbest (/ (nrow best) (nrow D))
	      prest (/ (nrow rest) (nrow D))
	
	      freqEbest (fn []
		                (let [one (filter #(= (nth % col) val) best)] 
		                  (if (= (nrow one) 0)
			                  0
			                  (let [freqEbest0 (count (filter #(= (nth % col) val) best)) 
			                        freqEbest1 (/ freqEbest0 (nrow best))]
			                    freqEbest1))))
	
      	freqErest (fn []
		                (let [two (filter #(= (nth % col) val) rest)]
		                  (if (= (nrow two) 0)
			                  0
			                  (let [freqErest0 (count (filter #(= (nth % col) val) rest))
			                        freqErest1 (/ freqErest0 (nrow rest))]
			                    freqErest1))))														 
	
      	likebestE (* (freqEbest) pbest)
      	likerestE (* (freqErest) prest)
      	rank (/ (Math/pow likebestE 2) (+ likebestE likerestE))]
    [val rank col]))

(defn rank-vals [D best rest]
	(let [get-ranks1 (fn [col]
										(let [vals (uc (to-vect (sel D :cols col)))]
											(map #(bin-rank D % col best rest) vals)))
				get-ranks2 (map #(get-ranks1 %) (range 0 (- (ncol D) 1)))
				get-ranks3 (map #(reverse (sort-by second %)) get-ranks2)
				get-ranks4 (reverse (sort-by second (map first get-ranks3)))]
		get-ranks4))
