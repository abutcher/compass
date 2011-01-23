(defn get-criteria [D]
	(let [br (my-best-rest D)]
		(loop [br1 br result []]
			(if (empty? br1)
				result
				(recur (rest br1)
							 (conj 
								 result 
									 (rank-vals 
										 D 
										 (first (first br1)) 
										 (second (first br1)))))))))
							 
(defn select-instances [crit inst]
	(loop [c crit prev inst result inst]
		(if (or (empty? c) (empty? result))
			prev
			(recur 
				(rest c)
				result
				(filter #(= (nth % (last (first c)))
																 (first (first c))) 
											 result)))))

(defn cliff [D]	
	(let [crits (get-criteria D)
				insts (mygroup D)
				prototypes (fn [lst-of-lst]
										 (loop [lol lst-of-lst result []]
											(if (empty? lol)
												(extract-unique 
													(apply bind-rows result))
												(recur 
													(rest lol)
													(conj result (if (matrix? (first lol))
																				 (first lol)
																				 (matrix (first lol))))))))
				ans (prototypes (map #(select-instances %1 %2) crits insts))]
			ans))
