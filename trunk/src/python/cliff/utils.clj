;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; GENERAL FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defn member? [item lst]
  (if (empty? lst)
    'nil
    (if (= item (first lst))
      true
      (member? item (rest lst)))))

(defn extract-unique [lst]
 (loop [l lst result []]
  (if (empty? l)
   (matrix (remove #(= 'none %) result))
   (recur (rest l)
          (conj result (if (member? (first l) result)
                        'none
                        (first l)))))))				      
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMPRESS FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		
	
(defn n-elts [elt n]
  (if (> n 1)
    (list n elt)
    (list 1 elt)))

(defn compr [elt n lst]
  (if (empty? lst)
    (list (n-elts elt n))
    (let [after (first lst)]
      (if (= after elt)
	(compr elt (+ n 1) (rest lst))
	(cons (n-elts elt n)
	      (compr after 1 (rest lst)))))))

(defn compress [lst]
"Accepts list of sorted values and returns
 unique values along with the number of times
 they occur in the list"
  (if (or (= (count lst) 0) (= (count lst) 1))
    lst
    (compr (first lst) 1 (rest lst))))
    
(defn unique-compress [coll]
"Accepts a compressed list and returns only the
 unique values"
  (map #(second %) coll))
  
(defn uc [lst]
	(unique-compress (compress (sort lst)))) 	    

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CLUSTER FUNCTIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;				
				
(defn mygroup [data]
	(let [goals (uc (to-vect (sel data :cols (- (ncol data) 1))))]
		(loop [g goals result []]
			(if (empty? g)
				result
				(recur
					(rest g)
					(conj result (apply vector (filter #(= (last %) (first g)) (to-vect data)))))))))
