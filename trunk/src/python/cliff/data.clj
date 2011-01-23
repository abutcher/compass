(def incanter-home (System/getProperty "incanter.home")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UCI DATASETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def vote (read-dataset 
                 (str incanter-home "data/vote.csv") 
                 :delim \,
                 :header true))
(def vote (to-matrix vote))

(def iris (read-dataset 
                 (str incanter-home "data/iris.csv") 
                 :delim \,
                 :header true))
(def iris  (to-matrix iris))

(def breastcancer (read-dataset 
                 (str incanter-home "data/breastcancer.csv") 
                 :delim \,
                 :header true))
(def breastcancer (to-matrix breastcancer))

(def labor (read-dataset 
                 (str incanter-home "data/labor.csv") 
                 :delim \,
                 :header true))
(def labor (to-matrix labor))

(def lymph (read-dataset 
                 (str incanter-home "data/lymph.csv") 
                 :delim \,
                 :header true))
(def lymph (to-matrix lymph))
