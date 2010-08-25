(defstruct fold
  index
  (train (make-counted))
  (test  (make-counted)))

(defun folds (l0 nfolds &key (pred #'<) (key #'identity)  )
  (let (folds
        (i nfolds)
        (l (sort l0 pred :key key)))
    (dotimes (j nfolds)
      (push (make-fold :index j) folds)) ; initialize the folds
    (if (= nfolds 1)
        (dolist (x l)
          (counted+ x (fold-test  (nth 0 folds)))
          (counted+ x (fold-train (nth 0 folds))))
        (dolist (x l)
          (decf  i) ; go to the next fold
          (dotimes (j nfolds)
            (if (= i j) 
                (counted+ x (fold-test  (nth j folds)))  ; add 1/j to "test"
                (counted+ x (fold-train (nth j folds))))) ; add rest to "train"
          (if (zerop i)
              (setf i nfolds))))
    folds)) ; go back to the start

;(deftest test-folds ()
;  (let ((tmp3 (folds '(1 2 30 4 5 6 70 8 9 10 11) 3))
;        (tmp1 (folds '(1 2 30 4 5 6 70 8 9 10 11) 1)))
;    (check tmp3 "
;   (#S(FOLD
;      :INDEX 2
;      :TRAIN #S(COUNTED :ALL (70 30 10 9 6 5 2 1) :N 8 :SORTED NIL)
;      :TEST  #S(COUNTED :ALL (11 8 4) :N 3 :SORTED NIL))
;     #S(FOLD
;        :INDEX 1
;        :TRAIN #S(COUNTED :ALL (30 11 9 8 5 4 1) :N 7 :SORTED NIL)
;        :TEST  #S(COUNTED :ALL (70 10 6 2) :N 4 :SORTED NIL))
;     #S(FOLD
;        :INDEX 0
;        :TRAIN #S(COUNTED :ALL (70 11 10 8 6 4 2) :N 7 :SORTED NIL)
;        :TEST  #S(COUNTED :ALL (30 9 5 1) :N 4 :SORTED NIL)))")))
