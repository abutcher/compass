(defun make (&rest files)
   (handler-bind
      ((style-warning #'muffle-warning))
  	(dolist (f files)
	  (load f))))

(defun make-compass ()
  (make "lisp/random"
	"lisp/randomizer"
	"lisp/gen"
	"lisp/compass"
	"lisp/counted"
	"lisp/cat.lisp"
	"lisp/cluster"
	"lisp/crossindex"
	"lisp/dot"
	"lisp/era"
	"lisp/separate"
	"lisp/testing"
	"lisp/distance"
	"lisp/folds"
	"lisp/hash"
	"lisp/entropy"
	"lisp/list"
	"lisp/caution"
	"lisp/nb"
	"lisp/normal"
	"lisp/normalize"
	"lisp/nway"
	"lisp/split"
	"lisp/ddp/model"
	"lisp/ddp/model2"
	"lisp/ddp/model3"
	"lisp/ddp/model4"
	"lisp/ddp/model5"
	"lisp/ddp/model24"
	"lisp/variance"
	"lisp/cosine-similarity"
	"lisp/math"
	"lisp/oracle"
	"lisp/prune"
	"lisp/wilcoxon"
	"lisp/wilcoxon2"
	"lisp/median"
	"lisp/mre"
	"lisp/strings"
	"lisp/while"
	"lisp/kmeans"
	"lisp/tableselectors"
	"lisp/transform"
	"lisp/utilities"
	"lisp/k"
	"lisp/xindex"
	"lisp/xindexselectors"
	"lisp/discrete"
	))

(defun make-tables ()
  (make "lisp/table/structs"
	"lisp/table/header"
	"lisp/table/data"
	"lisp/table/nbins"
	"lisp/table/table"
	"lisp/table/xindex"
	))

(defun make-debug ()
  (make "lisp/lispfuns"
	"lisp/debug"
	))

(defun make-models-large ()
  (make "lisp/ddp/model44"
	"lisp/ddp/model48"
	"lisp/ddp/model416"
	))

(defun make-effort ()
  (make "lisp/effort/data/albrecht.lisp"
	"lisp/effort/data/china.lisp"
	"lisp/effort/data/cocomo81.lisp"
	"lisp/effort/data/cocomo81e.lisp"
	"lisp/effort/data/cocomo81o.lisp"
	"lisp/effort/data/cocomo81s.lisp"
	"lisp/effort/data/desharnais-all.lisp"
	"lisp/effort/data/desharnais-l1.lisp"
	"lisp/effort/data/desharnais-l2.lisp"
	"lisp/effort/data/desharnais-l3.lisp"
	"lisp/effort/data/finnish.lisp"
	"lisp/effort/data/kemerer.lisp"
	"lisp/effort/data/maxwell.lisp"
	"lisp/effort/data/nasa93-center-1.lisp"
	"lisp/effort/data/nasa93-center-2.lisp"
	"lisp/effort/data/nasa93-center-3.lisp"
	"lisp/effort/data/nasa93-center-5.lisp"
	"lisp/effort/data/nasa93.lisp"
	"lisp/effort/data/sdr.lisp"
	"lisp/effort/data/telecom.lisp"
	))

(defun make-classification ()
  (make "lisp/classification/data/anneal.lisp"
	"lisp/classification/data/audiology.lisp"
	"lisp/classification/data/credit-rating.lisp"
	"lisp/classification/data/horse-colic.lisp"
	"lisp/classification/data/hypothyroid.lisp"
	"lisp/classification/data/pima_diabetes.lisp"
	"lisp/classification/data/primary-tumor.lisp"
	"lisp/classification/data/sonar.lisp"
	"lisp/classification/data/soybean.lisp"
	"lisp/classification/data/splice.lisp"
	"lisp/classification/data/vote.lisp"))

(defun make-defect ()
  (make "lisp/defect-testing.lisp"
        "lisp/defect/jm1.lisp"
	"lisp/defect/kc1.lisp"
	"lisp/defect/mc1.lisp"
	"lisp/defect/pc1.lisp"
	"lisp/defect/small.lisp"))

(defun make-demo ()
  (make "lisp/examples/demo1.lisp"
	"lisp/examples/demo2.lisp"
	"lisp/examples/demo3.lisp"
	"lisp/examples/demo4.lisp"
	"lisp/examples/demo5.lisp"
	"lisp/examples/demo6.lisp"
	"lisp/examples/demo7.lisp"
	"lisp/examples/demo8.lisp"
	"lisp/examples/demo9.lisp"
	"lisp/examples/demo10.lisp"))


(make-compass)
(make-tables)
(make-debug)
(make-effort)
;(make-classification)
(make-demo)
(make-defect)