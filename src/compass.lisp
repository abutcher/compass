(defun make (&rest files)
   (handler-bind 
      ((style-warning #'muffle-warning))
  	(dolist (f files)
	  (load f))))

(defun make-compass ()
  (make "lisp/random"
	"lisp/gen"
	"lisp/compass"
	"lisp/distance"
	"lisp/hash"
	"lisp/entropy"
	"lisp/list"
	"lisp/caution"
	"lisp/normal"
	"lisp/ddp/model"
	"lisp/ddp/model2"
	"lisp/ddp/model3"
	"lisp/ddp/model4"
	"lisp/ddp/model5"
	"lisp/ddp/model24"
	"lisp/variance"
	"lisp/cosine-similarity"
	"lisp/prune"
	"lisp/wilcoxon"
	"lisp/wilcoxon2"
	"lisp/median"
	"lisp/mre"
	"lisp/while"
	"lisp/kmeans"
	"lisp/utilities"
	"lisp/k"
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

(make-compass)
(make-tables)
(make-debug)
(make-effort)