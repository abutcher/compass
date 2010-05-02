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
	))

(defun make-table ()
  (make "lisp/table/structs"
	"lisp/table/data"
	"lisp/table/header"
	"lisp/table/nbins"
	"lisp/table/table"
	"lisp/table/xindex"
	))

(defun make-debug ()
  (make "lisp/lispfuns"
	"lisp/debug"
	))

(defun make-effort ()
  (make "lisp/effort/data/cocomo81"
	"lisp/effort/data/cocomo81e"
	"lisp/effort/data/cocomo81s"
	"lisp/effort/data/desharnais-all"
	"lisp/effort/data/desharnais-l2"
	"lisp/effort/data/nasa93-center-1"
	"lisp/effort/data/nasa93-center-3"
	"lisp/effort/data/nasa93.lisp"
	"lisp/effort/data/cocomo81o"
	"lisp/effort/data/desharnais-l1"
	"lisp/effort/data/desharnais-l3"
	"lisp/effort/data/nasa93-center-2"
	"lisp/effort/data/nasa93-center-5"
	"lisp/effort/data/sdr"))


(make-compass)
(make-table)
(make-debug)
(make-effort)