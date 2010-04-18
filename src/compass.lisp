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
	"lisp/list"
	"lisp/ddp/model"
	"lisp/ddp/model2"
	"lisp/ddp/model24"
	))

(defun make-debug ()
  (make "lisp/lispfuns"
	"lisp/debug"
	))

(make-compass)
(make-debug)