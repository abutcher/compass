demo1:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo1.lisp\")" --eval "(demo-1)" --eval "(quit)" | less

demo2:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo2.lisp\")" --eval "(demo-2)" --eval "(quit)" | less

demo3:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo3.lisp\")" --eval "(demo-3)" --eval "(quit)" | less

demo4:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo4.lisp\")" --eval "(demo-4)" --eval "(quit)" | less

demo5:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo5.lisp\")" --eval "(demo-5)" --eval "(quit)" | less

demo6:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo6.lisp\")" --eval "(demo-6)" --eval "(quit)" | less

demo7:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo7.lisp\")" --eval "(demo-7)" --eval "(quit)" | less

demo8:
	sbcl --noinform --load "compass.lisp" \
			--eval "(cat \"lisp/examples/demo8.lisp\")" \
			--eval "(format t \"~%~%Wait for it...~%~%\")" \
			--eval "(demo-8)" --eval "(quit)" | less

demo9:
	sbcl --noinform --load "compass.lisp" --eval "(cat \"lisp/examples/demo9.lisp\")" --eval "(demo-9)" --eval "(quit)" | less

demo10:
	sbcl --noinform --load "compass.lisp" \
			--eval "(cat \"lisp/examples/demo10.lisp\")" \
			--eval "(demo-10)" \
			--eval "(format t \"Already ran it for you... go ahead and open up maxwell.dot.png~%\")" \
			--eval "(quit)"
	dot -Tpng -O maxwell.dot
