(defmacro while (test &body b)
	`(loop (when (not ,test) (return)) ,@b))
