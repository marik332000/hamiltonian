(defun get-graph (file)
  "Read scores s-exp from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun make-dot (graph)
  "Generate the DOT graph from GRAPH."
  (dolist (winner graph)
    (dolist (loser (cdr winner))
      (insert "\t" (symbol-name (car winner))
	      " -> " (symbol-name loser) "\n"))))

(make-dot (get-graph "graph2009.el"))
