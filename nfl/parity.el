(require 'cl)

;; Accessors
(defalias 'team-away 'caar
  "Access home team for given game.")
(defalias 'team-home 'cadr
  "Access away team for given game.")
(defalias 'away-score 'cdar
  "Access away team score.")
(defalias 'home-score 'cddr
  "Access home team score.")

(defun get-scores (file)
  "Read scores s-exp from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (read (current-buffer))))

(defun init-graph (scores)
  "Initialize a directed graph from SCORES."
  (let ((graph ()))
    (dolist (game scores graph)
      (add-to-list 'graph (list (team-home game)))
      (add-to-list 'graph (list (team-away game))))))

(defun create-win-graph (scores)
  "Generate the directed graph of wins."
  (let ((graph (init-graph scores)))
    (dolist (game scores graph)
      (if (> (away-score game) (home-score game))
	  (assign-win graph (team-away game) (team-home game))
	(assign-win graph (team-home game) (team-away game))))))

(defun assign-win (graph winner loser)
  "Add loser to winner's list in graph."
  (let ((winlist (assq winner graph)))
    (unless (member loser winlist)
      (setf (cdr winlist) (cons loser (cdr winlist))))))

;; Hamiltonian paths

(defun clear-search (graph val)
  "Initialize the graph nodes to value."
  (dolist (node graph)
    (set (car node) val)))

(defun print-path (path)
  "Output the path sexp."
  (insert (format "%S\n" path)))

(defun find-hamil (graph first node stack)
  "Recursively find all Hamiltonian cycles."
  (push node stack)
  (cond
   ((and (eq first node)
	 (= (length stack) (1+ (length graph))))
    (print-path (reverse stack)))
   ((eq (symbol-value node) 'visited) nil)
   (t (set node 'visited)
      (dolist (cur (cdr (assq node graph)))
	(find-hamil graph first cur stack))
      (set node 'unvisited))))

(defun hamil (graph)
  "Find and output all Hamiltonian cycles in the data to buffer *hamil*."
  (let ((first 'Steelers))
    (clear-search graph 'unvisited)
    (with-current-buffer (get-buffer-create "*hamil*")
      (erase-buffer)
      (find-hamil graph first first ()))))

;(hamil (create-win-graph (get-scores "scores2008.el")))
;(hamil (create-win-graph (get-scores "scores2009.el")))
