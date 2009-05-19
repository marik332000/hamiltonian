;; Exhaustive Hamiltonian path finder

(load-option 'format)

(define graph (make-eq-hash-table))

;; Mark the node as visited
(define (visit node)
  (hash-table/put! graph node 'visited))

;; Mark the node as unvisited
(define (unvisit node)
  (hash-table/put! graph node 'unvisited))

;; Check if node has been visited
(define (visited? node)
  (eq? (hash-table/get graph node 'unvisited) 'visited))

;; Initialize the graph so that all nodes are unvisited.
(define (clear-graph data)
  (if (not (null? data))
      (begin
        (unvisit (caar data))
        (clear-graph (cdr data)))))

;; Get nodes's connections.
(define (get-edges data node)
  (cond
   ((null? data) '())
   ((eq? (caar data) node) (cdar data))
   ((get-edges (cdr data) node))))

;; Print out a path
(define (print-path path)
  (if (null? path) (format #t "~%")
      (begin
        (format #t "~A-" (car path))
        (print-path (cdr path)))))

;; Print out a path
(define (print-path-n path n)
  (if (eq? n 0) (format #t "~%")
      (begin
        (format #t "~A-" (car path))
        (print-path-n (cdr path) (- n 1)))))

;; Recursively find all Hamiltonian paths.
(define (find-hamil data node path)
  (set! path (cons node path))
  (if (> (length path) 41) (print-path-n path 8))
  (cond
   ((visited? node) #f)
   ((= (length path) (length data)) (print-path path))
   ((begin
      (visit node)
      (let walk ((edges (get-edges data node)))
        (if (not (null? edges))
            (begin
              (find-hamil data (car edges) path)
              (walk (cdr edges)))))
      (unvisit node)))))

(define (hamil data node)
  (clear-graph data)
  (find-hamil data node '()))

;; Test graph
(define test-graph'((a d)
                    (b j d)
                    (c d e)
                    (d a b j i g c)
                    (e c g f)
                    (f h g e)
                    (g d e f)
                    (h i f)
                    (i d h j)
                    (j b d i)))

;; US States graph data
(define us-graph '((me nh)
                   (nh vt ma me)
                   (vt ny ma nh)
                   (ma ri ct ny nh vt)
                   (ny pa nj ma ct vt)
                   (ri ma ct)
                   (ct ri ma ny)
                   (nj pa ny de)
                   (de md pa nj)
                   (pa nj ny de md wv oh)
                   (md pa de va wv)
                   (va md wv ky tn nc)
                   (nc va tn ga sc)
                   (sc nc ga)
                   (ga fl sc al nc tn)
                   (al ms fl ga tn)
                   (ms la ar tn al)
                   (tn ms al ga nc va ky mo ar)
                   (ky wv va tn mo il in oh)
                   (wv md pa oh ky va)
                   (oh pa wv ky in mi)
                   (fl al ga)
                   (mi wi oh in)
                   (wi mn ia il mi)
                   (il in ky mo ia wi)
                   (in oh ky il mi)
                   (mo il ky tn ar ok ks ne ia)
                   (ar mo tn ms la tx ok)
                   (la ms ar tx)
                   (tx ok nm ar la)
                   (ok ks mo ar tx nm co)
                   (ks ok co ne mo)
                   (ne sd ia mo ks co wy)
                   (sd nd mn ia ne wy mt)
                   (nd mt sd mn)
                   (ia ne mo il wi mn sd)
                   (mn wi ia sd nd)
                   (mt id wy sd nd)
                   (wy id ut co ne sd mt)
                   (co ne ks ok nm ut wy)
                   (nm co ok tx az)
                   (az nm ut ca nv)
                   (ut nv id wy co az)
                   (id mt wy ut nv or wa)
                   (wa or id)
                   (or wa id nv ca)
                   (nv or id ut az ca)
                   (ca az nv or)))
