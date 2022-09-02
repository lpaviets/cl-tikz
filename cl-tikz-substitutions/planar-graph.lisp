(in-package #:org.numbra.cl-tikz-substitutions)

;;;; Planar graph

;;; This file implements substitutions on graphs as defined in
;;; [LINK TO REFERENCE]
;;; Substitutions use a graph G, with oriented and labeled edges.
;;; At each step
;;; - each vertex is replaced by G
;;; - each edge is replaced by a set of edges, depending on its label

(defclass graph-substitution ()
  ((root :reader graph-root
         :initarg :root
         :type symbol)
   (vertices :reader graph-vertices
             :initarg :vertices
             :type hash-table
             :documentation "Hash table of vertices. Keys are their name,
and values are their position, as a POINT")
   (edges :reader graph-edges
          :initarg :edges
          :type hash-table
          :documentation "Set of edges. Each edge is a list of 3 elements,
the name of its two endpoints and its type")
   (substitution :reader graph-substitution
                 :initarg :substitution
                 :type hash-table
                 :documentation "Hash table of substitution rules for the edges.
Each key is an edge type, and the values are lists of edges.")
   (expansion-factor :reader expansion-factor
                     :initarg :factor
                     :type (cons integer integer))))

(defmacro def-graph-substitution (&body description)
  (assert (eq (car description) :vertices)
          nil
          "Malformed description: should start with :VERTICES instead of ~S"
          (car description))
  (let* ((edges-pos (position :edges description))
         (subst-pos (position :substitution description)))
    (assert edges-pos nil "Malformed description: missing :EDGES argument")
    (assert subst-pos nil "Malformed description: missing :SUBSTITUTION argument")
    (assert (< edges-pos subst-pos)
            nil
            "Malformed description: :EDGES should come before :SUBSTITUTION")
    `(make-graph-substitution
      ',(subseq description 1 edges-pos)
      ',(subseq description (1+ edges-pos) subst-pos)
      ',(subseq description (1+ subst-pos)))))

(defun make-graph-substitution (vertices edges substitution)
  (let ((vertices-table (make-hash-table :test 'eq
                                         :size (length vertices)))
        (edges-table (make-hash-table :test 'equal
                                      :size (length edges)))
        (subst-table (make-hash-table :test 'eq
                                      :size (length edges)))
        factor root)
    (loop :for (vertex x y) :in vertices
          :do (setf (gethash vertex vertices-table) (point x y))
          :maximize x :into max-x
          :maximize y :into max-y
          :when (= 0 x y)
            :do (setf root vertex)
          :finally (setf factor (cons (1+ max-x) (1+ max-y))))
    (loop :for edge :in edges
          :do (setf (gethash edge edges-table) t))
    (loop :for (type . subst) :in substitution
          :do (setf (gethash type subst-table) subst))
    (make-instance 'graph-substitution
                   :vertices vertices-table
                   :edges edges-table
                   :substitution subst-table
                   :factor factor
                   :root root)))

(defun factor-steps (factor steps)
  (cons (expt (car factor) steps)
        (expt (cdr factor) steps)))

(defun scale-point (point factor)
  (with-point (x y) point
    (point (* x (car factor))
           (* y (cdr factor)))))

(defun super-vertex-pos (vertex graph steps)
  (let ((factor (factor-steps (expansion-factor graph) steps)))
    (with-point (x y) (gethash vertex (graph-vertices graph))
      (point (* x (1- (car factor)))
             (* y (1- (cdr factor)))))))

;;; Return the position of the "origin" of the meta-tile
;;; of order STEPS *when seen as the subtile in position VERTEX
;;; in the steps+1-meta-tile*
(defun super-tile-origin (vertex graph steps)
  (scale-point (gethash vertex (graph-vertices graph))
               (factor-steps (expansion-factor graph) steps)))

(defun super-vertex-in-meta-tile-pos (tile-name vertex graph steps)
  (let ((origin (super-tile-origin tile-name graph steps))
        (vertex-pos (super-vertex-pos vertex graph steps)))
    (add-point origin vertex-pos)))

(defun vertex-node-name (vertex)
  (let ((origin (point-absolute-point 0 0)))
    (format nil "~A_~D_~D"
            vertex
            (floor (point-x origin))
            (floor (point-y origin)))))

(defun draw-vertex-1 (vertex x y)
  (draw-node x y
             :name (vertex-node-name vertex)
             :options (make-options :draw t
                                    :circle t
                                    :|inner sep| "2pt")))

(defun draw-graph-substitution-1 (graph)
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges))
      graph
    (dohash (vertex coords) vertices
      (draw-vertex-1 vertex (point-x coords) (point-y coords)))
    (dohash (edge) edges
      (draw-edge (vertex-node-name (first edge))
                 (vertex-node-name (second edge))
                 :->))))

(defgeneric draw-substitution (base steps))
(defmethod draw-substitution :before (base steps)
  (when (< steps 1)
    (error "Number ~S of steps cannot be negative" steps)))

(defmethod draw-substitution ((graph graph-substitution) steps)
  (cond
    ((= steps 1) (draw-graph-substitution-1 graph))
    (t (with-accessors ((vertices graph-vertices)
                        (edges graph-edges)
                        (subst graph-substitution)
                        (factor expansion-factor))
           graph
         (let ((factor-steps (factor-steps factor steps)))
           (dohash (vertex coords) vertices
             (with-point (x y) (scale-point coords factor-steps)
               (with-shift (x y)
                 (draw-substitution graph (1- steps))))))))))

;;; Sierpinski:
(defun draw-sierpinski (steps)
  (let ((sierpinski (def-graph-substitution
                      :vertices
                      (a 0 0)
                      (b 1 0)
                      (c 1 1)
                      :edges
                      (a b hori)
                      (b c vert)
                      (a c diag)
                      :substitution
                      (hori (b a hori))
                      (vert (c b vert))
                      (diag (c a diag)))))
    (with-preamble-to-file ("sierpinski.tex") ()
      (with-env (tikzpicture)
        (draw-substitution sierpinski steps)))))

(defun draw-square-diagonal (steps)
  (let ((square (def-graph-substitution
                  :vertices
                  (a 0 0)
                  (b 1 0)
                  (c 1 1)
                  (d 0 1)
                  :edges
                  (a b hori)
                  (b c vert)
                  (d c hori)
                  (a d vert)
                  (a c diag)
                  :substitution
                  (hori (c d hori))
                  (vert (c b vert))
                  (diag (c a diag)))))
    (with-preamble-to-file ("square.tex") ()
      (with-env (tikzpicture)
        (draw-substitution square steps)))))
