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
                     :type (cons integer integer))
   (colours :reader graph-colours
            :initarg :colours
            :type hash-table
            :documentation "Hash table mapping edge types to TikZ colours")))

(defun split-on-keywords (sequence keywords)
  (loop :with subseq = nil
        :with splitted = nil
        :for elt :in sequence
        :if (member elt keywords :test 'eq)
          :do (when subseq (push (nreverse subseq) splitted))
              (setf subseq (list elt))
        :else
          :do (push elt subseq)
        :finally (push (nreverse subseq) splitted)
                 (return (nreverse splitted))))

(defmacro def-graph-substitution (&body description)
  (let ((keywords '(:vertices :edges :substitution :colours)))
    (assert (member (car description) keywords)
            nil
            "Malformed description: should start with a valid keyword, not ~S"
            (car description))
    (let ((splitted (split-on-keywords description keywords)))
      (flet ((extract-argument (arg list)
               (cdr (find arg list :key 'car :test 'eq))))
        `(make-graph-substitution
          :vertices ',(extract-argument :vertices splitted)
          :edges ',(extract-argument :edges splitted)
          :substitution ',(extract-argument :substitution splitted)
          :colours ',(extract-argument :colours splitted))))))

(defun make-graph-substitution (&key vertices edges substitution colours)
  (let ((vertices-table (make-hash-table :test 'eq
                                         :size (length vertices)))
        (edges-table (make-hash-table :test 'equal
                                      :size (length edges)))
        (subst-table (make-hash-table :test 'eq
                                      :size (length substitution)))
        (colours-table (make-hash-table :test 'eq
                                        :size (length colours)))
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
    (loop :for (type colour) :in colours
          :do (setf (gethash type colours-table) colour))
    (make-instance 'graph-substitution
                   :vertices vertices-table
                   :edges edges-table
                   :substitution subst-table
                   :colours colours-table
                   :factor factor
                   :root root)))

(defun factor-steps (factor steps)
  (cons (expt (car factor) (1- steps))
        (expt (cdr factor) (1- steps))))

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

(defun draw-super-edge (beg-tile beg-vertex end-tile end-vertex type graph steps)
  (let ((beg (super-vertex-in-meta-tile-pos beg-tile beg-vertex graph steps))
        (end (super-vertex-in-meta-tile-pos end-tile end-vertex graph steps)))
    (draw-line (point-x beg) (point-y beg) (point-x end) (point-y end)
               :options (let ((colour (gethash type (graph-colours graph))))
                          (if colour
                              (list colour '->)
                              '->)))))

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
                   (edges graph-edges)
                   (substs graph-substitution)
                   (colours graph-colours))
      graph
    (dohash (vertex coords) vertices
      (draw-vertex-1 vertex (point-x coords) (point-y coords)))
    (dohash (edge) edges
      (destructuring-bind (beg end type) edge
        (draw-edge (vertex-node-name beg)
                   (vertex-node-name end)
                   :->
                   :options (gethash type colours))))))

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
                 (draw-substitution graph (1- steps)))))
           (dohash (edge) edges
             (destructuring-bind (beg end type) edge
               (let ((new-edges (gethash type subst)))
                 (dolist (new-edge new-edges)
                   (draw-super-edge beg (first new-edge)
                                    end (second new-edge)
                                    (third edge)
                                    graph steps))))))))))

;;; Sierpinski:
(defun draw-substitution-in-file (graph name steps)
  (with-preamble-to-file ((format nil "~A.tex" name)) ()
      (with-env (tikzpicture)
        (draw-substitution graph steps))))

;; (defparameter *sierpinski* (def-graph-substitution
;;                              :vertices
;;                              (a 0 0)
;;                              (b 1 0)
;;                              (c 1 1)
;;                              :edges
;;                              (a b hori)
;;                              (b c vert)
;;                              (a c diag)
;;                              :substitution
;;                              (hori (b a hori))
;;                              (vert (c b vert))
;;                              (diag (c a diag))
;;                              :colours
;;                              (hori blue)
;;                              (vert red)
;;                              (diag green)))

;; (defparameter *square* (def-graph-substitution
;;                          :vertices
;;                          (a 0 0)
;;                          (b 1 0)
;;                          (c 1 1)
;;                          (d 0 1)
;;                          :edges
;;                          (a b hori)
;;                          (b c vert)
;;                          (d c hori)
;;                          (a d vert)
;;                          (a c diag)
;;                          :substitution
;;                          (hori (c d hori))
;;                          (vert (c b vert))
;;                          (diag (c a diag))
;;                          :colours
;;                          (hori blue)
;;                          (vert red)
;;                          (diag green)))

;; ;; Might be bugged ? Seems to produce too many edges
;; (defparameter *H* (def-graph-substitution
;;                     :vertices
;;                     (a 0 0)
;;                     (b 1 1)
;;                     (c 1 -1)
;;                     (d -1 1)
;;                     (e -1 -1)
;;                     :edges
;;                     (a b diag1)
;;                     (a c diag2)
;;                     (e a diag1)
;;                     (d a diag2)
;;                     (e d vert1)
;;                     (c b vert2)
;;                     :substitution
;;                     (diag1 (b e diag1))
;;                     (diag2 (c d diag2))
;;                     (vert1 (d e vert1))
;;                     (vert2 (b c vert2))
;;                     :colours
;;                     (diag1 red)
;;                     (diag2 blue)
;;                     (vert1 green)
;;                     (vert2 black)))

;; (defparameter *weak-grid* (def-graph-substitution
;;                             :vertices
;;                             (a 0 0)
;;                             (b 1 0)
;;                             (c 2 0)
;;                             (d 2 1)
;;                             (e 2 2)
;;                             (f 1 2)
;;                             (g 0 1)
;;                             :edges
;;                             (a b hori)
;;                             (b c hori)
;;                             (c d vert)
;;                             (d e vert)
;;                             (f e hori)
;;                             (g f diag)
;;                             (a g vert)
;;                             :substitution
;;                             (hori (c a hori))
;;                             (vert (e c vert))
;;                             (diag (e a diag))
;;                             :colours
;;                             (hori blue)
;;                             (vert green)
;;                             (diag red)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convention: a 1-substitution is the prototile graph
;; A 0-substitution makes no sense
