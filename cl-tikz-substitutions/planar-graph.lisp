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
                     :type point)
   (colours :reader graph-colours
            :initarg :colours
            :type hash-table
            :documentation "Hash table mapping edge types to TikZ colours")))

(eval-when (:compile-toplevel :load-toplevel :execute)
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
            :finally (setf factor (point (1+ max-x) (1+ max-y))))
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
                     :root root))))

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

(defclass vertex ()
  ((pos :accessor pos
        :initarg :pos
        :type point)
   (name :reader name
         :initarg :name
         :type symbol)))

(defun vertex (name pos)
  (make-instance 'vertex :pos pos :name name))

(defclass edge ()
  ((start :accessor edge-start
          :initarg :start
          :type vertex)
   (end :accessor edge-end
        :initarg :end
        :type vertex)
   (type :reader edge-type
         :initarg :type)))

(defun edge (start end type)
  (make-instance 'edge :start start :end end :type type))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Convention: a 1-substitution is the prototile graph
;; A 0-substitution makes no sense

(defun n-expansion-factor (graph n)
  (point-exp (expansion-factor graph) (1- n)))

(defun vertex-pos-in-substitution (origin name graph)
  (point+ (point*pt origin
                    (expansion-factor graph))
          (gethash name (graph-vertices graph))))

(defun substitute-vertex (vertex graph)
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges))
      graph
    (with-accessors ((name name)
                     (pos pos))
        vertex
      (let (new-vertices
            new-edges)
        (dohash (v) vertices
          (push (vertex v (vertex-pos-in-substitution pos v graph))
                new-vertices))
        (dohash (edge) edges
          (destructuring-bind (beg end type) edge
            (let ((beg-v (vertex beg (vertex-pos-in-substitution pos beg graph)))
                  (end-v (vertex end (vertex-pos-in-substitution pos end graph))))
              (push (edge beg-v end-v type) new-edges))))
        (values new-vertices new-edges)))))

(defun substitute-edge (edge graph)
  "EDGE is a list of length 3 (BEG END TYPE) where
BEG and END are of type POINT"
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges)
                   (subst graph-substitution))
      graph
    (let (new-edges)
      (with-accessors ((beg edge-start)
                       (end edge-end)
                       (type edge-type))
          edge
        (let ((substituted-edges (gethash type subst)))
          (dolist (subst-edge substituted-edges)
            (destructuring-bind (subst-beg-name subst-end-name subst-type) subst-edge
              (let ((new-beg (vertex subst-beg-name
                                     (vertex-pos-in-substitution (pos beg)
                                                                 subst-beg-name
                                                                 graph)))
                    (new-end (vertex subst-end-name
                                     (vertex-pos-in-substitution (pos end)
                                                                 subst-end-name
                                                                 graph))))
                (push (edge new-beg new-end subst-type) new-edges))))
          new-edges)))))

(defun 1-substitute-graph (graph)
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges))
      graph
    (let (1-vertices
          1-edges)
      (dohash (v pos) vertices
        (push (vertex v pos) 1-vertices))
      (dohash (edge) edges
        (destructuring-bind (beg end type) edge
          (push (edge (vertex beg (gethash beg vertices))
                      (vertex end (gethash end vertices))
                      type)
                1-edges)))
      (values 1-vertices 1-edges))))

(defun n-substitute-graph (graph n)
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges)
                   (factor expansion-factor)
                   (subst graph-substitution))
      graph
    (if (= n 1)
        (1-substitute-graph graph)
        (multiple-value-bind (n-1-vertices n-1-edges)
            (n-substitute-graph graph (1- n))
          (let ((subst-of-vertices
                  (loop :for v :in n-1-vertices
                        :for (subst-v-vertices
                              subst-v-edges)
                          = (multiple-value-list (substitute-vertex v graph))
                        :nconc subst-v-vertices :into subst-vertices
                        :nconc subst-v-edges :into subst-edges
                        :finally (return (list subst-vertices
                                               subst-edges))))
                (subst-of-edges (loop :for edge :in n-1-edges
                                      :nconc (substitute-edge edge graph))))
            (values (first subst-of-vertices)
                    (nconc (second subst-of-vertices)
                           subst-of-edges)))))))

(defun vertex-node-name (vertex)
  (with-point (x y) (pos vertex)
    (format nil "~A_~D_~D" (name vertex) (floor x) (floor y))))

(defun draw-vertex-1 (vertex)
  (with-accessors ((pos pos))
      vertex
    (draw-node (point-x pos) (point-y pos)
               :name (vertex-node-name vertex) ;; to correct
               :options (make-options :draw t
                                      :circle t
                                      :|inner sep| "2pt"))))

(defun draw-graph-edge (edge graph)
  (with-accessors ((beg edge-start)
                   (end edge-end)
                   (type edge-type))
      edge
    (draw-edge (vertex-node-name beg)
               (vertex-node-name end)
               :->
               :options (gethash type (graph-colours graph)))))

(defgeneric draw-substitution (base n))
(defmethod draw-substitution :before (base n)
  (when (< n 1)
    (error "Number ~S of steps cannot be negative" n)))

(defmethod draw-substitution ((graph graph-substitution) n)
  (multiple-value-bind (vertices edges) (n-substitute-graph graph n)
    (dolist (v vertices)
      (draw-vertex-1 v))
    (dolist (e edges)
      (draw-graph-edge e graph))))

(defun draw-substitution-in-file (graph name steps)
  (with-preamble-to-file ((format nil "~A.tex" name)) ()
    (with-env (tikzpicture)
      (draw-substitution graph steps))))


;;;; Examples
(defparameter *sierpinski* (def-graph-substitution
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
                             (diag (c a diag))
                             :colours
                             (hori blue)
                             (vert red)
                             (diag green)))

(defparameter *square* (def-graph-substitution
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
                         (diag (c a diag))
                         :colours
                         (hori blue)
                         (vert red)
                         (diag green)))

;; Might be bugged ? Seems to produce too many edges
(defparameter *H* (def-graph-substitution
                    :vertices
                    (a 0 0)
                    (b 1 1)
                    (c 1 -1)
                    (d -1 1)
                    (e -1 -1)
                    :edges
                    (a b diag1)
                    (a c diag2)
                    (e a diag1)
                    (d a diag2)
                    (e d vert1)
                    (c b vert2)
                    :substitution
                    (diag1 (b e diag1))
                    (diag2 (c d diag2))
                    (vert1 (d e vert1))
                    (vert2 (b c vert2))
                    :colours
                    (diag1 red)
                    (diag2 blue)
                    (vert1 green)
                    (vert2 black)))

(defparameter *weak-grid* (def-graph-substitution
                            :vertices
                            (a 0 0)
                            (b 1 0)
                            (c 2 0)
                            (d 2 1)
                            (e 2 2)
                            (f 1 2)
                            (g 0 1)
                            :edges
                            (a b hori)
                            (b c hori)
                            (c d vert)
                            (d e vert)
                            (f e hori)
                            (g f diag)
                            (a g vert)
                            :substitution
                            (hori (c a hori))
                            (vert (e c vert))
                            (diag (e a diag))
                            :colours
                            (hori blue)
                            (vert green)
                            (diag red)))
