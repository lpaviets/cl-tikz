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
            :minimize x :into min-x
            :maximize y :into max-y
            :minimize y :into min-y
            :when (= 0 x y)
              :do (setf root vertex)
            :finally (setf factor (point (1+ (- max-x min-x))
                                         (1+ (- max-y min-y)))))
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

(defun vertex-pos-in-graph (name graph)
  (gethash name (graph-vertices graph)))

(defun vertex-pos-in-substitution (origin name graph)
  (point+ (point*pt origin
                    (expansion-factor graph))
          (vertex-pos-in-graph name graph)))

(defun substitute-edge-list-edges (edge graph)
  (gethash (edge-type edge) (graph-substitution graph)))

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
            (let ((beg-v (find beg new-vertices :key #'name))
                  (end-v (find end new-vertices :key #'name)))
              (assert (and beg-v end-v))
              (push (edge beg-v end-v type) new-edges))))
        (values new-vertices new-edges)))))

(defun substitute-edge (edge graph start-subst end-subst)
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges)
                   (subst graph-substitution))
      graph
    (let (new-edges)
      (with-accessors ((type edge-type))
          edge
        (let ((substituted-edges (substitute-edge-list-edges edge graph)))
          (dolist (subst-edge substituted-edges)
            (destructuring-bind (subst-beg-name subst-end-name subst-type)
                subst-edge
              (let ((new-beg (find subst-beg-name start-subst :key #'name))
                    (new-end (find subst-end-name end-subst :key #'name)))
                (assert (and new-beg new-end))
                (push (edge new-beg new-end subst-type) new-edges))))
          new-edges)))))

(defun 1-substitute-graph (graph)
  (with-accessors ((vertices graph-vertices)
                   (edges graph-edges))
      graph
    (let ((table (make-hash-table :test #'eq))
          1-vertices
          1-edges)
      (dohash (v pos) vertices
        (let ((new-v (vertex v pos)))
          (push new-v 1-vertices)
          (setf (gethash v table) new-v)))
      (dohash (edge) edges
        (destructuring-bind (beg end type) edge
          (push (edge (gethash beg table)
                      (gethash end table)
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
          (let* ((table (make-hash-table :test #'eq))
                 (subst-of-vertices
                   (loop :for v :in n-1-vertices
                         :for (subst-v-vertices
                               subst-v-edges)
                           = (multiple-value-list (substitute-vertex v graph))
                         :do (setf (gethash v table) subst-v-vertices)
                         :append subst-v-vertices :into subst-vertices
                         :append subst-v-edges :into subst-edges
                         :finally (return (list subst-vertices
                                                subst-edges))))
                 (subst-of-edges
                   (loop :for edge :in n-1-edges
                         :for start = (gethash (edge-start edge) table)
                         :for end = (gethash (edge-end edge) table)
                         :do (assert (and start end))
                         :append (substitute-edge edge
                                                  graph
                                                  start
                                                  end))))
            (values (first subst-of-vertices)
                    (append (second subst-of-vertices)
                            subst-of-edges)))))))

(defun vertex-node-name (vertex)
  (with-point (x y) (pos vertex)
    (format nil "~A_~D_~D" (name vertex) (floor x) (floor y))))

(defun draw-vertex-1 (vertex)
  (with-accessors ((pos pos))
      vertex
    (draw-node (+ (random 0.35) (point-x pos)) (+ (random 0.35) (point-y pos))
               :name (vertex-node-name vertex) ;; to correct
               :options (make-options :circle t
                                      :|inner sep| "2pt"))))

(defun edge-colour (edge graph)
  (gethash (edge-type edge) (graph-colours graph)))

(defun draw-graph-edge (edge graph)
  (with-accessors ((beg edge-start)
                   (end edge-end)
                   (type edge-type))
      edge
    (draw-edge (vertex-node-name beg)
               (vertex-node-name end)
               :->
               :options (edge-colour edge graph))))

(defmethod draw-substitution ((graph graph-substitution) n)
  (multiple-value-bind (vertices edges) (n-substitute-graph graph n)
    (dolist (v vertices)
      (draw-vertex-1 v))
    (dolist (e edges)
      (draw-graph-edge e graph))))
