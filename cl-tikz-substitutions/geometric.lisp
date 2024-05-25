(in-package #:org.numbra.cl-tikz-substitutions)

;;;; Planar graph

;;; This file implements substitutions on graphs as studied by e.g.
;;; Goodman-Strauss. Classic examples would involve the chair tiling,
;;; the sphinx ...
;;; More precisely, a geometric substitutive is given by one or more shapes
;;; (typically polygons, not necessarily convex), that can be "cut" into
;;; several of the same, smaller shapes (trivial example would be how a square
;;; can be divided into four identical squares).
;;; The substitution is then defined as repeated inflation and division of an
;;; initial shape.
;;; CAVEAT: At the moment, substitutions can only involve a single shape
;;; (so e.g. Penrose, obtained by combining triangles & rhombuses, cannot be
;;; defined), and this shape cannot be mirrored (so e.g. the Sphinx, divided
;;; in 4 smaller sphinxes, cannot be defined as one of them is mirrored)

(defclass polygon ()
  ((vertices :reader polygon-vertices
             :initarg :vertices
             :type list)
   (origin :reader polygon-origin
           :initarg :origin
           :type point)
   (rotation :reader polygon-rotation
             :initarg :rotation
             :type double-float)))

(defclass shape-substitution ()
  ((initial :reader shape-initial
            :initarg :initial
            :type polygon)
   (expansion-factor :reader expansion-factor
                     :initarg :factor
                     :type point)
   (subdivision :reader shape-subdivison
                :initarg :subdivision
                :type list
                :documentation "List of POLYGON that subdivide this shape S.
The meaning is that when this shape S is expanded by its
EXPANSION-FACTOR, then each at position ORIGIN, we cut a shape POLYGON
in it.")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun make-polygon (vertices origin rotation)
    (make-instance 'polygon
                   :vertices vertices
                   :origin origin
                   :rotation rotation))

  (defun make-geometric-subdivision (&key initial factor subdivision)
    (let ((initial-points (mapcar 'point-ensure initial)))
      (make-instance 'shape-substitution
                     :initial (make-polygon initial-points (point 0 0) 0)
                     :factor (point-ensure factor)
                     :subdivision (mapcar (lambda (args)
                                            (make-polygon initial-points
                                                          (point-ensure (first args))
                                                          (second args)))
                                          subdivision)))))

(defmacro def-geometric-subdivision (&body description)
  (let ((keywords '(:initial :factor :subdivision)))
    (assert (member (car description) keywords)
            nil
            "Malformed description: should start with a valid keyword, not ~S"
            (car description))
    (let ((splitted (split-on-keywords description keywords)))
      (flet ((extract-argument (arg list)
               (cdr (find arg list :key 'car :test 'eq))))
        `(make-geometric-subdivision
          ,@(loop :for kw :in keywords
                  :if (eq kw :subdivision)
                    :collect kw
                    :and :collect `(list
                                    ,@(loop :for (pt angle)
                                              :in (extract-argument kw splitted)
                                            :collect `(list ',pt ,angle)))
                  :else
                    :collect kw
                    :and :collect (list 'quote (extract-argument kw splitted))))))))

(defun draw-polygon (polygon &key options)
  (let ((origin (polygon-origin polygon)))
    (with-point (x y) (polygon-origin polygon)
      (with-rotation (polygon-rotation polygon) (x y)
        (draw-long-path (mapcar (lambda (pt)
                                  (point+ pt origin))
                                (polygon-vertices polygon))
                        :cycle t
                        :options options)))))

(defmethod draw-substitution ((shape shape-substitution) n &key initial)
  (declare (ignore initial))
  (if (= n 1)
      (draw-polygon (shape-initial shape))
      (with-accessors ((factor expansion-factor)
                       (subdivision shape-subdivison))
          shape
        (let ((scaled-factor (point-exp factor (- n 2))))
          (dolist (sub-poly subdivision)
            (with-accessors ((rotation polygon-rotation)
                             (origin polygon-origin))
                sub-poly
              (with-point (ox oy) (point*pt origin scaled-factor)
                (with-shift (ox oy)
                  (with-rotation rotation ()
                    (draw-substitution shape (1- n)))))))))))
