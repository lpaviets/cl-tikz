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

(defun make-polygon (vertices origin rotation)
  (make-instance 'polygon
                 :vertices vertices
                 :origin origin
                 :rotation rotation))

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

(defun draw-polygon (polygon &key options)
  (let ((origin (polygon-origin polygon)))
    (with-point (x y) (polygon-origin polygon)
      (with-rotation (polygon-rotation polygon) (x y)
        (draw-long-path (mapcar (lambda (pt)
                                  (point+ pt origin))
                                (polygon-vertices polygon))
                        :cycle t
                        :options options)))))

(defun substitute-polygon (polygon shape-substitution)
  (with-accessors ((factor expansion-factor)
                   (subdivision shape-subdivison))
      shape-substitution
    (with-accessors ((vertices polygon-vertices)
                     (origin polygon-origin)
                     (rotation polygon-rotation))
        polygon
      (let ((scaled-origin (point*pt origin factor)))
        (mapcar (lambda (sub-poly)
                  (make-polygon (polygon-vertices sub-poly)
                                (point+ scaled-origin (polygon-origin sub-poly))
                                (+ rotation (polygon-rotation sub-poly))))
                subdivision)))))

(defun n-substitute-polygon (polygon shape-substitution n)
  (if (= n 1)
      (list polygon)
      (loop :for sub-poly :in (n-substitute-polygon polygon shape-substitution (1- n))
            :nconc (substitute-polygon sub-poly shape-substitution))))

(defmethod draw-substitution ((shape shape-substitution) n)
  (let ((polygons (n-substitute-polygon (shape-initial shape) shape n)))
    (map nil 'draw-polygon polygons)))
