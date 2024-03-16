(in-package #:org.numbra.cl-tikz-tilings)

;;; TODO : Major refactor ? Content and topology are mixed here

(defclass topology ()
  ((%content :initarg :content
             :accessor %content
             :documentation "Internal.
The content of the structure.")
   (bounds :initarg :bounds
           :reader bounds
           :type function
           :documentation "Function of one argument, a position POS.

POS is the position as seen by the outside world, and can be arbitrary.

Should return T if POINT is in the bounds of the topology, NIL otherwise.")
   (%get-tile-fun :initarg :tile-fun
                 :reader %get-tile-fun
                 :type function
                 :documentation "Internal.
Function of a single argument, a position POS.

POs is the position as seen by the outside world, and can be arbitrary.

The function should return a POINT corresponding to the position in CONTENT of
this required position.

For example, in a toric 10x10 grid, on input (POINT 123 -6), the function should
return (POINT 3 4), provided the dimensions are stored in the same order."))
  (:default-initargs
   :content (error "A content is required to define a tiling")
   :bounds (error "A bounding function is required to define a tiling")))

(defun topology-tile-at (topology position)
  (when (funcall (bounds topology) position)
    (with-point (x y) (funcall (%get-tile-fun topology) position)
      (aref (%content topology) y x))))

(defun (setf topology-tile-at) (val topology position)
  (when (funcall (bounds topology) position)
    (with-point (x y) (funcall (%get-tile-fun topology) position)
      (setf (aref (%content topology) y x) val))))

(defun copy-topology (topology)
  (with-accessors ((content %content)
                   (bounds bounds)
                   (tile-fun %get-tile-fun))
      topology
    (let ((new-content (make-array (array-dimensions content))))
      (loop :for idx :below (array-total-size content)
            :do (setf (row-major-aref new-content idx)
                      (row-major-aref content idx)))
      (make-instance 'topology
                     :content new-content
                     :bounds bounds
                     :tile-fun tile-fun))))

(defun make-topology-grid (width height)
  (let ((grid (make-array (list height width) :initial-element nil)))
    (flet ((grid-bounds (pos)
             (with-point (x y) pos
               (and (<= 0 x (1- width))
                    (<= 0 y (1- height)))))
           (grid-tile-fun (pos)
             pos))
      (make-instance 'topology
                     :content grid
                     :bounds #'grid-bounds
                     :tile-fun #'grid-tile-fun))))

(defun make-topology-torus (width height)
  (let ((torus (make-array (list height width) :initial-element nil)))
    (flet ((torus-tile-fun (pos)
             (with-point (x y) pos
               (point (mod (point-x pos) width)
                      (mod (point-y pos) height)))))
      (make-instance 'topology
                     :content torus
                     :bounds (constantly t) ;; torus is unbounded
                     :tile-fun #'torus-tile-fun))))
