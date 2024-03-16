(in-package #:org.numbra.cl-tikz-tilings)

(defclass hom-shift-tile (tile)
  ((colour :initarg :colour
           :reader colour)
   (vertex :initarg :vertex
           :reader vertex-name)
   (valid-neighbours :initarg :neighbours
                     :reader valid-neighbours
                     :documentation "A list of valid vertices that can be placed
next to this tile."))
  (:default-initargs
   :colour (error "Must specify a colour when defining a Hom-Shift tile")))

;;; Hom-Shift
(defmethod make-tile-drawing-function ((tile hom-shift-tile))
  (def-drawing-function ()
    (with-tikz-command (fill :options `((fill . ,(colour tile))) :newline nil)
      (format t " ~A rectangle ~A" (point-str -0.5 -0.5) (point-str 0.5 0.5)))
    (format t " % ~A~%" (vertex-name tile))))

(defun make-hom-shift-tile (tileset vertex colour valid-neighbours)
  (make-instance 'hom-shift-tile
                 :tileset tileset
                 :vertex vertex
                 :colour colour
                 :neighbours valid-neighbours))

(defun make-hom-shift-from-edges (name edges &optional colours)
  "Create a Hom-Shift named NAME.

EDGES is a list of pairs (U V) of vertices.

COLOURS is a list of pairs (COLOUR . VERTEX*), where:
- VERTEX* is a list of vertices, or T.
- COLOUR is the colour with which it will be drawn.

The pair (COLOUR . T) can only appear once, and should be last in the list
COLOURS. It acts as a default for vertices not appearing in any other pair.

If a vertex U does not appear in COLOURS, and COLOURS does not contain a
pair (COLOUR . T), it will be coloured with the colour named U."
  (let ((vertices (make-hash-table :test 'equal))
        (tiles nil))
    (loop :for (x y) :in edges
          :do (pushnew y (gethash x vertices) :test #'equal)
              (pushnew x (gethash y vertices) :test #'equal))
    (dohash (vertex neighbours) vertices
      (let ((colour (car (rassoc-if (lambda (v)
                                      (or (eq v t)
                                          (member vertex v)))
                                    colours))))
        (push (make-hom-shift-tile name
                                   vertex
                                   (or colour vertex)
                                   neighbours)
              tiles)))
    (make-tileset name (list-to-set tiles))))

;; TODO Make faster: the hom-shift could remember that info ...
(defun hom-shift-tile-from-vertex (vertex tileset &key (test 'eql))
  (dotiles (tile tileset)
    (when (funcall test vertex (vertex-name tile))
      (return-from hom-shift-tile-from-vertex tile))))

(defmacro defhomshift (name colours &body edges)
  "See `make-hom-shift-from-edges'"
  `(make-hom-shift-from-edges ',name ',edges ',colours))

(defun make-chessboard-shift (name colours)
  (make-hom-shift-from-edges name (loop :for (col-a . rest) :on colours
                                        :append (loop :for col-b :in rest
                                                      :collect (list col-a col-b)))))

(defmethod valid-neighbour-p ((t1 hom-shift-tile) (t2 hom-shift-tile) dir)
  (declare (ignore dir))
  (member (vertex-name t2) (valid-neighbours t1)))

(defmethod make-rotated-tile ((tile hom-shift-tile) turns)
  (declare (ignore turns))
  (make-instance 'hom-shift-tile
                 :tileset (tileset tile)
                 :colour (colour tile)
                 :neighbours (valid-neighbours tile)))
