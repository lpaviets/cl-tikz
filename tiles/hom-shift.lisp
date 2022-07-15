(in-package #:cl-tikz/tiles)

(defclass hom-shift-tile (tile)
  ((colour :initarg :colour
           :reader colour)
   (valid-neighbours :initarg :neighbours
                     :reader valid-neighbours
                     :documentation "A list of valid colours that can be placed
next to this tile."))
  (:default-initargs
   :colour (error "Must specify a colour when defining a Hom-Shift tile")))

;;; Hom-Shift
(defmethod make-tile-drawing-function ((tile hom-shift-tile) &optional (turns 0))
  (def-drawing-function (turns)
    (draw-square -0.5 -0.5 :options `((fill . ,(colour tile))))))

(defun make-hom-shift-tile (tileset colour valid-neighbours)
  (make-instance 'hom-shift-tile
                 :tileset tileset
                 :colour colour
                 :neighbours valid-neighbours))

(defun make-hom-shift-from-edges (name edges)
  (let ((all-colours (make-hash-table :test 'equal)))
    (loop :for (x y) :in edges
          :do (pushnew y (gethash x all-colours))
              (pushnew x (gethash y all-colours)))
    (make-tileset name
                  (let (tiles)
                    (dohash (colour neighbours) all-colours
                      (push (make-hom-shift-tile name
                                                 colour
                                                 neighbours)
                            tiles))
                    (list-to-set tiles)))))

(defmacro defhomshift (name &body edges)
  `(make-hom-shift-from-edges ',name ',edges))

(defun make-chessboard-shift (name colours)
  (make-hom-shift-from-edges name (loop :for (col-a . rest) :on colours
                                        :append (loop :for col-b :in rest
                                                      :collect (list col-a col-b)))))

(defmethod valid-neighbour-p ((t1 hom-shift-tile) (t2 hom-shift-tile) dir)
  (declare (ignore dir))
  (member (colour t2) (valid-neighbours t1)))

(defmethod make-rotated-tile ((tile hom-shift-tile) turns)
  (declare (ignore turns))
  (make-instance 'hom-shift-tile
                 :tileset (tileset tile)
                 :colour (colour tile)
                 :neighbours (valid-neighbours tile)))
