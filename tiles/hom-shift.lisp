(in-package #:cl-tikz/tiles)

;;; Hom-Shift
(defun make-hom-shift-drawing-function (colour)
  (lambda (pos)
    (with-point (x y) pos
     (draw-square (- x 0.5) (- y 0.5) :options `((fill . ,colour))))))

(defun make-hom-shift-tile (tileset colour valid-neighbours &key draw)
  (let* ((sides (make-array 4 :initial-element colour))
         (draw-fun (or draw
                       (make-hom-shift-drawing-function colour)))
         (valid-neighbours-table (list-to-set valid-neighbours))
         (valid-neighbours-array (make-array 4 :initial-element valid-neighbours-table)))
    (dolist (nghb valid-neighbours))
    (make-instance 'variant-wang-tile
                   :tileset tileset
                   :sides sides
                   :draw-function draw-fun
                   :neighbours valid-neighbours-array)))


(defun make-hom-shift-from-edges (name edges)
  (let ((all-colours (make-hash-table)))
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
