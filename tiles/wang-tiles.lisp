(in-package #:cl-tikz/tiles)

;;; Tiles

(defclass tile ()
  ((tileset :initarg :tileset
            :reader tileset
            :type (or tileset symbol)
            :documentation "The tileset to which this tile belongs")
   (draw-function :initarg :draw-function
                  :accessor draw-function
                  :type function
                  :documentation "Function of one argument, a POINT (X, Y),
 used to draw the tile at the position (X, Y)
A tile has to implement the following methods to be used in the solver:
- VALID-NEIGHBOUR-P
A class deriving from TILE can also implement the following helper methods
to be able to define tiles more easily:
- MAKE-ROTATED-TILE"))
    (:default-initargs
     :tileset (error "A tileset is required for each Wang tile")))

(defclass wang-tile (tile)
  ((sides :initarg :sides
          :type (array t (4))
          :reader sides
          :documentation "This slot represents those sides in order left, down,
right and up."))
  (:documentation "Class representing Wang tiles.
A tile is said to be a Wang Tile if it is only defined by its sides, which
in turn determine the valid ajaacency rules.")
  (:default-initargs
   :tileset (error "A tileset is required for each Wang tile")
   :sides nil))

(defclass variant-wang-tile (tile)
  ((sides :initarg :sides
          :type (array t (4))
          :reader sides
          :documentation "This slot represents the sides in order left, down,
right and up. Those keywords simply represent the side of the tile, and
the valid pairings are given by the 'valid-neighbours' slot")
   (valid-neighbours :initarg :neighbours
                     :type (array hash-table (4))
                     :reader valid-neighbours
                     :documentation "An array of four hash-tables.
Each one corresponds to a direction, in order left, down, right, up.
Each hash-table contains the 'colours' of the tiles that can be placed
next to this tile, in the corresponding direction.")))

(defun make-wang-tile (tileset left down right up &key draw)
  (let ((sides (make-array 4
                           :element-type 'keyword
                           :initial-contents (list left down right up)))
        (draw-fun (or draw
                      (make-wang-drawing-function left down right up))))
    (make-instance 'wang-tile
                   :tileset tileset
                   :sides sides
                   :draw-function draw-fun)))


(defgeneric make-rotated-tile (tile turns)
  (:documentation "Create a tile that is a copy of TILE, rotated counter-clockwise
by TURNS quarter-turns."))

(defmethod make-rotated-tile ((tile wang-tile) turns)
  (with-accessors ((tileset tileset)
                   (draw-function draw-function)
                   (sides sides))
      tile
    (let ((draw-fun (lambda (pos)
                      (with-rotation (* turns (/ pi 2)) ((point-x pos) (point-y pos))
                        (funcall draw-function pos)))))
      (make-instance 'wang-tile
                     :tileset tileset
                     :sides (rotate-sequence sides turns)
                     :draw-function draw-fun))))

(defmethod make-rotated-tile ((tile variant-wang-tile) turns)
  (with-accessors ((tileset tileset)
                   (draw-function draw-function)
                   (sides sides)
                   (valid-neighbours valid-neighbours))
      tile
    (let ((draw-fun (lambda (pos)
                      (with-rotation (* turns (/ pi 2)) ((point-x pos) (point-y pos))
                        (funcall draw-function pos)))))
      (make-instance 'variant-wang-tile
                     :tileset tileset
                     :sides (rotate-sequence sides turns)
                     :draw-function draw-fun
                     :neighbours (rotate-sequence valid-neighbours turns)))))

(declaim (inline side-to-digit))
(defun side-to-digit (dir)
  (ccase dir
    ((:left :l 0)  0)
    ((:down :d 1)  1)
    ((:right :r 2) 2)
    ((:up :u 3)    3)))

(declaim (inline tile-colour))
(defun tile-colour (tile dir)
  (aref (sides tile) (side-to-digit dir)))

(defmacro with-tile-sides ((left down right up) tile &body body)
  (with-gensyms (gtile)
    `(let* ((,gtile ,tile))
       (let ((,left (tile-colour ,gtile :left))
             (,down (tile-colour ,gtile :down))
             (,right (tile-colour ,gtile :right))
             (,up (tile-colour ,gtile :up)))
         (declare (ignorable ,left ,down ,right ,up))
         ,@body))))

(defun make-wang-drawing-function (left down right up &optional (turns 0))
  (lambda (pos)
    (with-rotation (* turns (/ pi 2)) ((point-x pos) (point-y pos))
      (draw-wang-tile (point-x pos) (point-y pos) left down right up))))

(defgeneric valid-neighbour-p (t1 t2 dir)
  (:method :around ((t1 tile) (t2 tile) dir)
    (setf dir (side-to-digit dir))
    (call-next-method t1 t2 dir))
  (:method ((t1 tile) (t2 tile) dir)
    (error "~&No adjacency rule defined for tiles ~S and ~S~%" t1 t2)))

(defmethod valid-neighbour-p ((t1 wang-tile) (t2 wang-tile) dir)
  (with-tile-sides (l1 d1 r1 u1) t1
    (with-tile-sides (l2 d2 r2 u2) t2
      (ccase dir
        (0 (equal l1 r2))
        (1 (equal d1 u2))
        (2 (equal r1 l2))
        (3 (equal u1 d2))))))

(defmethod valid-neighbour-p ((t1 variant-wang-tile) (t2 variant-wang-tile) dir)
  (with-tile-sides (l1 d1 r1 u1) t1
    (let ((allowed-colours (aref (valid-neighbours t2) dir))
          (colour-to-check (tile-colour t1 dir)))
      (gethash colour-to-check allowed-colours))))
