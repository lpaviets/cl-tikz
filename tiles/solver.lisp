(in-package #:cl-tikz/tiles)

;;; Solvers

(defun invalid-tiles-right (tileset grid x y)
  (let ((size (tileset-size tileset))
        (rules (rules-right (tileset-rules tileset)))
        (tile (aref grid x y)))
    (loop :for j :below size
          :unless (aref rules tile j)
            :collect j)))

(defun invalid-tiles-left (tileset grid x y)
  (let ((size (tileset-size tileset))
        (rules (rules-right (tileset-rules tileset)))
        (tile (aref grid x y)))
    (loop :for i :below size
          :unless (aref rules i tile)
            :collect i)))

(defun invalid-tiles-up (tileset grid x y)
  (let ((size (tileset-size tileset))
        (rules (rules-up (tileset-rules tileset)))
        (tile (aref grid x y)))
    (loop :for j :below size
          :unless (aref rules tile j)
            :collect j)))

(defun invalid-tiles-down (tileset grid x y)
  (let ((size (tileset-size tileset))
        (rules (rules-up (tileset-rules tileset)))
        (tile (aref grid x y)))
    (loop :for i :below size
          :unless (aref rules i tile)
            :collect i)))

(defun invalid-tiles-side (tileset grid x y side)
  (ecase side
    (:left (valid-tiles-left tileset grid x y))
    (:right (valid-tiles-right tileset grid x y))
    (:down (valid-tiles-down tileset grid x y))
    (:up (valid-tiles-up tileset grid x y))))

(defun all-invalid-tiles (tileset grid x y)
  (destructuring-bind (m n) (array-dimensions grid)
    (let ((left-constraint (when (> x 0)
                             (invalid-tiles-right tileset grid (1- x) y)))
          (right-constraint (when (< x (1- m))
                              (invalid-tiles-left tileset grid (1+ x) y)))
          (down-constraint (when (> y 0)
                             (invalid-tiles-up tileset grid x (1- y))))
          (up-constraint (when (< x (1- n))
                           (invalid-tiles-down tileset grid x (1+ y)))))

      (nconc left-constraint right-constraint down-constraint up-constraint))))

(defun all-valid-tiles (tileset grid x y)
  (let* ((size (tileset-size tileset))
         (valid-tiles (make-array size :initial-element t)))
    (loop :for tile :in (all-invalid-tiles tileset grid x y)
          :do (setf (aref valid-tiles tile) nil))
    (loop :for tile :across valid-tiles
          :when tile
            :collect tile)))

(defun solver-find-empty-cell (grid)
  "Return NIL if it does not contain any NIL entry, and return a position
a list (i j) where (aref i j GRID) is NIL otherwise"
  (destructuring-bind (m n) (array-dimensions grid)
    (loop :for i :below m
          :for error = (loop :for j :below n
                             :for cell = (aref grid i j)
                             :while cell
                             :finally (return (and (not cell) j)))
          :until error
          :finally (return (when error (list i error))))))

(defun solver-naive (tileset grid)
  (let ((empty-cell (solver-find-empty-cell grid)))
    (if empty-cell
        (progn
          )
        grid)))
