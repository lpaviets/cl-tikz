(in-package #:cl-tikz/tiles)

;;; Solvers

(defun invalid-tiles-right (tileset tile)
  (when tile
    (let ((size (tileset-size tileset))
          (rules (rules-right (tileset-rules tileset))))
      (loop :for j :below size
            :unless (aref rules tile j)
              :collect j))))

(defun invalid-tiles-left (tileset tile)
  (when tile
    (let ((size (tileset-size tileset))
          (rules (rules-right (tileset-rules tileset))))
      (loop :for i :below size
            :unless (aref rules i tile)
              :collect i))))

(defun invalid-tiles-up (tileset tile)
  (when tile
    (let ((size (tileset-size tileset))
          (rules (rules-up (tileset-rules tileset))))
      (loop :for j :below size
            :unless (aref rules tile j)
              :collect j))))

(defun invalid-tiles-down (tileset tile)
  (when tile
    (let ((size (tileset-size tileset))
          (rules (rules-up (tileset-rules tileset))))
      (loop :for i :below size
            :unless (aref rules i tile)
              :collect i))))

(defun all-invalid-tiles (tileset grid x y)
  (destructuring-bind (m n) (array-dimensions grid)
    (let ((left-constraint (when (> y 0)
                             (invalid-tiles-right tileset (aref grid x (1- y)))))
          (right-constraint (when (< y (1- m))
                              (invalid-tiles-left tileset (aref grid x (1+ y)))))
          (down-constraint (when (> x 0)
                             (invalid-tiles-up tileset (aref grid (1- x) y))))
          (up-constraint (when (< x (1- n))
                           (invalid-tiles-down tileset (aref grid (1+ x) y)))))

      (nconc left-constraint right-constraint down-constraint up-constraint))))

(defun all-valid-tiles (tileset grid x y)
  (let* ((size (tileset-size tileset))
         (valid-tiles (make-array size :initial-element t)))
    (loop :for tile :in (all-invalid-tiles tileset grid x y)
          :do (setf (aref valid-tiles tile) nil))
    (loop :for i :below size
          :for tile = (aref valid-tiles i)
          :when tile
            :collect i)))

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

(defun solver-check (tileset grid)
  (destructuring-bind (m n) (array-dimensions grid)
    (loop :for i :below m
          :always
          (loop :for j :below n
                :for cell = (aref grid i j)
                :for valid = (all-valid-tiles tileset grid i j)
                :always
                (member cell valid)))))

(defun set-grid-pos-from (grid x y val)
  "Return a new grid, all cells of which are equal to the ones of GRID,
except in position (X Y) where it is equal to VAL"
  (destructuring-bind (m n) (array-dimensions grid)
   (let ((new-grid (make-array (list m n))))
     (loop :for i :below m :do
           (loop :for j :below n :do
             (setf (aref new-grid i j) (aref grid i j))))
     (setf (aref new-grid x y) val)
     new-grid)))

(defun %solver-naive (tileset grid)
  (let ((empty-cell (solver-find-empty-cell grid)))
    (if empty-cell
        (let ((valid-tiles (apply 'all-valid-tiles tileset grid empty-cell)))
          (loop :for tile :in valid-tiles
                :for new-grid = (set-grid-pos-from grid
                                                   (car empty-cell)
                                                   (cadr empty-cell)
                                                   tile)
                :for result = (%solver-naive tileset new-grid)
                :when result
                  :do (return-from %solver-naive result)))
        (and (solver-check tileset grid) grid))))

(defun solver-naive (tileset dims)
  (let ((grid (make-array dims :initial-element nil)))
    (%solver-naive tileset grid)))
