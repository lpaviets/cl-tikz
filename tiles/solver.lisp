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
          (right-constraint (when (< y (1- n))
                              (invalid-tiles-left tileset (aref grid x (1+ y)))))
          (down-constraint (when (> x 0)
                             (invalid-tiles-up tileset (aref grid (1- x) y))))
          (up-constraint (when (< x (1- m))
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

(defun nshuffle (sequence)
  (loop :for i :from (length sequence) :downto 2
        :do (rotatef (elt sequence (random i))
                     (elt sequence (1- i))))
  sequence)

(defun %solver-naive (tileset grid &optional random)
  (let ((empty-cell (solver-find-empty-cell grid)))
    (if empty-cell
        (let* ((valid-tiles-1 (apply 'all-valid-tiles tileset grid empty-cell))
               (valid-tiles (if random (nshuffle valid-tiles-1) valid-tiles-1)))
          (loop :for tile :in valid-tiles
                :for new-grid = (set-grid-pos-from grid
                                                   (car empty-cell)
                                                   (cadr empty-cell)
                                                   tile)
                :for result = (%solver-naive tileset new-grid)
                :when result
                  :do (return-from %solver-naive result)))
        (and (solver-check tileset grid) grid))))

(defun solver-naive (tileset dims &optional random)
  (let ((grid (make-array dims :initial-element nil)))
    (%solver-naive tileset grid random)))

;;; New

(defclass tiling ()
  ((tileset :initarg :tileset
            :reader tileset
            :type (or tileset symbol))
   (surface :initarg :surface
            :reader surface
            :type array)
   (bounds :initarg :bounds
           :reader bounds
           :type function)
   (get-tile-fun :initarg :tile-fun
                 :reader get-tile-fun
                 :type function
                 :documentation "Function of two arguments, a TILING and a POINT. Can assume that
POINT is in the bounds of TILING, and should return the tile located at
position POINT in TILING, or NIL if no such tile exists."))
  (:default-initargs
   :tileset (error "A tileset is required to define a tiling")
   :surface (error "A surface is required to define a tiling")
   :bounds (error "A bounding function is required to define a tiling")))

(defun make-tiling-grid (tileset width height)
  "Return a new empty tiling, using TILESET as its tileset, and which
tiles an WIDTHxHEIGHT rectangular grid"
  (let ((grid (make-array (list height width) :initial-element nil)))
    (flet ((grid-bounds (pos)
             (with-point (x y) pos
               (and (<= 0 x (1- width))
                    (<= 0 y (1- height)))))
           (grid-tile-fun (tiling pos)
             (aref (surface tiling) (point-x pos) (point-y pos))))
      (make-instance 'tiling
                     :tileset tileset
                     :surface grid
                     :bounds #'grid-bounds
                     :tile-fun #'grid-tile-fun))))

(defun copy-tiling (tiling)
  (with-accessors ((tileset tileset)
                   (surface surface)
                   (bounds bounds)
                   (tile-fun get-tile-fun))
      tiling
    (let ((new-surface (make-array (array-dimensions surface))))
      (loop :for idx :below (array-total-size surface)
            :do (setf (row-major-aref new-surface idx)
                      (row-major-aref surface idx)))
      (make-instance 'tiling
                     :tileset tileset
                     :surface new-surface
                     :bounds bounds
                     :tile-fun tile-fun))))

(defun in-tiling-bounds-p (pos tiling)
  (funcall (bounds tiling) pos))

(defun tiling-tile-at (pos tiling &key out-of-bounds)
  "Return the tile at position POS in TILING.
If no tile is present, return NIL.
If POS is out of the bounds of TILING, return OUT-OF-BOUNDS"
  (if (in-tiling-bounds-p pos tiling)
      (funcall (get-tile-fun tiling) tiling pos)
      out-of-bounds))

(defun (setf tiling-tile-at) (val pos tiling)
  (assert (in-tiling-bounds-p pos tiling) ()
          "Cannot set tile at ~S in tiling ~S~%" pos tiling)
  (setf (aref (surface tiling) (point-x pos) (point-y pos)) val))

(defmacro dotiling ((pos tile) tiling &body body)
  "Iterate over the surface tiled by TILING. Locally binds POS to the current
position as a POINT, and TILE to the element of TILING at position POS,
or NIL otherwise.
Starts at the bottom-left corner (pos (0, 0)), and iterates row-wise."
  (let ((height (gensym "HEIGHT"))
        (h (gensym "H"))
        (width (gensym "WIDTH"))
        (w (gensym "W"))
        (gtiling (gensym "GTILING"))
        (surface (gensym "SURFACE"))
        (bounds (gensym "BOUNDS"))
        (oob (gensym "OOB")))
    `(block nil
      (let ((,gtiling ,tiling))
        (with-accessors ((,surface surface)
                         (,bounds bounds))
            ,gtiling
          (destructuring-bind (,height ,width) ,surface
            (loop :for ,h :below ,height :do
              (loop :for ,w :below ,width
                    :for ,pos = (point ,w ,h)
                    :for ,tile = (tiling-tile-at ,pos ,gtiling :out-of-bounds ',oob)
                    :unless (eq ',oob ,tile)
                      :do ,@body))))))))

;; (defun show-tiling (tiling &optional (out-of-bounds #\*))
;;   (with-accessors ((tileset tileset)
;;                    (surface surface)
;;                    (bounds bounds))
;;       tiling
;;     (format t "~&Showing tiling:~%Tileset: ~S~%Tiled surface:~%" (tileset-name tileset))
;;     (destructuring-bind (height width) (array-dimensions surface)
;;       (loop :for j :from (1- height) :to 0 :do
;;         (loop :for i :below width
;;               :for tile = (tiling-tile-at (point i j) tiling :out-of-bounds :oob)
;;               :)))))

(defun tiling-neighbours-of (pos tiling)
  "List of four neighbours of POS in the tiling TILING
In order left, down, right, up.
If some neighbour is unspecified in TILING, or if some neighbour
would fall outside the bounds of the tiling, use NIL instead."
  (loop :for  pt :in (point-neighbours pos)
        :collect (tiling-tile-at pt tiling)))

(defun tiling-valid-p (tiling)
  (dotiling (pos tile) tiling
    (unless (apply #'tile-fits-with-p tile (tiling-neighbours-of pos tiling))
      (return-from tiling-valid-p nil)))
  tiling)

(defun find-all-valid-tiles (pos tiling)
  (let (valid)
    (dotiles (tile (tileset tiling))
      (when (apply #'tile-fits-with-p tile (tiling-neighbours-of pos tiling))
        (push tile valid)))
    valid))

(defun find-empty-cell (tiling)
  "Return NIL if it does not contain any NIL entry, and return a empty
position in the bounds of tiling as a POINT"
  (dotiling (pos tile) tiling
    (unless tile
      (return pos))))

(defun solve-naive (tiling &key random)
  "Solve TILING using a naive approach, similar to a DFS.
If RANDOM is non-nil, the nodes are explored in a random order, i.e.
tiles are tried non-deterministically.
Otherwise, the order is defined by (DOTILES (TILE (TILESET TILING)) ...)"
  (loop :with copied-tiling = (copy-tiling tiling)
        :with history = ()
        :for empty-pos = (find-empty-cell copied-tiling)
        :for possible-tiles = (when empty-pos (find-all-valid-tiles empty-pos copied-tiling))
        :when random
          :do (setf possible-tiles (nshuffle possible-tiles))
        :if possible-tiles         ; If we can continue the tiling ...
          :do (setf (tiling-tile-at empty-pos copied-tiling) (car possible-tiles))
              ;; We store the other tiles in HISTORY in case we chose
              ;; wrong
              (push (cons empty-pos (cdr possible-tiles)) history)
        :else
          :do (unless history ; if we can't back-up, we are done
                (return (tiling-valid-p copied-tiling)))
              (loop :for ((prev-pos other-tiles) . rest) :on history
                    :until other-tiles
                    ;; We made the wrong choice: we reset the tiling
                    :do (setf (tiling-tile-at prev-pos copied-tiling) nil)
                        ;; We also remove this entry from the history
                        (pop history))
              ;; We now need to take an alternate choice
              (let ((next-tile (pop (cdar history))))
                ;; We update the tiling with another choice
                (setf (tiling-tile-at (caar history) copied-tiling) next-tile))))
