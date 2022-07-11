(in-package #:cl-tikz/tiles)

;;; Solvers

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
             (aref (surface tiling) (point-y pos) (point-x pos))))
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
  (setf (aref (surface tiling) (point-y pos) (point-x pos)) val))

(defmacro dotiling ((pos tile &optional block-name) tiling &body body)
  "Iterate over the surface tiled by TILING. Locally binds POS to the current
position as a POINT, and TILE to the element of TILING at position POS,
or NIL otherwise.
Starts at the bottom-left corner (pos (0, 0)), and iterates row-wise.
Wraps the iteration in a block called BLOCK-NAME"
  (let ((height (gensym "HEIGHT"))
        (h (gensym "H"))
        (width (gensym "WIDTH"))
        (w (gensym "W"))
        (gtiling (gensym "GTILING"))
        (surface (gensym "SURFACE"))
        (bounds (gensym "BOUNDS"))
        (oob (gensym "OOB")))
    `(block ,block-name
      (let ((,gtiling ,tiling))
        (with-accessors ((,surface surface)
                         (,bounds bounds))
            ,gtiling
          (destructuring-bind (,height ,width) (array-dimensions ,surface)
            (loop :for ,h :below ,height :do
              (loop :for ,w :below ,width
                    :for ,pos = (point ,w ,h)
                    :for ,tile = (tiling-tile-at ,pos ,gtiling :out-of-bounds ',oob)
                    :unless (eq ',oob ,tile)
                      :do ,@body))))))))

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
  (dotiling (pos tile block-iter) tiling
    (unless tile
      (return-from block-iter pos))))

(defun nshuffle (sequence)
  (loop :for i :from (length sequence) :downto 2
        :do (rotatef (elt sequence (random i))
                     (elt sequence (1- i))))
  sequence)

(defun solve-naive (tiling &key random)
  "Solve TILING using a naive approach, similar to a DFS.
If RANDOM is non-nil, the nodes are explored in a random order, i.e.
tiles are tried non-deterministically.
Otherwise, the order is defined by (DOTILES (TILE (TILESET TILING)) ...)"
  (loop :with copied-tiling = (copy-tiling tiling)
        :with history = ()
        :for empty-pos = (find-empty-cell copied-tiling)
        :for possible-tiles = (when empty-pos (find-all-valid-tiles empty-pos copied-tiling))
        ;; If we have no empty position, our job is done
        :unless empty-pos
          :do (return (tiling-valid-p copied-tiling))
        :when random
          :do (setf possible-tiles (nshuffle possible-tiles))
        :when possible-tiles       ; If we can continue the tiling ...
          :do (setf (tiling-tile-at empty-pos copied-tiling) (car possible-tiles))
              ;; We store the other tiles in HISTORY in case we chose
              ;; wrong
              (push (cons empty-pos (cdr possible-tiles)) history)
        :else
          :do (unless history       ; if we can't back-up, we are done
                (return (tiling-valid-p copied-tiling)))
              (loop :for ((prev-pos . other-tiles) . rest) :on history
                    :until other-tiles
                    ;; We made the wrong choice: we reset the tiling
                    :do (setf (tiling-tile-at prev-pos copied-tiling) nil)
                        ;; We also remove this entry from the history
                        (pop history))
              ;; We now need to take an alternate choice
              (let ((next-tile (pop (cdar history))))
                ;; We update the tiling with another choice
                (setf (tiling-tile-at (caar history) copied-tiling) next-tile))))
