(in-package #:org.numbra.cl-tikz-tilings)

;;; Tiling

(defclass tiling ()
  ((tileset :initarg :tileset
            :reader tileset
            :type (or tileset symbol))
   (topology :initarg :topology
             :reader topology
             :type topology))
  (:default-initargs
   :tileset (error "A tileset is required to define a tiling")
   :topology (error "A topology is required to define a tiling")))

(defun make-tiling-grid (tileset width height &rest args)
  "Return a new empty tiling, using TILESET as its tileset, and which
tiles an WIDTHxHEIGHT rectangular grid"
  (let ((grid (make-array (list height width) :initial-element nil)))
    (flet ((grid-bounds (pos)
             (with-point (x y) pos
               (and (<= 0 x (1- width))
                    (<= 0 y (1- height)))))
           (grid-tile-fun (tiling pos)
             (aref (topology tiling) (point-y pos) (point-x pos))))
      (apply #'make-instance
             'tiling
             :tileset tileset
             :topology grid
             :bounds #'grid-bounds
             :tile-fun #'grid-tile-fun
             args))))

(defun make-tiling-torus (tileset width height &rest args)
  "Return a new empty tiling, using TILESET as its tileset, and which
tiles an WIDTHxHEIGHT torus."
  (let ((torus (make-array (list height width) :initial-element nil)))
    (flet ((torus-tile-fun (tiling pos)
             (aref (topology tiling)
                   (mod (point-y pos) height)
                   (mod (point-x pos) width))))
      (apply #'make-instance
             'tiling
             :tileset tileset
             :topology torus
             :bounds (constantly t)
             :tile-fun #'torus-tile-fun
             args))))

(defun copy-tiling (tiling)
  (with-accessors ((tileset tileset)
                   (topology topology)
                   (bounds bounds)
                   (tile-fun get-tile-fun))
      tiling
    (let ((new-topology (make-array (array-dimensions topology))))
      (loop :for idx :below (array-total-size topology)
            :do (setf (row-major-aref new-topology idx)
                      (row-major-aref topology idx)))
      (make-instance 'tiling
                     :tileset tileset
                     :topology new-topology
                     :bounds bounds
                     :tile-fun tile-fun))))

(defun in-tiling-bounds-p (pos tiling)
  (funcall (bounds tiling) pos))

(defun tiling-tile-at (pos tiling &key out-of-bounds no-check)
  "Return the tile at position POS in TILING.
If no tile is present, return NIL.
If POS is out of the bounds of TILING, return OUT-OF-BOUNDS
If NO-CHECK is non-NIL, don't test for out-of-boundness."
  (if (or no-check (in-tiling-bounds-p pos tiling))
      (funcall (get-tile-fun tiling) tiling pos)
      out-of-bounds))

(defun (setf tiling-tile-at) (val pos tiling &key no-check)
  (assert (or no-check (in-tiling-bounds-p pos tiling)) ()
          "Cannot set tile at ~S in tiling ~S~%" pos tiling)
  (setf (aref (topology tiling) (point-y pos) (point-x pos)) val))

(defmacro dotiling ((pos tile &optional block-name) tiling &body body)
  "Iterate over the topology tiled by TILING. Locally binds POS to the current
position as a POINT, and TILE to the element of TILING at position POS,
or NIL otherwise.
Starts at the bottom-left corner (pos (0, 0)), and iterates row-wise.
Wraps the iteration in a block called BLOCK-NAME"
  (with-gensyms (height h width w (gtiling tiling) topology bounds oob)
    `(block ,block-name
       (with-accessors ((,topology topology)
                        (,bounds bounds))
           ,gtiling
         (destructuring-bind (,height ,width) (array-dimensions ,topology)
           (loop :for ,h :below ,height :do
             (loop :for ,w :below ,width
                   :for ,pos = (point ,w ,h)
                   :for ,tile = (tiling-tile-at ,pos ,gtiling :out-of-bounds ',oob)
                   :unless (eq ',oob ,tile)
                     :do ,@body)))))))

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
