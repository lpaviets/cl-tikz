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

;;; Forward some calls to the topology
(defmethod bounds ((tiling tiling))
  (bounds (topology tiling)))

;;; Creation of common tilings
(defun make-tiling-grid (tileset width height &rest args)
  "Return a new empty tiling, using TILESET as its tileset, and which
tiles an WIDTHxHEIGHT rectangular grid"
  (apply #'make-instance
         'tiling
         :tileset tileset
         :topology (make-topology-grid width height)
         args))

(defun make-tiling-torus (tileset width height &rest args)
  "Return a new empty tiling, using TILESET as its tileset, and which
tiles an WIDTHxHEIGHT torus."
  (apply #'make-instance
         'tiling
         :tileset tileset
         :topology (make-topology-torus width height)
         args))

;;; Utilities
(defun copy-tiling (tiling)
  (with-accessors ((tileset tileset)
                   (topology topology))
      tiling
    (make-instance 'tiling
                   :tileset tileset
                   :topology (copy-topology topology))))

(defun in-tiling-bounds-p (pos tiling)
  (funcall (bounds tiling) pos))

(defun tiling-tile-at (pos tiling &key out-of-bounds no-check)
  "Return the tile at position POS in TILING.
If no tile is present, return NIL.
If POS is out of the bounds of TILING, return OUT-OF-BOUNDS
If NO-CHECK is non-NIL, don't test for out-of-boundness."
  (if (or no-check (in-tiling-bounds-p pos tiling))
      (topology-tile-at (topology tiling) pos)
      out-of-bounds))

(defun (setf tiling-tile-at) (val pos tiling &key no-check)
  (assert (or no-check (in-tiling-bounds-p pos tiling)) ()
          "Cannot set tile at ~S in tiling ~S~%" pos tiling)
  (setf (topology-tile-at (topology tiling) pos) val))

;; TODO: refactor, a tiling does not know how to iterate over itself, only the
;; topology can.
;;
;; Same for function asking for neighbours, etc
(defmacro dotiling ((pos tile &optional block-name) tiling &body body)
  "Iterate over the topology tiled by TILING.

Locally binds POS to the current position as a POINT, and TILE to the element
of TILING at position POS, or NIL otherwise.

Starts at the bottom-left corner (pos (0, 0)), and iterates row-wise.

Wraps the iteration in a block called BLOCK-NAME"
  (with-gensyms (height h width w (gtiling tiling) topology oob)
    `(block ,block-name
       (with-accessors ((,topology topology))
           ,gtiling
         (destructuring-bind (,height ,width) (array-dimensions (%content ,topology))
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
