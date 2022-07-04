(in-package #:cl-tikz/tiles)

;;; Tile sets

(defclass tileset ()
  ((name :initarg :name
         :reader tileset-name
         :type symbol)
   (tiles :initarg :tiles
          :accessor tileset-tiles
          :type hash-table)))

(defmacro dotiles ((tile tileset) &body body)
  `(dohash (,tile) ,tileset
     ,@body))

(defun make-tileset (name tiles)
  (make-instance 'tileset :name name :tiles tiles))

(defun all-valid-neighbours (tileset tile dir)
  (let ((valid-neighbours (make-hash-table :test #'eq
                                           :size (hash-table-size (tileset-tiles tileset)))))
    (dotiles (t2 tileset)
      (when (valid-neighbour-p tile t2 dir)
        (setf (gethash t2 valid-neighbours) t)))
    valid-neighbours))

(defun tile-fits-with-p (tile left down right up)
  (and (or (not left)
           (valid-neighbour-p tile left :left))
       (or (not down)
           (valid-neighbour-p tile down :down))
       (or (not right)
           (valid-neighbour-p tile right :right))
       (or (not up)
           (valid-neighbour-p tile up :up))))
