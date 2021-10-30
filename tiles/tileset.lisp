(in-package #:cl-tikz/tiles)

;;; Tile sets

(defstruct (tileset
            (:constructor make-tileset (size)))
  "A simple structure implementing a tileset"
  (size (error "Must specify a size for the tileset") :type fixnum :read-only t)
  (tiles (make-array size :initial-element nil) :type array)
  (rules (make-rules size)))

(defun tileset-add-tile-function (tileset id fun)
  (setf (aref (tileset-tiles tileset) id) fun))

(defun tileset-get-tile-function (tileset id)
  (aref (tileset-tiles tileset) id))

(defmethod add-rules ((t-a tileset) (t-b tileset))
  (add-rules (tileset-rules t-a) (tileset-rules t-b)))

(defmethod add-rules ((tileset tileset) r)
  (add-rules (tileset-rules tileset) r))
