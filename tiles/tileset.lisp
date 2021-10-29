(in-package #:cl-tikz)

(defstruct tileset
  "A simple structure implementing a tileset"
  (tiles (make-hash-table) :type hash-table)
  (rules (make-hash-table) :type hash-table))

(defun tileset-add-tile-function (tileset id fun)
  (setf (gethash id (tileset-tiles tileset)) fun))

(defun tileset-get-tile-function (tileset id)
  (gethash id (tileset-tiles tileset)))

(defun tileset-add-rules (tileset rules))
