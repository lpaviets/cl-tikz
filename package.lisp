;;;; package.lisp

(defpackage #:cl-tikz
  (:use #:cl)
  (:export
   ;; Draw paths
   #:draw-rectangle
   #:draw-square
   #:draw-grid
   #:draw-long-path
   ;; Tiles
   #:deftile
   #:draw-tiling))
