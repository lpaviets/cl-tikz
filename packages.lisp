;;;; package.lisp

(defpackage #:cl-tikz
  (:use #:cl)
  (:export
   ;; Draw paths
   #:draw-rectangle
   #:draw-square
   #:draw-grid
   #:draw-long-path))

(defpackage #:cl-tikz/tiles
  (:use #:cl #:cl-tikz)
  (:export
   ;; Tiles
   #:deftile
   #:draw-tiling))
