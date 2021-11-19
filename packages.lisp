;;;; package.lisp

(defpackage #:cl-tikz
  (:use #:cl)
  (:export
   ;; Draw paths
   #:draw-rectangle
   #:draw-square
   #:draw-node
   #:draw-grid
   #:draw-long-path
   #:with-random-crop
   ;; Utilities
   #:symb
   #:capture-stdout
   #:format-options
   #:latex-command
   #:with-env
   #:with-tikz-command
   #:with-preamble-to-file))

(defpackage #:cl-tikz/tiles
  (:use #:cl #:cl-tikz)
  (:export
   ;; Tiles
   #:deftile
   #:draw-tiling
   #:def-wang-tileset
   #:solve-naive))
