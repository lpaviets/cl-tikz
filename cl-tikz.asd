;;;; cl-tikz.asd

(asdf:defsystem #:cl-tikz
  :description "Describe cl-tikz here"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "utils")
               (:file "maths")
               (:file "helpers")
               (:file "shapes")
               (:file "cl-tikz")
               (:module "tiles"
                :components ((:file "wang-tiles")
                             (:file "tileset")
                             (:file "tiling")
                             (:file "hom-shift")
                             (:file "draw-tiles")
                             (:file "tiles-patterns")
                             (:file "dancing-links")
                             (:file "solver")
                             (:file "example")))))
