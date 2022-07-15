;;;; cl-tikz-tilings.asd

(asdf:defsystem #:cl-tikz-tilings
  :description "Describe cl-tikz here"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-tikz)
  :components ((:file "packages")
               (:file "wang-tiles")
               (:file "tileset")
               (:file "tiling")
               (:file "hom-shift")
               (:file "draw-tiles")
               (:file "tiles-patterns")
               (:file "dancing-links")
               (:file "solver")
               (:file "example")))
