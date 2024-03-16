;;;; cl-tikz-tilings.asd

(asdf:defsystem #:cl-tikz-tilings
  :description "Describe cl-tikz here"
  :author "Léo Paviet Salomon"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (:cl-tikz :cl-sat.glucose :cl-ppcre)
  :components ((:file "packages")
               (:file "wang-tiles")
               (:file "tileset")
               (:file "topology")
               (:file "tiling")
               (:file "hom-shift")
               (:file "draw-tiles")
               (:module "solvers"
                :components
                ((:file "dancing-links")
                 (:file "solver-naive")
                 (:file "glucose-fix")
                 (:file "solver-sat")))
               (:module "examples"
                :components
                ((:file "small-aperiodics")
                 (:file "hom-shifts")
                 (:file "robinson")
                 (:file "example")))))
