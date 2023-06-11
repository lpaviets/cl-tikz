(in-package #:org.numbra.cl-tikz-tilings)

(defvar *three-chessboard*
  (defhomshift three-chessboard
    (:blue :red)
    (:blue :green)
    (:green :red)))

(defvar *house*
  (defhomshift house
    (:blue :red)
    (:blue :green)
    (:green :red)
    (:red :purple)
    (:purple :yellow)
    (:yellow :green)))
