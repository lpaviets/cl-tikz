(in-package #:org.numbra.cl-tikz-substitutions)

;;;; Graph
(defparameter *sierpinski* (def-graph-substitution
                             :vertices
                             (a 0 0)
                             (b 1 0)
                             (c 1 1)
                             :edges
                             (a b hori)
                             (b c vert)
                             (a c diag)
                             :substitution
                             (hori (b a hori))
                             (vert (c b vert))
                             (diag (c a diag))
                             :colours
                             (hori blue)
                             (vert red)
                             (diag green)))

(defparameter *square* (def-graph-substitution
                         :vertices
                         (a 0 0)
                         (b 1 0)
                         (c 1 1)
                         (d 0 1)
                         :edges
                         (a b hori)
                         (b c vert)
                         (d c hori)
                         (a d vert)
                         (a c diag)
                         :substitution
                         (hori (c d hori))
                         (vert (c b vert))
                         (diag (c a diag))
                         :colours
                         (hori blue)
                         (vert red)
                         (diag green)))

(defparameter *H* (def-graph-substitution
                    :vertices
                    (a 0 0)
                    (b 1 1)
                    (c 1 -1)
                    (d -1 1)
                    (e -1 -1)
                    :edges
                    (a b diag1)
                    (a c diag2)
                    (e a diag1)
                    (d a diag2)
                    (e d vert1)
                    (c b vert2)
                    :substitution
                    (diag1 (b e diag1))
                    (diag2 (c d diag2))
                    (vert1 (d e vert1))
                    (vert2 (b c vert2))
                    :colours
                    (diag1 red)
                    (diag2 blue)
                    (vert1 green)
                    (vert2 black)))

(defparameter *weak-grid* (def-graph-substitution
                            :vertices
                            (a 0 0)
                            (b 1 0)
                            (c 2 0)
                            (d 2 1)
                            (e 2 2)
                            (f 1 2)
                            (g 0 1)
                            :edges
                            (a b hori)
                            (b c hori)
                            (c d vert)
                            (d e vert)
                            (f e hori)
                            (g f diag)
                            (a g vert)
                            :substitution
                            (hori (c a hori) (d g hori))
                            (vert (e c vert) (f b vert))
                            (diag (e a diag))
                            :colours
                            (hori blue)
                            (vert "LimeGreen")
                            (diag red)))

;;;; Geometric
(defparameter *chair*
  (flet ((chair-polygon-at (origin rotation)
           (make-polygon (list (point 0 0)
                               (point 2 0)
                               (point 2 1)
                               (point 1 1)
                               (point 1 2)
                               (point 0 2))
                         origin
                         rotation)))
    (make-instance 'shape-substitution
                   :initial (chair-polygon-at (point 0 0) 0)
                   :factor (point 2 2)
                   :subdivision (list (chair-polygon-at (point 0 0) 0)
                                      (chair-polygon-at (point 1 1) 0)
                                      (chair-polygon-at (point 0 4) (- (/ pi 2)))
                                      (chair-polygon-at (point 4 0) (/ pi 2))))))
