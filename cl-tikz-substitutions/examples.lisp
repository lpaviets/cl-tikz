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

(defparameter *square-no-diag* (def-graph-substitution
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
                                 :substitution
                                 (hori (c d hori)
                                       (b a hori)) ; could be removed
                                 (vert (c b vert)
                                       (d a vert)) ; could be removed
                                 :colours
                                 (hori blue)
                                 (vert red)))

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

(defparameter *H-bartholdi*
  (def-graph-substitution
    :vertices
    (a 0 0)
    (b 0 -1)
    (c 1 -1)
    (d 1 0)
    (e 1 1)
    (f 0 1)
    :edges
    (a f up)
    (b a up)
    (d e up)
    (c d up)
    (f e right)
    (a d right)
    (b c right)
    :substitution
    (up (f b up))
    (right (d a right))
    :colours
    (up blue)
    (right red)))

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

;; Unclear if it works
(defparameter *lamplighter* (def-graph-substitution
                              :vertices
                              (u 0 0)
                              (v -1 2)
                              (w 1 2)
                              (x 0 1)
                              :edges
                              (u v a)
                              (u w b)
                              (x v b)
                              (x w a)
                              :substitution
                              (a (v u i))
                              (b (w x i))
                              (i (u u i) (v v i) (w w i) (x x i))
                              :colours
                              (a blue)
                              (b green)
                              (i red)))

;;;; Geometric
(defparameter *chair*
  (def-geometric-subdivision
    :initial
    (0 0)
    (2 0)
    (2 1)
    (1 1)
    (1 2)
    (0 2)
    :factor
    2 2
    :subdivision
    ((0 0) 0)
    ((1 1) 0)
    ((0 4) (- (/ pi 2)))
    ((4 0) (/ pi 2))))
