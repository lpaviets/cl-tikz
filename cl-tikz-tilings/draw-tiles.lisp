(in-package #:org.numbra.cl-tikz-tilings)

(defun draw-wang-tile (x y left down right up &key (size 1))
  (let ((d (/ size 2)))
   (let ((bl (point-str (- x d) (- y d)))
         (br (point-str (+ x d) (- y d)))
         (tl (point-str (- x d) (+ y d)))
         (tr (point-str (+ x d) (+ y d)))
         (center (point-str x y)))
     (with-tikz-command (fill :options left)
       (format t "~{~A -- ~} cycle" (list tl center bl)))
     (with-tikz-command (fill :options down)
       (format t "~{~A -- ~} cycle" (list bl center br)))
     (with-tikz-command (fill :options right)
       (format t "~{~A -- ~} cycle" (list tr center br)))
     (with-tikz-command (fill :options up)
       (format t "~{~A -- ~} cycle" (list tl center tr)))
     (with-tikz-command (draw)
       (format t "~A -- ~A" bl tr))
     (with-tikz-command (draw)
       (format t "~A -- ~A" br tl))
     (draw-square (- x d) (- y d) :size size :options '(thin)))))

(defun draw-tiling (tiling &key with-grid grid-options)
  "Draw the tiling corresponding to TILING."
  (dotiling (pos tile) tiling
    (funcall (draw-function tile) pos))
  (when with-grid
    (destructuring-bind (n m) (tiling-dimensions tiling)
      (draw-grid 0 0 m n
                 :options '(thick)
                 :grid-options (list* (cons 'xshift "-0.5cm")
                                      (cons 'yshift "-0.5cm")
                                      grid-options)))))
