(in-package #:cl-tikz/tiles)

;;; Wang Tiles

(defun draw-wang-tile (x y left down right up &key (size 1))
  (let ((bl (format nil " (~f, ~f) " x y))
        (br (format nil " (~f, ~f) " (+ x size) y))
        (tl (format nil " (~f, ~f) " x (+ y size)))
        (tr (format nil " (~f, ~f) " (+ x size) (+ y size)))
        (center (format nil " (~f, ~f) " (+ x (/ size 2)) (+ y (/ size 2)))))
   (with-tikz-command (fill :options left)
     (format t "~{~a -- ~^ cycle~}" (list tl center bl)))
    (with-tikz-command (fill :options down)
      (format t "~{~a -- ~^ cycle~}" (list bl center br)))
    (with-tikz-command (fill :options right)
      (format t "~{~a -- ~^ cycle~}" (list tr center br)))
    (with-tikz-command (fill :options up)
      (format t "~{~a -- ~^ cycle~}" (list tl center tr)))
    (with-tikz-command (draw)
      (format t "~a -- ~a" bl tr))
    (with-tikz-command (draw)
      (format t "~a -- ~a" br tl))
    (draw-square x y :size size)))
