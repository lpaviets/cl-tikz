(in-package #:cl-tikz/tiles)

;;; Wang Tiles

(defun draw-wang-tile (x y left down right up &key (size 1))
  (let ((bl (format nil " (~f, ~f) " x y))
        (br (format nil " (~f, ~f) " (+ x size) y))
        (tl (format nil " (~f, ~f) " x (+ y size)))
        (tr (format nil " (~f, ~f) " (+ x size) (+ y size)))
        (center (format nil " (~f, ~f) " (+ x (/ size 2)) (+ y (/ size 2)))))
   (with-tikz-command (fill :options left)
     (format t "~{~a -- ~} cycle" (list tl center bl)))
    (with-tikz-command (fill :options down)
      (format t "~{~a -- ~} cycle" (list bl center br)))
    (with-tikz-command (fill :options right)
      (format t "~{~a -- ~} cycle" (list tr center br)))
    (with-tikz-command (fill :options up)
      (format t "~{~a -- ~} cycle" (list tl center tr)))
    (with-tikz-command (draw)
      (format t "~a -- ~a" bl tr))
    (with-tikz-command (draw)
      (format t "~a -- ~a" br tl))
    (draw-square x y :size size)))

(defmacro def-wang-tileset (name &body tiles)
  (let ((tileset (gensym)))
    `(let ((,tileset (make-tileset ,(length tiles))))
       ,@(loop :for tile :in tiles
               :for i :from 0
               :collect
               `(deftile (,tileset ,i ,name :sides ',tile) ()
                  (apply 'draw-wang-tile 0 0 ',tile)))
       (tileset-make-rules-from-sides ,tileset)
       ,tileset)))
