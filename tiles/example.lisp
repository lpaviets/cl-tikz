(in-package #:cl-tikz/tiles)

(defun gen-test-file ()
  (with-preamble-to-file ("./test-output.tex") ()
    (with-env (tikzpicture)
      (let ((set (make-tileset 3)))
        (deftile (set 0) ()
          (draw-square x y :options '(fill red)))
        (deftile (set 1) (:background :blue)
          (draw-square x y))
        (deftile (set 2) (:background blue)
          (draw-square x y :options '(fill green)))
        (let ((solution (make-array '(3 3)
                                    :initial-contents (loop :repeat 3
                                                            :collect
                                                            (loop :repeat 3
                                                                  :collect
                                                                  (random 3))))))
          (draw-tiling solution set)
          nil)))))
