(in-package #:cl-tikz/tiles)

(defun gen-test-file ()
  (with-preamble-to-file ("./test-output.tex") ()
    (with-env (tikzpicture)
      (let ((test-set (make-tileset 3)))
        (deftile (test-set 0 :test) ()
          (draw-square 0 0 :options '(fill red)))
        (deftile (test-set 1 :test) (:background :blue)
          (draw-square 0 0))
        (deftile (test-set 2 :test) ()
          (draw-wang-tile 0 0 :red :blue :green :black))
        (let ((solution (make-array '(3 3)
                                    :initial-contents (loop :repeat 3
                                                            :collect
                                                            (loop :repeat 3
                                                                  :collect
                                                                  (random 3))))))
          (draw-tiling solution test-set)
          nil)))))