(in-package #:org.numbra.cl-tikz-tilings)

;;; Solvers

(defun find-all-valid-tiles (pos tiling)
  (let (valid)
    (dotiles (tile (tileset tiling))
      (when (apply #'tile-fits-with-p tile (tiling-neighbours-of pos tiling))
        (push tile valid)))
    valid))

(defun find-empty-cell (tiling)
  "Return NIL if it does not contain any NIL entry, and return a empty
position in the bounds of tiling as a POINT"
  (dotiling (pos tile block-iter) tiling
    (unless tile
      (return-from block-iter pos))))

(defun solve-naive (tiling &key random)
  "Solve TILING using a naive approach, similar to a DFS.
If RANDOM is non-nil, the nodes are explored in a random order, i.e.
tiles are tried non-deterministically.
Otherwise, the order is defined by (DOTILES (TILE (TILESET TILING)) ...)"
  (loop :with copied-tiling = (copy-tiling tiling)
        :with history = ()
        :for empty-pos = (find-empty-cell copied-tiling)
        :for possible-tiles = (when empty-pos
                                (find-all-valid-tiles empty-pos copied-tiling))
        ;; If we have no empty position, our job is done
        :unless empty-pos
          :do (return (tiling-valid-p copied-tiling))
        :when random
          :do (setf possible-tiles (nshuffle possible-tiles))
        :if possible-tiles              ; If we can continue the tiling ...
          :do (setf (tiling-tile-at empty-pos
                                    copied-tiling
                                    :no-check t)
                    (car possible-tiles))
              ;; We store the other tiles in HISTORY in case we chose wrong
              (push (cons empty-pos (cdr possible-tiles)) history)
        :else
          :do (unless history           ; if we can't back-up, we are done
                (return (tiling-valid-p copied-tiling)))
              (loop :while (null (cdr (first history))) ; Undo "forced" history
                                                        ;; We made the wrong choice: we reset the tiling
                    :do (setf (tiling-tile-at (caar history) copied-tiling :no-check t) nil)
                        (pop history))
              ;; We now need to take an alternate choice
              (let ((next-tile (pop (cdar history))))
                ;; We update the tiling with another choice
                (setf (tiling-tile-at (caar history)
                                      copied-tiling
                                      :no-check t)
                      next-tile))))
