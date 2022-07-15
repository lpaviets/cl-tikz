(in-package #:cl-tikz-tilings)

;;; Wang Tiles

(defvar *rao-jeandel*
  (defwangtiles rao-jeandel
    (:green :red :red :red)
    (:green :blue :red :blue)
    (:green :green :green :red)
    (:blue :red :blue :white)
    (:blue :white :blue :blue)
    (:white :red :white :white)
    (:white :blue :green :red)
    (:red :blue :white :blue)
    (:red :white :red :blue)
    (:red :blue :green :green)
    (:green :red :white :red)))

(defvar *kari-culik*
  (defwangtiles kari-culik
    (:red :green :yellow :yellow)
    (:red :yellow :green :yellow)
    (:yellow :green :green :yellow)
    (:yellow :yellow :red :red)
    (:green :green :red :red)
    (:green :yellow :yellow :red)
    (:blue :red :blue :blue)
    (:blue :yellow :blue :green)
    (:blue :red :purple :yellow)
    (:blue :blue :purple :yellow)
    (:purple :red :purple :blue)
    (:purple :yellow :purple :green)
    (:purple :yellow :blue :yellow)))

(defvar *three-chessboard*
  (defhomshift three-chessboard
    (:blue :red)
    (:blue :green)
    (:green :red)))


;; (defvar *robinson*)
;; (defclass robinson-tile (tile)
;;   ((cornerp :initarg :cornerp
;;             :reader cornerp
;;             :type boolean)
;;   (sides :initarg :sides
;;           :type (array robinson-tile-side (4))
;;           :reader sides))
;;   (:default-initargs
;;    :cornerp (error "Must specify whether the tile is a corner tile")))

;; (defclass robinson-tile-side ()
;;   ((colour :initarg :colour
;;            :reader robinson-side-colour
;;            :type (member :green :red))
;;    (pos :initarg :pos
;;         :accessor robinson-side-pos
;;         :type (member :center :left :down :right :up))
;;    (arrow :initarg :arrow
;;           :reader robinson-side-arrow
;;           :type (member :in :out))))

;; (defun make-robinson-tile-side (colour pos arrow)
;;   (make-instance 'robinson-tile-side
;;                  :colour colour
;;                  :pos pos
;;                  :arrow arrow))

;; (defun rotate-robinson-tile-side (side turns)
;;   (with-accessors ((colour robinson-side-colour)
;;                    (pos robinson-side-pos)
;;                    (arrow robinson-side-arrow))
;;       side
;;     (let* ((pos-order #(:down :right :up :left))
;;            (new-pos (case pos
;;                       (:center :center)
;;                       (t (let* ((init-idx (position pos pos-order))
;;                                 (new-idx (if (integerp init-idx)
;;                                              (mod (+ init-idx turns) 4)
;;                                              (error "~S is not a valid pos for ~S" pos side))))
;;                            (aref pos-order new-idx))))))
;;       (make-robinson-tile-side colour
;;                                new-pos
;;                                arrow))))

;; (defun robinson-tile-side-matching-p (side1 side2)
;;   (and (eq (robinson-side-colour side1) (robinson-side-colour side2))
;;        (eq (robinson-side-pos side1) (robinson-side-pos side2))
;;        (not (eq (robinson-side-arrow side1) (robinson-side-colour side2)))))

;; (defmethod valid-neighbour-p ((t1 robinson-tile) (t2 robinson-tile) dir)
;;   (and (xor (cornerp t1) (cornerp t2))
;;        (with-tile-sides (l1 d1 r1 u1) t1
;;          (with-tile-sides (l2 d2 r2 u2) t2
;;            (ccase dir
;;              (0 (robinson-tile-side-matching-p l1 r2))
;;              (1 (robinson-tile-side-matching-p d1 u2))
;;              (2 (robinson-tile-side-matching-p r1 l2))
;;              (3 (robinson-tile-side-matching-p u1 d2)))))))

;; (defmethod make-rotated-tile ((tile variant-wang-tile) turns)
;;   (with-accessors ((tileset tileset)
;;                    (draw-function draw-function)
;;                    (sides sides)
;;                    (cornerp cornerp))
;;       tile
;;     (with-tile-sides (left down right up) tile
;;       (let ((draw-fun (lambda (pos)
;;                         (with-rotation (* turns (/ pi 2)) ((point-x pos) (point-y pos))
;;                           (funcall draw-function pos))))
;;             (rotated-sides (map 'vector
;;                                 (lambda (side)
;;                                   (rotate-robinson-tile-side side turns))
;;                                 sides)))
;;         (make-instance 'robinson-wang-tile
;;                        :tileset tileset
;;                        :sides (rotate-sequence rotated-sides turns)
;;                        :draw-function draw-fun
;;                        :cornerp cornerp)))))

;; ;;; Corner tiles are always of the form:
;; ;;; - A centered, green corner
;; ;;; - A non-centered red corner
;; ;;; - Only outgoing arrows
;; ;;; We therefore only need to have the position of the green corner
;; ;;; to define the tile.
;; ;;; We define the "non-rotated" one as having the green arrows leaving
;; ;;; the tile on its left and on its upper part.
;; (defun make-robinson-corner-tile (tileset turns)
;;   (let* ((left (make-robinson-tile-side :green :center :out))
;;          (down (make-robinson-tile-side :red :left :out))
;;          (right (make-robinson-tile-side :red :up :out))
;;          (up (make-robinson-tile-side :green :center :out))
;;          (tile (make-instance 'robinson-tile
;;                               :tileset tileset
;;                               :cornerp t
;;                               :sides (vector left down right up))))
;;     (setf (draw-function tile) (make-robinson-drawing-function tile))
;;     (make-rotated-tile tile turns)))

;; (defmacro with-robinson-horizontal-arrow ((hcol hpos left right) tile &body body)
;;     (with-gensyms (gtile arrows)
;;     `(let ((,gtile ,tile))
;;        (with-accessors ((,hcol horizontal-colour)
;;                         (,hpos horizontal-pos)
;;                         (,arrows arrow-directions))
;;            ,gtile
;;          (declare (ignorable ,hcol ,hpos))
;;          (symbol-macrolet ((,left (aref ,arrows 0))
;;                            (,right (aref ,arrows 2)))
;;            (declare (ignorable ,left ,right))
;;            ,@body)))))

;; (defmacro with-robinson-vertical-arrow ((vcol vpos down up) tile &body body)
;;   (with-gensyms (gtile arrows)
;;     `(let ((,gtile ,tile))
;;        (with-accessors ((,vcol vertical-colour)
;;                         (,vpos vertical-pos)
;;                         (,arrows arrow-directions))
;;            ,gtile
;;          (declare (ignorable ,vcol ,vpos))
;;          (symbol-macrolet ((,down (aref ,arrows 1))
;;                            (,up (aref ,arrows 3)))
;;            (declare (ignorable ,down ,up))
;;            ,@body)))))

;; (defun robinson-arrow-dirs-to-tikz-option (dir1 dir2)
;;   (flet ((translate (dir)
;;            (ccase dir
;;              (:in "stealth reversed")
;;              (:out "stealth")
;;              (:none ""))))
;;     (format nil "~A-~A" (translate dir1) (translate dir2))))


;; (defun %make-robinson-drawing-function-no-corner (left down right up)
;;   (lambda (pos)
;;     (with-accessors ())))

;; (defun make-robinson-drawing-function (cornerp left down right up)
;;   (declare (ignorable cornerp))
;;   (lambda (pos)
;;     (with-point (x y) pos)))

;; ;; (defun robinson-draw-horizontal-arrow (tile pos)
;; ;;   (with-robinson-horizontal-arrow (hcol hpos left right) tile
;; ;;     (with-point (x y) pos
;; ;;       (let ((height (ecase hpos
;; ;;                       (:center y)
;; ;;                       (:down (- y 0.25))
;; ;;                       (:up (+ y 0.25))))
;; ;;             (start (case left
;; ;;                      (:none (- x 0.25))
;; ;;                      (t (- x 0.5))))
;; ;;             (end (case right
;; ;;                    (:none (+ x 0.25))
;; ;;                    (t (+ x 0.5)))))
;; ;;         (draw-line start height
;; ;;                    end height
;; ;;                    :options (list hcol
;; ;;                                   (robinson-arrow-dirs-to-tikz-option left right)))))))

;; ;; (defun robinson-draw-vertical-arrow (tile pos)
;; ;;   (with-robinson-vertical-arrow (vcol vpos down up) tile
;; ;;     (with-point (x y) pos
;; ;;       (let ((width (ecase vpos
;; ;;                      (:center x)
;; ;;                      (:left (- x 0.25))
;; ;;                      (:right (+ x 0.25))))
;; ;;             (start (case down
;; ;;                      (:none (- y 0.25))
;; ;;                      (t (- y 0.5))))
;; ;;             (end (case up
;; ;;                    (:none (+ y 0.25))
;; ;;                    (t (+ y 0.5)))))
;; ;;         (draw-line width start
;; ;;                    width end
;; ;;                    :options (list vcol
;; ;;                                   (robinson-arrow-dirs-to-tikz-option down up)))))))

;; ;; (defun make-robinson-drawing-function (tile)
;; ;;   (lambda (pos)
;; ;;     (with-point (x y) pos
;; ;;       (draw-square (- x 0.5) (- y 0.5))
;; ;;       (robinson-draw-horizontal-arrow tile pos)
;; ;;       (robinson-draw-vertical-arrow tile pos))))

;; ;; (let ((corner (make-instance 'variant-wang-tile
;; ;;                              :tileset 'robinson
;; ;;                              :
;; ;;                              :sides #(:even-out-green
;; ;;                                       :even-out-red
;; ;;                                       :even-out-red
;; ;;                                       :even-out-green)))))
