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

;;; Difficulty
;;; Robinson tiles usually involve *corner* adjacency rules.
;;; Even worse: the rule is something like "each corner of the grid has to
;;; see at least one tile of type X"
;;; Solution: in the solver, replace FIND-ALL-VALID-TILEs - which itself
;;; uses only TILE-FITS-WITH-P, and therefore only neighbours - by a
;;; suitable function.
(defclass robinson-tile (tile)
  ((cornerp :initarg :cornerp
            :reader cornerp
            :type boolean
            :documentation "Whether the tile is a corner or not.
This is used to determine how to draw the tile.")
   (parity :initarg :parity
           :reader parity
           :type boolean
           :documentation "Whether the tile is a \"parity tile\" or not.
The rule is that two parity tiles can't be placed next to each other.")
   (sides :initarg :sides
          :type (array robinson-tile-side (4))
          :reader sides))
  (:default-initargs
   :cornerp (error "Must specify whether the tile is a corner tile")
   :parity (error "Must specify whether the tile is a parity tile")))

(defclass robinson-tile-side ()
  ((colour :initarg :colour
           :reader robinson-side-colour
           :type (member :blue :red))
   (shifted :initarg :shifted
            :accessor robinson-side-shifted
            :type (member :center :left :down :right :up))
   (arrow :initarg :arrow
          :reader robinson-side-arrow
          :type (member :in :out))))

(defun make-robinson-tile-side (colour shifted arrow)
  (make-instance 'robinson-tile-side
                 :colour colour
                 :shifted shifted
                 :arrow arrow))

(defun make-robinson-tile (tileset cornerp parity left down right up)
  "Each of LEFT, DOWN, RIGHT and UP is a list of length 3, of the form
(COLOUR SHIFTEDP ARROW)"
  (flet ((make-side (arg)
           (apply #'make-robinson-tile-side arg)))
    (let ((sides (map '(array robinson-tile-side (4))
                      #'make-side
                      (list left down right up))))
      (make-instance 'robinson-tile
                     :tileset tileset
                     :sides sides
                     :cornerp cornerp
                     :parity parity))))

(defun rotate-robinson-tile-side (side turns)
  (with-accessors ((colour robinson-side-colour)
                   (shifted robinson-side-shifted)
                   (arrow robinson-side-arrow))
      side
    (let* ((shifted-order #(:down :right :up :left))
           (new-shifted (case shifted
                      (:center :center)
                      (t (let* ((init-idx (position shifted shifted-order))
                                (new-idx (if (integerp init-idx)
                                             (mod (+ init-idx turns) 4)
                                             (error "~S is not a valid shift for ~S"
                                                    shifted
                                                    side))))
                           (aref shifted-order new-idx))))))
      (make-robinson-tile-side colour
                               new-shifted
                               arrow))))

(defmethod make-rotated-tile ((tile robinson-tile) turns)
  (let ((new-sides (map '(array robinson-tile-side (4))
                        (lambda (side)
                          (rotate-robinson-tile-side side turns))
                        (sides tile))))
    (make-instance 'robinson-tile
                   :tileset (tileset tile)
                   :cornerp (cornerp tile)
                   :parity (parity tile)
                   :sides (rotate-sequence new-sides turns))))

(defun robinson-tile-side-matching-p (side1 side2)
  (and (eq (robinson-side-colour side1) (robinson-side-colour side2))
       (eq (robinson-side-shifted side1) (robinson-side-shifted side2))
       (not (eq (robinson-side-arrow side1) (robinson-side-arrow side2)))))

(defmethod valid-neighbour-p ((t1 robinson-tile) (t2 robinson-tile) dir)
  (and (or (not (parity t1))
           (not (parity t2))) ; Can't have 2 adjacent corner tile
       (with-tile-sides (l1 d1 r1 u1) t1
         (with-tile-sides (l2 d2 r2 u2) t2
           (ecase dir
             (0 (robinson-tile-side-matching-p l1 r2))
             (1 (robinson-tile-side-matching-p d1 u2))
             (2 (robinson-tile-side-matching-p r1 l2))
             (3 (robinson-tile-side-matching-p u1 d2)))))))

(defun robinson-side-arrow-to-tikz-options (side)
  (let ((arrowinp (eq (robinson-side-arrow side) :in)))
   (option-arrow-head-at (if arrowinp 0.7 0.98) arrowinp :style 'latex)))

(defmethod make-tile-drawing-function ((tile robinson-tile))
  (with-tile-sides (left down right up) tile
    (def-drawing-function ()
      (draw-square -0.5 -0.5)
      (when (parity tile)
        (dolist (x '(-0.5 0.45))
          (dolist (y '(-0.5 0.45))
            (draw-square x y
                         :size 0.05
                         :options '((fill . yellow))))))
      ;; Up side
      (with-accessors ((colour robinson-side-colour)
                       (shifted robinson-side-shifted)
                       (arrow robinson-side-arrow))
          up
        (let ((width (ecase shifted
                       (:center 0)
                       (:left -0.25)
                       (:right 0.25)))
              (height (if (and (cornerp tile) (eq colour :red)) -0.25 0)))
          (draw-line width height width 0.5
                     :options `(,(robinson-side-arrow-to-tikz-options up)
                                ,colour))))

      ;; Left side
      (with-accessors ((colour robinson-side-colour)
                       (shifted robinson-side-shifted)
                       (arrow robinson-side-arrow))
          left
        (let ((height (ecase shifted
                        (:center 0)
                        (:down -0.25)
                        (:up 0.25)))
              (width (if (and (cornerp tile) (eq colour :red)) 0.25 0)))
          (draw-line width height -0.5 height
                     :options `(,(robinson-side-arrow-to-tikz-options left)
                                ,colour))))

      ;; Down side
      (with-accessors ((colour robinson-side-colour)
                       (shifted robinson-side-shifted)
                       (arrow robinson-side-arrow))
          down
        (let ((width (ecase shifted
                       (:center 0)
                       (:left -0.25)
                       (:right 0.25)))
              (height (if (and (cornerp tile) (eq colour :red)) 0.25 0)))
          (draw-line width height width -0.5
                     :options `(,(robinson-side-arrow-to-tikz-options down)
                                ,colour))))

      ;; Right side
      (with-accessors ((colour robinson-side-colour)
                       (shifted robinson-side-shifted)
                       (arrow robinson-side-arrow))
          right
        (let ((height (ecase shifted
                        (:center 0)
                        (:down -0.25)
                        (:up 0.25)))
              (width (if (and (cornerp tile) (eq colour :red)) -0.25 0)))
          (draw-line width height 0.5 height
                     :options `(,(robinson-side-arrow-to-tikz-options right)
                                ,colour)))))))

(defvar *robinson*
  (let ((robinson-tiles (make-hash-table :test 'eq)))
    (dolist (tile (list (make-robinson-tile 'robinson t t
                                            '(:green :center :out)
                                            '(:red :left :out)
                                            '(:red :up :out)
                                            '(:green :center :out))
                        (make-robinson-tile 'robinson t nil
                                            '(:green :center :out)
                                            '(:red :left :out)
                                            '(:red :up :out)
                                            '(:green :center :out))
                        (make-robinson-tile 'robinson nil nil
                                            '(:green :center :in)
                                            '(:green :center :in)
                                            '(:green :center :in)
                                            '(:green :center :out))
                        (make-robinson-tile 'robinson nil nil
                                            '(:green :center :in)
                                            '(:red :left :in)
                                            '(:green :center :in)
                                            '(:red :left :out))
                        (make-robinson-tile 'robinson nil nil
                                            '(:green :center :in)
                                            '(:red :right :in)
                                            '(:green :center :in)
                                            '(:red :right :out))
                        (make-robinson-tile 'robinson nil nil
                                            '(:red :up :in)
                                            '(:green :center :in)
                                            '(:red :up :in)
                                            '(:green :center :out))
                        (make-robinson-tile 'robinson nil nil
                                            '(:red :up :in)
                                            '(:red :left :in)
                                            '(:red :up :in)
                                            '(:red :left :out))
                        (make-robinson-tile 'robinson nil nil
                                            '(:red :up :in)
                                            '(:red :right :in)
                                            '(:red :up :in)
                                            '(:red :right :out))))
      (dotimes (turns 4)
        (setf (gethash (make-rotated-tile tile turns) robinson-tiles) t))
      (make-tileset 'robinson robinson-tiles))))

(let (parity-tiles non-parity-tiles all-tiles)
  (dotiles (tile *robinson*)
    (if (parity tile)
        (push tile parity-tiles)
        (push tile non-parity-tiles))
    (push tile all-tiles))

  (defun find-all-valid-robinson-tiles (pos tiling)
    (destructuring-bind (left down-left down down-right right up-right up up-left)
        (mapcar (lambda (p)
                  (tiling-tile-at p tiling))
                (point-all-neighbours pos))
      (labels ((tile-fits-p (tile)
                 (tile-fits-with-p tile left down right up))
               (square-has-parity-p (t1 t2 t3)
                 (xor (and t1 (parity t1))
                      (and t2 (parity t2))
                      (and t3 (parity t3)))))
        (if (or (square-has-parity-p left down-left down)
                (square-has-parity-p down down-right right)
                (square-has-parity-p right up-right up)
                (square-has-parity-p up up-left left))
            (remove-if-not #'tile-fits-p non-parity-tiles)
            (if (or (and left down-left down)
                    (and down down-right right)
                    (and right up-right up)
                    (and up up-left left))
                (remove-if-not #'tile-fits-p parity-tiles)
                (remove-if-not #'tile-fits-p all-tiles)))))))
