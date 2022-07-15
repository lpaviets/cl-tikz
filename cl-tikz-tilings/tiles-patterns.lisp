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


(defvar *robinson*)
(defclass robinson-tile (tile)
  ((cornerp :initarg :cornerp
            :reader cornerp
            :type boolean)
  (sides :initarg :sides
          :type (array robinson-tile-side (4))
          :reader sides))
  (:default-initargs
   :cornerp (error "Must specify whether the tile is a corner tile")))

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

(defun make-robinson-tile (tileset cornerp left down right up)
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
                     :cornerp cornerp))))

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

(defun robinson-tile-side-matching-p (side1 side2)
  (and (eq (robinson-side-colour side1) (robinson-side-colour side2))
       (eq (robinson-side-shifted side1) (robinson-side-shifted side2))
       (not (eq (robinson-side-arrow side1) (robinson-side-colour side2)))))

(defmethod valid-neighbour-p ((t1 robinson-tile) (t2 robinson-tile) dir)
  (and (xor (cornerp t1) (cornerp t2))
       (with-tile-sides (l1 d1 r1 u1) t1
         (with-tile-sides (l2 d2 r2 u2) t2
           (ccase dir
             (0 (robinson-tile-side-matching-p l1 r2))
             (1 (robinson-tile-side-matching-p d1 u2))
             (2 (robinson-tile-side-matching-p r1 l2))
             (3 (robinson-tile-side-matching-p u1 d2)))))))

(defun robinson-side-arrow-to-tikz-options (side)
  (let ((arrowinp (eq (robinson-side-arrow side) :in)))
   (option-arrow-head-at (if arrowinp 0.6 0.95) arrowinp :style 'latex)))

(defmethod make-tile-drawing-function ((tile robinson-tile) &optional (turns 0))
  (with-tile-sides (left down right up) tile
    (def-drawing-function (turns)
      (draw-square -0.5 -0.5)
      ;; Up side
      (with-accessors ((colour robinson-side-colour)
                       (shifted robinson-side-shifted)
                       (arrow robinson-side-arrow))
          up
        (let ((width (ecase shifted
                       (:center 0)
                       (:left -0.25)
                       (:right 0.25)))
              (height (if (cornerp tile) -0.25 0)))
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
              (width (if (cornerp tile) 0.25 0)))
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
              (height (if (cornerp tile) 0.25 0)))
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
              (width (if (cornerp tile) -0.25 0)))
          (draw-line width height 0.5 height
                     :options `(,(robinson-side-arrow-to-tikz-options right)
                                ,colour)))))))
