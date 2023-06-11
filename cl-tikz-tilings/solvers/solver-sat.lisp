(in-package #:org.numbra.cl-tikz-tilings)
;; need to load cl-sat and (e.g.) cl-sat.glucose

;; Variable xijt is true if tile t at pos (i, j)

(ql:quickload :cl-ppcre)
(ql:quickload :cl-sat.glucose)

(defun var-i-j-tile (i j tile)
  (symb i "-" j "-" tile))

(defun clause-ij (i j tiles)
  (cons 'or
        (loop :with ij-tiles = (mapcar (lambda (tile)
                                         (var-i-j-tile i j tile))
                                       tiles)
              :for tile :in ij-tiles
              :collect `(and ,tile
                            (not (or ,@(remove tile ij-tiles)))))))

(defun one-tile-per-pos (n m tiles)
  (let (clauses)
    (dotimes (i n)
      (dotimes (j m)
        (push (clause-ij i j tiles) clauses)))
    (cons 'and clauses)))

(defun golden-mean (n m)
  (let* ((tiles '(0 1))
         (clauses (list (one-tile-per-pos n m tiles))))
    (dotimes (i n)
      (dotimes (j m)
        (when (< i (1- n))
          (push `(not (and ,(var-i-j-tile i j 1)
                           ,(var-i-j-tile (1+ i) j 1)))
                clauses))
        (when (< j (1- m))
          (push `(not (and ,(var-i-j-tile i j 1)
                           ,(var-i-j-tile i (1+ j) 1)))
                clauses))))
    (cons 'and clauses)))

(defun get-tiling-from-solver (n m res)
  (assert (= (* n m) (length res)))
  (let ((array (make-array `(,n ,m))))
    (flet ((read-tile (tile)
             (ppcre:register-groups-bind ((#'parse-integer i)
                                          (#'parse-integer j)
                                          name)
                 ("^\(\\d+\)-\(\\d+\)-\(.*\)$" (symbol-name tile))
               (setf (aref array i j) name))))
      (mapcar #'read-tile res))
    array))

(defun proto-solve (n m tiles-clauses-fun)
  (let* ((clauses (funcall tiles-clauses-fun n m))
         (res (cl-sat:solve clauses :glucose)))
    (when res
      (let ((array (get-tiling-from-solver n m res)))
        (loop :for i :from (1- n) :downto 0
              :do (loop :for j :from (1- m) :downto 0
                        :do (format t "~3A" (aref array i j)))
              (format t "~%"))
        array))))

;; Idea: add a way to specify neighbourhoods ?
