(in-package #:org.numbra.cl-tikz-tilings)

;;; TODO:
;;; - Fix width-height vs m-n vs ... i.e. parameter consistency
;;; - Interact with TILING/TOPOLOGY classes: should not have to give explicit parameters

;; need to load cl-sat and (e.g.) cl-sat.glucose

;; Idea: one variable per tile per position: the variable xijt is true if the
;; tile t appears at pos (i, j)

(ql:quickload :cl-ppcre)
(ql:quickload :cl-sat.glucose)

(defun valid-pairs-up-right (tileset)
  (let ((valid-up (make-hash-table))
        (valid-right (make-hash-table)))
    (dotiles (t1 tileset)
      (dotiles (t2 tileset)
        (when (valid-neighbour-p t1 t2 :up)
          (push t2 (gethash t1 valid-up)))
        (when (valid-neighbour-p t1 t2 :right)
          (push t2 (gethash t1 valid-right)))))
    (cons valid-up valid-right)))

(defun mappings-tileset-to-num (tileset)
  (let ((tiles->num (make-hash-table))
        (num->tiles (make-hash-table))
        (i 0))
    (dotiles (tile tileset)
      (setf (gethash tile tiles->num) i)
      (setf (gethash i num->tiles) tile)
      (incf i))
    (cons tiles->num num->tiles)))

(defun var-i-j-tile (i j tile &optional mappings)
  (symb i "-" j "-" (if mappings
                        (gethash tile (car mappings))
                        tile)))

(defun clause-ij (i j tiles &optional mappings)
  (cons 'or
        (loop :with ij-tiles = (mapcar (lambda (tile)
                                         (var-i-j-tile i j tile mappings))
                                       tiles)
              :for tile :in ij-tiles
              :collect `(and ,tile
                             (not (or ,@(remove tile ij-tiles)))))))

(defun one-tile-per-pos (n m tiles &optional mappings)
  (let (clauses)
    (dotimes (i n)
      (dotimes (j m)
        (push (clause-ij i j tiles mappings) clauses)))
    (cons 'and clauses)))

(defun clauses-prefilled-tiling (tiling &optional mappings)
  (let ((clauses nil))
    (dotiling (pos tile) tiling
      (when tile
        (with-point (x y) pos
          (push (var-i-j-tile y x tile mappings) clauses))))
    (cons 'and clauses)))

(defun clauses-from-tiling (tiling &optional mappings)
  ;; Assumes that tileset is a hom-shift, no checks
  (destructuring-bind (n m) (tiling-dimensions tiling)
    (let* ((tileset (tileset tiling))
           (tiles (set-to-list (tileset-tiles tileset)))
           (clauses (list (clauses-prefilled-tiling tiling mappings)
                          (one-tile-per-pos n m tiles mappings)))
           (extra-rules (extra-rules tileset))
           (valid-pairs (valid-pairs-up-right tileset))
           (valid-pairs-up (car valid-pairs))
           (valid-pairs-right (cdr valid-pairs)))
      (dotimes (i n)
        (dotimes (j m)
          (when (< i (1- n))
            (dotiles (tile tileset)
              ;; If tile A in (I, J), want some allowed neighbour B in (I+1, J)
              ;; Can be rephrased as: IF A-I-J, THEN SOME B-I+1-J
              ;; Logically equivalent to:
              ;; NOT A-I-J OR SOME B-I-J
              (push `(or (not ,(var-i-j-tile i j tile mappings))
                         ,@(loop :for neighbour :in (gethash tile valid-pairs-up) ; up or right ?!
                                 :collect (var-i-j-tile (1+ i) j neighbour mappings)))
                    clauses)))
          (when (< j (1- m))
            (dotiles (tile tileset)
              (push `(or (not ,(var-i-j-tile i j tile mappings))
                         ,@(loop :for neighbour :in (gethash tile valid-pairs-right) ; up or right ?!
                                 :collect (var-i-j-tile i (1+ j) neighbour mappings)))
                    clauses)))))
      (when extra-rules
        (dotimes (i n)
          (dotimes (j m)
            (push (funcall extra-rules i j mappings) clauses))))
      (cons 'and clauses))))

;; Run solver and parse solution
(defun solver-sat (tiling &key random)
  (declare (ignore random))
  (let* ((tileset (tileset tiling))
         (mappings (mappings-tileset-to-num tileset))
         (clauses (clauses-from-tiling tiling mappings))
         (solution (cl-sat:solve clauses :glucose-parallel))
         (copied-tiling (copy-tiling tiling)))
    (when solution
      (flet ((set-tile-from-solution (var)
               (ppcre:register-groups-bind ((#'parse-integer i)
                                            (#'parse-integer j)
                                            (#'parse-integer num))
                   ("^\(\\d+\)-\(\\d+\)-\(.*\)$" (symbol-name var))
                 (let ((tile-at-i-j (gethash num (cdr mappings)))
                       (pos (point j i)))
                   (when (tiling-in-bounds-p pos copied-tiling)
                     (setf (tiling-tile-at pos copied-tiling) tile-at-i-j))))))
        (mapc #'set-tile-from-solution solution))
      copied-tiling)))

;; Idea: add a way to specify neighbourhoods ?
