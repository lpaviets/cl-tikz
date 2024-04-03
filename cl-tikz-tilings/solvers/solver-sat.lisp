(in-package #:org.numbra.cl-tikz-tilings)

;;; TODO:
;;; - Fix width-height vs m-n vs ... i.e. parameter consistency
;;; - Interact with TILING/TOPOLOGY classes: should not have to give explicit parameters

;; need to load cl-sat and (e.g.) cl-sat.glucose

;; Idea: one variable per tile per position: the variable xijt is true if the
;; tile t appears at pos (i, j)

;; May be some functions ought to be in LOGIC.LISP instead ?

;;; Utils
(defun tiles-same-neighbours (tileset dir &optional mappings)
  "Return a hash-table of equivalent tiles.

Keys are the list of neighbours, mapped by mappings to numbers.

Values are lists of tiles whose neighbours in direction DIR are the
corresponding key of the table."
  ;; TODO: fix when mappings is nil
  (assert mappings)
  (flet ((tile-to-neighbours-set-dir (tile)
           (sort (loop :for nghb :in (set-to-list (all-valid-neighbours tileset tile dir))
                       :collect (mappings-tile-to-num nghb mappings))
                 #'<)))
    (let ((equivalents (make-hash-table :test 'equalp)))
      (dotiles (tile tileset)
        (let ((neighbours-set (tile-to-neighbours-set-dir tile)))
          (push (mappings-tile-to-num tile mappings) (gethash neighbours-set equivalents))))
      equivalents)))

(defun var-i-j-tile (i j tile &optional mappings)
  (symb i "-" j "-" (if mappings
                        (mappings-tile-to-num tile mappings)
                        tile)))

;;; Clause generation
(defun make-clause-ij (i j tiles &optional mappings)
  (cons 'or
        (loop :with ij-tiles = (mapcar (lambda (tile)
                                         (var-i-j-tile i j tile mappings))
                                       tiles)
              :for tile :in ij-tiles
              :collect `(and ,tile
                             (not (or ,@(remove tile ij-tiles)))))))

(defun make-clauses-one-tile-per-pos (m n tiles &optional mappings)
  (let (clauses)
    (dotimes (i m)
      (dotimes (j n)
        (push (make-clause-ij i j tiles mappings) clauses)))
    (cons 'and clauses)))

(defun make-clauses-prefilled-tiling (tiling &optional mappings (except :default))
  (let ((clauses nil))
    (dotiling (pos tile) tiling
      (when (and tile (not (eq tile except)))
        (with-point (x y) pos
          (push (var-i-j-tile y x tile mappings) clauses))))
    (cons 'and clauses)))

(defun make-clause-neighbours (i j tileset dir &optional mappings)
  (let ((equivalents (tiles-same-neighbours tileset dir mappings))
        (clause nil)
        (next-i (case dir
                  (:right (1+ i))
                  (:left (1- i))
                  (t i)))
        (next-j (case dir
                  (:up (1+ j))
                  (:down (1- j))
                  (t j))))
    (dohash (neighbours here) equivalents
      ;; Tiles have alread gone through MAPPINGS: no need to re-do it here
      (let ((at-pos (cons 'or (loop :for num :in here
                                    :collect (var-i-j-tile i j num))))
            (at-next (cons 'or (loop :for num :in neighbours
                                     :collect (var-i-j-tile next-i next-j num)))))
        (push `(or (not ,at-pos)
                   ,at-next)
              clause)))
    (cons 'and clause)))

(defun clauses-from-tiling (tiling &optional mappings)
  ;; Assumes that tileset is a hom-shift, no checks
  (destructuring-bind (m n) (tiling-dimensions tiling)
    (let* ((tileset (tileset tiling))
           (tiles (set-to-list (tileset-tiles tileset)))
           (clauses (list (make-clauses-prefilled-tiling tiling mappings)
                          (make-clauses-one-tile-per-pos m n tiles mappings)))
           (extra-rules (extra-rules tileset)))
      (dotimes (i m)
        (dotimes (j n)
          (when (< i (1- m))
            (push (make-clause-neighbours i j tileset :right mappings) clauses))
          (when (< 0 i)
            (push (make-clause-neighbours i j tileset :left mappings) clauses))
          (when (< j (1- n))
            (push (make-clause-neighbours i j tileset :up mappings) clauses))
          (when (< 0 j)
            (push (make-clause-neighbours i j tileset :left mappings) clauses))))
      (when extra-rules
        (dotimes (i m)
          (dotimes (j n)
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
                 (let ((tile-at-i-j (mappings-num-to-tile num mappings))
                       (pos (point i j)))
                   (when (tiling-in-bounds-p pos copied-tiling)
                     ;; Might happen that we use variables OUTSIDE of the
                     ;; tiling, in particular, when using extra-rules.
                     (setf (tiling-tile-at pos copied-tiling) tile-at-i-j))))))
        (mapc #'set-tile-from-solution solution))
      copied-tiling)))

;; Idea: add a way to specify neighbourhoods ?
