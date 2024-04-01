(in-package #:org.numbra.cl-tikz-tilings)

(defclass rule ()
  ())


;; MAPPINGS: cons cell of 2 hash-tables, (TILE-TO-NUM . NUM-TO-TILE)

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

(defun mappings-tile-to-num (tile mappings)
  (gethash tile (car mappings)))

(defun mappings-num-to-tile (num mappings)
  (gethash num (cdr mappings)))

;; LOGIC DSL:
;; - (OR/SOME CLAUSES*)     => one of the clauses must be true
;; - (AND/ALL CLAUSES*)     => all the clauses must be true
;; - (EXACTLY NUM CLAUSES*) => exactly NUM of the clauses must be true
;; - (DISTANCE NUM)         => all the positions at distance NUM or less from current position

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defun logic-process-clauses (clauses)
;;     (ecase (car clauses)
;;       ((or some)
;;        :todo-or)
;;       ((and all)
;;        :todo-and)
;;       (exactly
;;        :todo-exact)
;;       (distance
;;        :todo-distance))))

;; (defmacro def-extra-rule ((x y) &body body)
;;   (with-gensyms (rule clauses)
;;     `(let ((,rule (make-instance 'rule)))
;;        (lambda (,x ,y)
;;          (declare (ignorable ,x ,y))
;;          (macrolet (($ (,clauses)
;;                       (logic-process-clauses ,clauses)))
;;            ,@body)))))

;; (def-extra-rule (a b)
;;     ($ (or (tile a b) (tile (1+ a)
;;                             (1+ b)))))
