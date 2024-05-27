(in-package #:org.numbra.cl-tikz-substitutions)

(defparameter *vertices* nil)
(defparameter *edges* nil)
(defparameter *origin* nil)
(defparameter *representatives* (make-hash-table))
(defparameter *incoming-edges* (make-hash-table))
(defparameter *outgoing-edges* (make-hash-table))

(defun update-colours-substitution (graph new-colours)
  "NEW-COLOURS is a list, each element being of the form:

(COL (U1 COL1) (U2 COL2) ... (UN COLN))

where U1 ... UN are vertices names, and COL, COL1 ... COLN are colours with a
corresponding entry in (GRAPH-COLOURS GRAPH)."
  (let ((colours-subst-table (make-hash-table :test 'eq)))
    (loop :for (v . col-subst-v) :in new-colours
          :do (setf (gethash v colours-subst-table) col-subst-v))
    (setf (graph-colours-substitution graph) colours-subst-table)))

(defun reset-subst (&optional (graph *lamplighter*) (n 6) initial)
  (setf *representatives* (make-hash-table)
        *vertices* nil
        *origin* nil)
  (multiple-value-setq (*vertices* *edges*)
    (n-substitute-graph graph n :initial initial))
  (setf *incoming-edges* (make-hash-table)
        *outgoing-edges* (make-hash-table))
  (loop :for edge :in *edges*
        :do (push edge (gethash (edge-start edge) *outgoing-edges*))
            (push edge (gethash (edge-end edge) *incoming-edges*)))
  (loop :with u = (car *vertices*)
        :with u-y = (point-y (pos u))
        :for v :in *vertices*
        :for v-y = (point-y (pos v))
        :when (< v-y u-y)
          :do (setf u v
                    u-y v-y)
        :finally (setf *origin* u))
  *origin*)

(defun get-next (v type dir)
  (let ((next
          (ecase dir
            (:in (find type (gethash v *incoming-edges*) :key #'edge-type))
            (:out (find type (gethash v *outgoing-edges*) :key #'edge-type)))))
    (when next
      (ecase dir
        (:in (edge-start next))
        (:out (edge-end next))))))

(defun compute-equivalence-classes ()
  (dolist (v *vertices*)
    (loop :for u = v :then new-u
          :for new-u = (get-next u 'i :in)
          :while new-u
          :do (assert (eq (vertex-type u) (vertex-type new-u)))
          :finally (setf (gethash v *representatives*) u))))

(defun get-representative (v)
  (gethash v *representatives*))

(defun collapse-identifications ()
  (compute-equivalence-classes)
  (let ((in-table (make-hash-table :test #'eq))
        (out-table (make-hash-table :test #'eq)))
    (loop :for v :in *vertices*
          :for repr = (get-representative v)
          :do (setf (gethash repr in-table)
                    (append (gethash repr *incoming-edges*)
                            (gethash repr in-table)))
              (setf (gethash repr out-table)
                    (append (gethash repr *outgoing-edges*)
                            (gethash repr out-table))))
    (flet ((normalize-edge (edge)
             (with-accessors ((start edge-start)
                              (end edge-end)
                              (type edge-type))
                 edge
               (edge (get-representative start)
                     (get-representative end)
                     type))))
      (dohash (v edges) in-table
        (setf (gethash v in-table) (remove 'i (mapcar #'normalize-edge edges)
                                           :key #'edge-type)))
      (dohash (v edges) out-table
        (setf (gethash v out-table) (remove 'i (mapcar #'normalize-edge edges)
                                            :key #'edge-type)))
      (values in-table out-table))))

(defun draw-collapsed (graph)
  (multiple-value-bind (in out)
      (collapse-identifications)
    (let (vertices edges)
      (dohash (v v-edges) in
        (push v vertices)
        (setf edges (nconc v-edges edges)))
      (dohash (v v-edges) out
        (setf edges (nconc v-edges edges)))
      (dolist (v vertices)
        (draw-vertex-1 v graph))
      (dolist (e edges)
        (draw-graph-edge e graph)))))

(defun draw-collapsed-in-file (graph name n &optional initial)
  (reset-subst graph n initial)
  (let ((file (merge-pathnames (format nil "examples/~A-~A-~A.tex" name n (if initial "col" "nocol")))))
    (format t "~&Writing to file ~A~%" file)
    (with-preamble-to-file (file) ()
      (with-env (:tikzpicture)
        (draw-collapsed graph)))))


;; (defun num-to-fun (n from to)
;;   (let ((base (length to))
;;         (to-vec (coerce to 'vector)))
;;     (loop :for u :in from
;;           :for pick = (mod n base) :then (mod rem base)
;;           :for rem = (truncate n base) :then (truncate rem base)
;;           :collect (list u (aref to-vec pick)))))

;; (defun test-all-substs ()
;;   (let ((from '(u v w x))
;;         (to '(a b p))
;;         (max (expt 3 4))
;;         (correct nil))
;;     (dotimes (a (/ max 3))
;;       (dotimes (b max)
;;         (dotimes (p max)
;;           (let* ((col-a `(a (u a) ,@(num-to-fun a (cdr from) to)))
;;                  (col-b (cons 'b (num-to-fun b from to)))
;;                  (col-p (cons 'p (num-to-fun p from to)))
;;                  (cols (list col-a col-b col-p)))
;;             (update-colours-substitution *lamplighter* cols)
;;             (reset-subst *lamplighter* 3 'a)
;;             (handler-case (compute-equivalence-classes)
;;               (simple-error ()
;;                 nil)
;;               (:no-error (x) (declare (ignore x)) (push cols correct)))))))
;;     correct))

;; (defun bad-subst (subst)
;;   (destructuring-bind ((a . sa) (b . sb) (p . sp))
;;       subst
;;     (let ((vals-a (mapcar 'second sa))
;;           (vals-b (mapcar 'second sb))
;;           (vals-p (mapcar 'second sp)))
;;       (or (= 1 (length (remove-duplicates vals-a)))
;;           (= 1 (length (remove-duplicates vals-b)))
;;           (= 1 (length (remove-duplicates vals-p)))
;;           (and (not (member a vals-b)) (not (member a vals-p)))
;;           (and (member b vals-a) (not (member p vals-b))) ; no p
;;           (and (member p vals-a) (not (member b vals-p))) ; no b
;;           (equalp vals-a vals-b)
;;           (equalp vals-a vals-p)
;;           (equalp vals-b vals-p)))))

;; Non-completely-stupid substitutions that can be iterated more than 6 or 7 times:
;; Could be non-trivial:
;;  ((A (U A) (V P) (W P) (X A))
;;   (B (U B) (V A) (W A) (X B))
;;   (P (U P) (V B) (W B) (X P)))

;; (((A (U A) (V B) (W B) (X A))
;;   (B (U B) (V P) (W P) (X B))
;;   (P (U P) (V A) (W A) (X P)))

;;  ((A (U A) (V A) (W B) (X B))
;;   (B (U P) (V P) (W B) (X B))
;;   (P (U A) (V A) (W B) (X B)))

;;  ((A (U A) (V B) (W B) (X P))
;;   (B (U B) (V A) (W P) (X B))
;;   (P (U A) (V B) (W B) (X P)))
