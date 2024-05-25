(in-package #:org.numbra.cl-tikz-substitutions)

(defparameter *vertices* nil)
(defparameter *edges* nil)
(defparameter *origin* nil)
(defparameter *representatives* (make-hash-table))
(defparameter *incoming-edges* (make-hash-table))
(defparameter *outgoing-edges* (make-hash-table))

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
