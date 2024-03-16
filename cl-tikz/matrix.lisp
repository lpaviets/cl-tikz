(in-package #:org.numbra.cl-tikz)

(defun format-matrix (array &key (env 'matrix)
                              (format-entry 'normalise-string)
                              options)
  "Format a matrix or an array.
ARRAY is a 1- or 2-dimensional array. Each entry of the array is inserted
in the LaTeX source after being processed by FORMAT-ENTRY.
The LaTeX environment is determined by ENV. Usually, ENV will be
a variant of 'matrix (e.g. pmatrix, bmatrix ...)"
  (with-math
    (with-env (env :options options)
      (if (= 1 (array-rank array))
          (loop :with len = (length array)
                :for i :from 1
                :for x :across array
                :do (princ x)
                :unless (= i len)
                  :do (princ '&))
          (destructuring-bind (height width)
              (array-dimensions array)
            (loop :for j :below height :do
              (loop :for i :below width
                    :do (format t "~A" (funcall format-entry (aref array j i)))
                    :if (= (1+ i) width)
                      :do (format t "\\\\")
                    :else :do
                      (format t " & "))
              (format t "~%")))))))

(defun format-array (array alignment &key hlines (format-entry 'normalise-string))
  "Format a LaTeX 2d-array.
ALIGNMENT is a string, representing the mandatory argument to the array
environment.
HLINES can be one of the following:
- NIL: in that case, no horizontal lines are inserted
- T: in that case, horizontal lines are inserted everywhere, including
before and after the array
- a list of integers: in that case, a horizontal line is inserted only in
the numbered rows. 0 represents the hline before the array.
Each entry is formatted using FORMAT-ENTRY."
  (with-math
    (with-env (:array :options alignment :options-args '(:mandatory t))
      (destructuring-bind (height width)
          (array-dimensions array)
        (loop :for j :below height
              :when (or (eq hlines t)
                        (member j hlines :test #'=))
                :do (format t "\\hline~%")
              :do
                 (loop :for i :below width
                       :do (format t "~A" (funcall format-entry (aref array j i)))
                       :if (= (1+ i) width)
                         :do (format t "\\\\")
                       :else :do
                         (format t " & "))
                 (format t "~%"))
        (when (or (eq hlines t)
                  (member height hlines :test #'=))
          (format t "\\hline~%"))))))

(defun format-array-as-grid (array &key (format-entry 'normalise-string))
  (let ((alignment (with-output-to-string (s)
                     (format s "|")
                     (dotimes (i (array-dimension array 1))
                       (format s "c|")))))
    (format-array array alignment :hlines t :format-entry format-entry)))
