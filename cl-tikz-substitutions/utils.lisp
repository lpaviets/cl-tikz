(in-package #:org.numbra.cl-tikz-substitutions)

(defun split-on-keywords (sequence keywords)
  (loop :with subseq = nil
        :with splitted = nil
        :for elt :in sequence
        :if (member elt keywords :test 'eq)
          :do (when subseq (push (nreverse subseq) splitted))
              (setf subseq (list elt))
        :else
          :do (push elt subseq)
        :finally (push (nreverse subseq) splitted)
                 (return (nreverse splitted))))

(defgeneric draw-substitution (base n))
(defmethod draw-substitution :before (base n)
  (when (< n 1)
    (error "Number ~S of steps cannot be negative" n)))

(defun draw-substitution-in-file (base name steps)
  (let ((filename (merge-pathnames (format nil "examples/~A.tex" name))))
    (format t "~&Writing to file ~A~%" filename)
    (with-preamble-to-file (filename) ()
      (with-env (:tikzpicture)
        (draw-substitution base steps)))))
