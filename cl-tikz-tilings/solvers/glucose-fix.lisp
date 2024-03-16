(in-package :cl-sat.glucose)

;; TODO: may be move this somewhere else ?

;; Use parallel version !

;; The version installed by CL-SAT is buggy, or at least doesn't work for unknown reasons.

;; Most of the time, BY FAR, is spent printing the CNF to the file given to
;; Glucose as input.
;; TODO: try to be faster ?
(setf *glucose-home* "/home/aminumbra/from_source/glucose/")

(defun glucose-binary (&optional (*glucose-home* *glucose-home*))
  (merge-pathnames "parallel/glucose-syrup" *glucose-home*))

(defmethod solve :before ((input list)
                          (solver (eql :glucose-parallel))
                          &key &allow-other-keys)
  (format t "~&Start processing SAT instance as a list of clauses ...~%"))

(defmethod solve :before ((input sat-instance)
                          (solver (eql :glucose-parallel))
                          &key &allow-other-keys)
  (format t "~&Done converting to CNF~%")
  (format t "~&Solving CNF ...~%")
  (force-output))

(defmethod solve ((input pathname)
                  (solver (eql :glucose-parallel))
                  &rest options &key debug &allow-other-keys)
  (remf options :debug)
  (pushnew "-model " options :test 'string-equal)
  (with-temp (dir :directory t :template "glucose.XXXXXXXX" :debug debug)
    ;; Ugly command: parallel version is broken, so does not output file ...
    (let* ((command (format nil "cd ~a; ~a ~{~A~^ ~}~a > ~a"
                            (namestring dir)
                            (namestring (glucose-binary))
                            options (namestring input) "result")))
      (format t "~&; ~a~%" command)
      (trivia:multiple-value-match (uiop:run-program command
                                                     :output *standard-output*
                                                     :error-output *error-output*
                                                     :ignore-error-status t)
        ((_ _ 0)
         ;; indeterminite
         (values nil nil nil))
        ((_ _ 10)
         ;; sat
         ;; Poor hack
         ;; (with-open-file (in (concatenate 'string (namestring dir) "/result"))
         ;;   (loop :for line = (read-line in nil nil)
         ;;         :while line
         ;;         :do (format t line)
         ;;             (terpri)))
         (let ((fix-result-command (format nil "cd ~a; sed -i -e '/^v/!d; s/^v //' ~a"
                                           (namestring dir)
                                           "result")))
           (uiop:run-program fix-result-command
                             :output *standard-output*
                             :error-output *error-output*)
           (parse-assignments (format nil "~a/result" dir) cl-sat:*instance*))) ; Bug here ?
        ((_ _ 20)
         ;; unsat
         (values nil nil t))))))

(in-package #:cl-sat)

(defun print-cnf (instance &optional (stream *standard-output*) (*verbosity* *verbosity*))
  (ematch instance
    ((sat-instance cnf variables)
     ;; Just use a cache ...
     (let ((atom-to-pos (make-hash-table :size (length variables))))
       (loop :for atom :across variables
             :for i :from 1
             :do (setf (gethash atom atom-to-pos) i))
       (format t "~&Start writing DIMACS file~%")
       (match cnf
         ((or (list* 'and clauses)
              (<> clauses (list cnf)))
          (format stream "~&p cnf ~A ~A" (length variables) (length clauses))

          (loop :for c :in clauses
                :do (ematch c
                      ((or (list* 'or terms)
                           (<> terms (list c)))
                       (format stream "~&")
                       (dolist (term terms)
                         (format stream "~D " (ematch term
                                                ((list 'not atom)
                                                 (- (gethash atom atom-to-pos)))
                                                (atom
                                                 (gethash atom atom-to-pos)))))

                       (format stream "0"))))
          (fresh-line stream)))
       (format t "~&Done writing DIMACS file~%")))))
