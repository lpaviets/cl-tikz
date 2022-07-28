(in-package #:org.numbra.cl-tikz)

(defmacro with-gensyms (gensyms &body body)
  "GENSYMS is a list of symbols SYM, or of pairs (SYM VAL).
Binds the symbols SYM to an uninterned symbol, as if using gensym.
For each pair (SYM VAL), the value of VAL will be evaluated once before
BODY, and bound to the gensym associated to SYM.
Example:
(defmacro double (x)
  (with-gensyms ((gx x))
    `(+ ,gx ,gx)))"
  (loop :for sym :in gensyms
        :for gensym = (gensym (if (symbolp sym)
                                  (symbol-name sym)
                                  (symbol-name (first sym))))
        :if (symbolp sym)
          :collect `(,sym ',gensym) :into gensym-list
        :else
          :collect `(,(first sym) ',gensym) :into gensym-list
          :and
            :collect `(list ,(first sym) ,(second sym)) :into eval-list
        :finally
           (return `(let ,gensym-list
                      (list 'let (list ,@eval-list)
                            ,@body)))))

(defmacro xor (&rest forms)
  "XOR evaluates each form of FORMS, left to right.
The evaluation terminates when exactly two forms evaluate to true.
The final result is either FORM when FORM is the only form of FORM
that evaluates to true, or NIL otherwise. In the first case, all
the forms have been evaluated"
  (with-gensyms (flag current xor-block)
    `(block ,xor-block
       (let ((,flag nil))
         ,@(loop :for form :in forms
                 :collect `(let ((,current ,form))
                             (when ,current
                               (if ,flag
                                   (return-from ,xor-block nil)
                                   (setf ,flag ,current)))))
         ,flag))))

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun to-lowercase-string (designator)
  (typecase designator
    ((or string symbol) (string-downcase (string designator)))
    (t (princ-to-string designator))))

(defmacro dohash ((key &optional val) table &body body)
  (with-gensyms (gval)
    `(block nil
       (maphash (lambda (,key ,(or val gval))
                  (declare (ignorable ,key ,(or val gval)))
                  ,@body)
                ,table))))

(defun list-to-set (list)
  (let ((table (make-hash-table)))
    (dolist (x list)
      (setf (gethash x table) t))
    table))

(defun set-to-list (set)
  (let (list)
    (dohash (x) set
      (push x list))
    list))

(defun nshuffle (sequence)
  (loop :for i :from (length sequence) :downto 2
        :do (rotatef (elt sequence (random i))
                     (elt sequence (1- i))))
  sequence)

(defun %rotate-list (list n)
  (let ((beg (butlast list n))
        (end (last list n)))
    (append end beg)))

(defun %rotate-array (array n)
  (let* ((len (length array))
         (new-array (make-array len)))
    (dotimes (i len)
      (setf (aref new-array i) (aref array (mod (- i n) len))))
    new-array))

(defun rotate-sequence (seq n)
  (etypecase seq
    (array (%rotate-array seq n))
    (list (%rotate-list seq n))))

(defmacro capture-stdout (&body body)
  "Redirect all the things printed on stdout by BODY to a string, and
return this string once BODY terminates"
  (with-gensyms ((string (make-string-output-stream)))
    `(progn
       (let ((*standard-output* ,string))
         ,@body)
       (get-output-stream-string ,string))))
