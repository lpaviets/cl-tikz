(in-package #:cl-tikz/tiles)

(defmacro deftile ((tileset id name &key rules sides)
                   (&key background other-args)
                   &body body)
  "Creates a function MAKE-TILE-<NAME>-<ID> which takes as arguments:
- X, Y (mandatory arguments): the position of the tile.
- SIZE (keyword argument): The size of the tile. Default to 1.
Note that this makes the deftile macro anaphoric.
Indeed, in BODY, X, Y, SIZE will be captured by the generated
function, while the programmer never explicitly gave them as arguments.
- OTHER-ARGS: this should be a list of keys

Other arguments to this macro are:
- TILESET: a tileset structure. The keys are of the same type as ID, and the
values are the functions MAKE-TILE-TILESET-ID
- ID: a fixnum, unique identifier for the tile, between 0 and the tileset
size
- RULES: a list of rules. See `add-rules' for more information.
- SIDES: nil, or a list of length 4. In this order: left, down, right, up
sides of the tiles (in the case of Wang tiles/SFT). Each element must be
comparable with EQ.
- BACKGROUND: If not nil, the tile will first be filled with this colour
on the background
- OTHER-ARGS: this should be a list, each element being a symbol or a
pair (symbol default-value). It will be inserted as-is in the parameter
list of the generated function
- BODY: the body of the generated function"
  (let ((fun-name (symb 'make-tile- name #\- id))
        (fun-args (append '(&key (size 1)) other-args))
        (node-content (gensym))
        (options (gensym)))
    `(progn
       (defun ,fun-name (x y ,@fun-args)
         ,(format nil
                  "Function creating the tile ~a of the tileset ~a.
See `deftile' for more information"
                  id
                  tileset)
         (declare (ignorable size))

         (let ((,node-content
                 (capture-stdout
                   (with-env (tikzpicture)
                     ,@body)
                   (format t "~%")))
               (,options (list (cons "minimum size" size)
                               (cons "inner sep" "0pt")
                               ,@(when background (list (list 'quote (cons :fill background)))))))

           (draw-node x y :options ,options
                          :label ,node-content)))
       (tileset-add-tile-function ,tileset ,id ',fun-name)
       ,(when rules
          `(tileset-add-rules ,tileset ,rules))
       ,(when sides
          `(tileset-add-tile-sides ,tileset ,id ,sides)))))


(defun draw-tiling (solution tileset &key other-args)
  "Draw the tiling corresponding to SOLUTION with the tileset TILESET.
SOLUTION is a 2D-array, each cell of which is an ID belonging to TILESET"
  (destructuring-bind (n m) (array-dimensions solution)
    (loop :for i :below n :do
      (loop :for j :below m :do
        (let* ((tile (aref solution i j))
               (fun-tile (tileset-get-tile-function tileset tile)))
          (apply fun-tile i j other-args))))
    ;; (draw-grid 0 0 m n
    ;;            :options '(thin))
    ))
