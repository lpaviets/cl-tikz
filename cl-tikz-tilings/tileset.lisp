(in-package #:org.numbra.cl-tikz-tilings)

;;; Tile sets

(defclass tileset ()
  ((name :initarg :name
         :reader tileset-name
         :type symbol)
   (tiles :initarg :tiles
          :accessor tileset-tiles
          :type hash-table)))

(defmacro dotiles ((tile tileset) &body body)
  `(dohash (,tile) (tileset-tiles ,tileset)
     ,@body))

(defun make-tileset (name tiles)
  (make-instance 'tileset :name name :tiles tiles))

(defmacro defwangtiles (name &body tiles-sides)
  (with-gensyms  (tiles tile)
    `(make-tileset ',name (let ((,tiles (make-hash-table :test 'equal)))
                            (dolist (,tile ',tiles-sides ,tiles)
                              (setf (gethash (apply 'make-wang-tile ',name ,tile) ,tiles) t))))))

(defun all-valid-neighbours (tileset tile dir)
  (let ((valid-neighbours (make-hash-table :test 'equal
                                           :size (hash-table-size (tileset-tiles tileset)))))
    (dotiles (t2 tileset)
             (when (valid-neighbour-p tile t2 dir)
               (setf (gethash t2 valid-neighbours) t)))
    valid-neighbours))

(defun tile-fits-with-p (tile left down right up)
  (and (or (not left)
           (valid-neighbour-p tile left :left))
       (or (not down)
           (valid-neighbour-p tile down :down))
       (or (not right)
           (valid-neighbour-p tile right :right))
       (or (not up)
           (valid-neighbour-p tile up :up))))

;; TODO: Add a way to define a product tileset for two SFTs
;; May be only for Wang tiles for now ?
;; With current architecture, tiles have a class, might need to give the product one a class inheriting from both ...

(defun make-product-tileset (name keep-tile-fun &rest tilesets)
  (labels ((cartesian-product (lists)
             (cond
               ((endp lists) nil)
               ((endp (cdr lists)) (car lists))
               (t
                (let ((prev (cartesian-product (cdr lists)))
                      new-acc)
                  (dolist (x (car lists))
                    (dolist (y prev)
                      (push (cons x y) new-acc)))
                  new-acc)))))
    (let ((all-tiles (cartesian-product (mapcar #'tileset-tiles tilesets))))
      (make-tileset name (remove-if-not keep-tile-fun all-tiles)))))
