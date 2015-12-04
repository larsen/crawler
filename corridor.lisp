(in-package :crawler)

(defun get-cell (cell dir &optional (scalar 1) tilep)
  "Get a cell to carve."
  (let* ((scale (list (* (first dir) (1+ scalar))
                      (* (second dir) (1+ scalar))))
         (cell (list (+ (first cell) (first scale))
                     (+ (second cell) (second scale)))))
    (if tilep (apply #'tile cell) cell)))

(defun pick-cell (cells)
  "Select a cell to be checked whether or not it should be carved."
  (if (> (rng 'range-i) (attr 'windiness))
      (rng 'elt :list cells)
      (first (last cells))))

(defun neighbors (x y)
  "Get all offset coordinates representing a neighbor of the specified coordinates."
  (with-slots (width height) *dungeon*
    (remove-if
     (lambda (dir)
       (or (< (+ x (first dir)) 1)
           (< (+ y (second dir)) 1)
           (> (+ x (first dir)) (- width 2))
           (> (+ y (second dir)) (- height 2))
           (walkablep (get-cell `(,x ,y) dir 1 t))))
     '((-1 0) (1 0) (0 -1) (0 1)))))

(defun carve-tile (cells)
  "Pick a cell and try carving it."
  (let* ((cell (pick-cell cells))
         (neighbors (neighbors (first cell) (second cell))))
    (deletef cells cell :test #'equal)
    (when neighbors
      (loop with dir = (rng 'elt :list neighbors)
            with new-cell = (list cell (get-cell cell dir 1))
            for i below 2
            for tile = (get-cell cell dir i t)
            do (setf (walkablep tile) t
                     (region-id tile) (current-region *dungeon*))
            finally (appendf cells new-cell))))
  cells)

(defun carve (tile neighbors)
  "Carve all tiles into a corridor starting at the specified tile."
  (declare (ignore neighbors))
  (with-slots (regions current-region) *dungeon*
    (with-slots (x y walkablep region-id) tile
      (setf walkablep t
            region-id (incf current-region)
            (gethash region-id regions) (make-instance 'region :id region-id))
      (loop with cells = `((,x ,y))
            while cells
            do (setf cells (carve-tile cells))))))
