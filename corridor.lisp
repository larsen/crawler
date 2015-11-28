(in-package :crawler)

(defun get-cell (cell dir &optional (scalar 1) tilep)
  (with-slots (tile-map) *dungeon*
    (let* ((scale (list (* (first dir) (1+ scalar))
                        (* (second dir) (1+ scalar))))
           (cell (list (+ (first cell) (first scale))
                       (+ (second cell) (second scale)))))
      (if tilep (apply #'aref tile-map cell) cell))))

(defun pick-cell (cells)
  (with-slots (windiness) *dungeon*
    (if (> (rng 'range-i) windiness)
        (rng 'elt :list cells)
        (first (last cells)))))

(defun neighbors (x y)
  (with-slots (width height tile-map) *dungeon*
    (remove-if
     (lambda (dir)
       (or (< (+ x (first dir)) 1)
           (< (+ y (second dir)) 1)
           (> (+ x (first dir)) (- width 2))
           (> (+ y (second dir)) (- height 2))
           (walkablep (get-cell `(,x ,y) dir 1 t))))
     '((-1 0) (1 0) (0 -1) (0 1)))))

(defun carve-tile (cells)
  (with-slots (tile-map regions current-region) *dungeon*
    (let* ((cell (pick-cell cells))
           (neighbors (neighbors (first cell) (second cell))))
      (deletef cells cell :test #'equal)
      (when neighbors
        (loop with dir = (rng 'elt :list neighbors)
              with new-cell = (list cell (get-cell cell dir 1))
              for i below 2
              for tile = (get-cell cell dir i t)
              do (setf (walkablep tile) t
                       (region-id tile) current-region)
              finally (appendf cells new-cell))))
    cells))

(defun carve (tile neighbors)
  (declare (ignore neighbors))
  (with-slots (width height regions current-region) *dungeon*
    (with-slots (x y walkablep region-id) tile
      (setf walkablep t
            region-id (incf current-region)
            (gethash region-id regions) (make-instance 'region :id region-id))
      (push tile (tiles (gethash region-id regions)))
      (loop with cells = `((,x ,y))
            while cells
            do (setf cells (carve-tile cells))))))

(defun carvablep (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (every #'null (list (walkablep tile) n ne e se s sw w nw))))
