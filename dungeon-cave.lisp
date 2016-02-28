(in-package :crawler)

(defclass cave (dungeon)
  ((buffers :initform (list 0 1))))

(defun fill-floors-by-percent (percent)
  (loop :with count = (tile-count :percent percent :perimeterp nil)
        :for tile = (random-tile :perimeterp nil)
        :until (zerop count)
        :when (wallp tile)
          :do (make-floor tile)
              (decf count)))

(defun evolve-tile (tile neighbors)
  (with-slots (x y) tile
    (with-neighbors neighbors
      (let ((dirs (remove-if-not #'wallp (list n ne e se s sw w nw)))
            (target (tile x y :buffer (next-buffer))))
        (setf (walkablep target) (walkablep tile)
              (map-features target) (map-features tile))
        (cond
          ((< (length dirs) 4)
           (make-floor target))
          ((> (length dirs) 5)
           (make-wall target)))))))

(defun evolve (&key generations)
  (dotimes (i generations)
    (map-tiles (constantly t) #'identity #'evolve-tile)
    (swap-buffers)))

(defmethod build ((type (eql :cave)) &key)
  (fill-floors-by-percent 0.45)
  (evolve :generations 5))
