(in-package :crawler)

(defmethod neighbors (x y)
  (shuffle
   (remove-if-not
    (lambda (x) (and (< -1 (first x) (width *dungeon*))
                (< -1 (second x) (height *dungeon*))))
    `((,x ,(+ y 2))
      (,(- x 2) ,y)
      (,x ,(- y 2))
      (,(+ x 2) ,y)))))

(defun carvablep (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (every #'null (list (walkablep tile) n ne e se s sw w nw))))

(defun carve (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (with-slots (tile-map visitedp regions current-region) *dungeon*
      (with-slots (x y walkablep region-id) tile
        (setf walkablep t
              region-id (incf current-region)
              (gethash region-id regions) (make-instance 'region :id region-id))
        (push tile visitedp)
        (push tile (tiles (gethash region-id regions)))
        (loop with neighbors = (neighbors x y)
              with stack = `((,x ,y ,neighbors))
              while stack
              for (nx ny nn) = (pop stack)
              do (loop while neighbors
                       for (u v) = (car neighbors)
                       do (if (member (aref tile-map u v) visitedp)
                              (pop neighbors)
                              (progn
                                (dolist (to-carve `(,(aref tile-map u v)
                                                    ,(aref tile-map (/ (+ x u) 2) (/ (+ y v) 2))))
                                  (setf (walkablep to-carve) t
                                        (region-id to-carve) current-region)
                                  (push to-carve (tiles (gethash region-id regions))))
                                (push `(,x ,y ,(cdr neighbors)) stack)
                                (setf neighbors (neighbors u v)
                                      x u
                                      y v)
                                (push (aref tile-map x y) visitedp))))
                 (setf x nx
                       y ny
                       neighbors nn))))))
