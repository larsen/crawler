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
    (every #'(lambda (x) (eq x :wall))
           (list (terrain tile) n ne e se s sw w nw))))

(defun carve (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (with-slots (data regions current-region) *dungeon*
      (with-slots (x y visitedp terrain region-id) tile
        (setf visitedp t
              terrain :floor
              region-id (incf current-region)
              (gethash region-id regions) (make-instance 'region :id region-id))
        (push tile (tiles (gethash region-id regions)))
        (loop with neighbors = (neighbors x y)
              with stack = `((,x ,y ,neighbors))
              while stack
              for (nx ny nn) = (pop stack)
              do (loop while neighbors
                       for (u v) = (car neighbors)
                       do (if (visitedp (aref data u v))
                              (pop neighbors)
                              (progn
                                (dolist (to-carve `(,(aref data u v)
                                                    ,(aref data (/ (+ x u) 2) (/ (+ y v) 2))))
                                  (setf (terrain to-carve) :floor
                                        (region-id to-carve) current-region)
                                  (push to-carve (tiles (gethash region-id regions))))
                                (push `(,x ,y ,(cdr neighbors)) stack)
                                (setf neighbors (neighbors u v)
                                      x u
                                      y v
                                      (visitedp (aref data x y)) t))))
                 (setf x nx
                       y ny
                       neighbors nn))))))
