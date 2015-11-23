(in-package :crawler)

(defmethod neighbors (x y)
  (shuffle
   (remove-if-not
    (lambda (x) (and (< -1 (first x) (w *dungeon*))
                (< -1 (second x) (h *dungeon*))))
    `((,x ,(+ y 2))
      (,(- x 2) ,y)
      (,x ,(- y 2))
      (,(+ x 2) ,y)))))

(defun carve (tile &optional n ne e se s sw w nw)
  (declare (ignore n ne e se s sw w nw))
  (with-slots (data regions current-region) *dungeon*
    (with-slots (x y visitedp terrain region-id) tile
      (setf visitedp t
            terrain :corridor
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
                                (setf (terrain to-carve) :corridor
                                      (region-id to-carve) current-region)
                                (push to-carve (tiles (gethash region-id regions))))
                              (push `(,x ,y ,(cdr neighbors)) stack)
                              (setf neighbors (neighbors u v)
                                    x u
                                    y v
                                    (visitedp (aref data x y)) t))))
               (setf x nx
                     y ny
                     neighbors nn)))))
