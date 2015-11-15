(in-package :crawler)

(defmethod neighbors (x y)
  (shuffle
   (remove-if-not
    (lambda (x) (and (< -1 (first x) (array-dimension (data *dungeon*) 0))
                (< -1 (second x) (array-dimension (data *dungeon*) 1))))
    `((,x ,(+ y 2))
      (,(- x 2) ,y)
      (,x ,(- y 2))
      (,(+ x 2) ,y)))))

(defmethod carve (x y)
  (with-slots (data current-region) *dungeon*
    (setf (visitedp (aref data x y)) t
          (terrain (aref data x y)) :corridor
          (region (aref data x y)) (incf current-region))
    (loop with neighbors = (neighbors x y)
          with stack = `((,x ,y ,neighbors))
          while stack
          for (nx ny nn) = (pop stack)
          do (loop while neighbors
                   for (u v) = (car neighbors)
                   for tile1 = (aref data u v)
                   for tile2 = (aref data (/ (+ x u) 2) (/ (+ y v) 2))
                   do (if (visitedp (aref data u v))
                          (setf neighbors (cdr neighbors))
                          (progn
                            (setf (terrain tile1) :corridor
                                  (terrain tile2) :corridor
                                  (region tile1) current-region
                                  (region tile2) current-region)
                            (push `(,x ,y ,(cdr neighbors)) stack)
                            (setf neighbors (neighbors u v)
                                  x u
                                  y v
                                  (visitedp (aref data x y)) t))))
             (setf x nx
                   y ny
                   neighbors nn))))

(defmethod carvablep ()
  (with-slots (data) *dungeon*
    (loop named out for x from 1 below (1- (array-dimension data 0))
          do (loop for y from 1 below (1- (array-dimension data 1))
                   do (when (and (eq (terrain (aref data x y)) :wall)
                                 (eq (terrain (aref data (1- x) (1- y))) :wall)
                                 (eq (terrain (aref data x (1- y))) :wall)
                                 (eq (terrain (aref data (1+ x) (1- y))) :wall)
                                 (eq (terrain (aref data (1- x) y)) :wall)
                                 (eq (terrain (aref data (1+ x) y)) :wall)
                                 (eq (terrain (aref data (1- x) (1+ y))) :wall)
                                 (eq (terrain (aref data x (1+ y))) :wall)
                                 (eq (terrain (aref data (1+ x) (1+ y))) :wall))
                        (return-from out (list x y)))))))
