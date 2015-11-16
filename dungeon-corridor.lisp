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
                   do (if (visitedp (aref data u v))
                          (setf neighbors (cdr neighbors))
                          (progn
                            (dolist (tile `(,(aref data u v)
                                            ,(aref data (/ (+ x u) 2) (/ (+ y v) 2))))
                              (setf (terrain tile) :corridor
                                    (region tile) current-region))
                            (push `(,x ,y ,(cdr neighbors)) stack)
                            (setf neighbors (neighbors u v)
                                  x u
                                  y v
                                  (visitedp (aref data x y)) t))))
             (setf x nx
                   y ny
                   neighbors nn))))
