(in-package :crawler)

(defun staircase-suitable-p (tile &optional neighbors)
  (declare (ignore neighbors))
  (and (roomp tile)
       (not (junctionp tile))
       (not (adjacent-junction-p tile))))

(defun upstairs-choices ()
  (with-attrs (width height) :dungeon
    (with-slots (x1 y1 x2 y2) (rng 'elt :list (rooms *dungeon*))
      (mapcar #'first
              (collect-tiles
               #'staircase-suitable-p
               #'identity
               :start `(,x1 ,y1)
               :end `(,(- x2 width) ,(- y2 height)))))))

(defun downstairs-choices (region)
  (mapcar #'first
          (collect-tiles
           (lambda (tile neighbors)
             (declare (ignore neighbors))
             (and (staircase-suitable-p tile)
                  (= (region-id tile) region)))
           #'identity)))

(defun pick-downstairs (region)
  (let ((tile (rng 'elt :list (downstairs-choices region))))
    (add-feature tile :stairs-down)
    tile))

(defun create-upstairs ()
  (let ((tile (rng 'elt :list (upstairs-choices))))
    (add-feature tile :stairs-up)
    (setf (distance tile) 0)
    tile))

(defun create-downstairs (source)
  (let ((queue (make-queue))
        (goal source))
    (enqueue source queue)
    (loop :until (queue-empty-p queue)
          :do (loop :with current = (dequeue queue)
                    :with neighbors = (walkable-neighbors current)
                    :for tile :in neighbors
                    :when (= (distance tile) -1)
                      :do (enqueue tile queue)
                          (setf (distance tile) (1+ (distance current)))
                          (when (and (staircase-suitable-p tile)
                                     (> (distance tile) (distance goal)))
                            (setf goal tile))))
    (pick-downstairs (region-id goal))))
