(in-package :crawler)

(defstruct (queue)
  (items)
  (tail))

(defun queue-empty-p (queue)
  "Check if a queue is empty."
  (endp (queue-items queue)))

(defun enqueue (item queue)
  "Enqueue an item to a queue."
  (if (queue-empty-p queue)
      (setf (queue-items queue) (list item)
            (queue-tail queue) (queue-items queue))
      (setf (cdr (queue-tail queue)) (list item)
            (queue-tail queue) (cdr (queue-tail queue))))
  queue)

(defun dequeue (queue)
  "Dequeue an item from a queue."
  (unless (queue-empty-p queue)
    (pop (queue-items queue))))

(defun staircase-suitable-p (tile &optional neighbors)
  "Check if a tile is allowed to become a staircase."
  (declare (ignore neighbors))
  (and (roomp tile)
       (not (junctionp tile))
       (not (adjacent-junction-p tile))))

(defun upstairs-choices ()
  "Get a list of tiles in a random room where an entrance staircase can be placed."
  (with-slots (width height rooms) *dungeon*
    (with-slots (x1 y1 x2 y2) (rng 'elt :list rooms)
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

(defun pick-upstairs ()
  "Create the entrance staircase."
  (let ((tile (rng 'elt :list (upstairs-choices))))
    (add-feature tile :stairs-up)
    (setf (distance tile) 0)
    tile))

(defun pick-downstairs (region)
  (let ((tile (rng 'elt :list (downstairs-choices region))))
    (add-feature tile :stairs-down)
    tile))

(defun create-upstairs ()
  "Create the entrance staircase."
  (let ((tile (rng 'elt :list (upstairs-choices))))
    (pushnew :stairs-up (map-features tile))
    (setf (distance tile) 0)
    tile))

(defun create-downstairs (source)
  "Create the exit staircase."
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

(defun create-stairs ()
  "Create the entrance and exit staircases."
  (create-downstairs (create-upstairs)))
