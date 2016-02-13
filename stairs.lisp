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

(defun upstairs-choices ()
  "Get a list of tiles in a random room where an entrance staircase can be placed."
  (with-slots (width height rooms) *dungeon*
    (with-slots (x1 y1 x2 y2) (rng 'elt :list rooms)
      (mapcar #'first
              (collect-tiles
               (lambda (tile neighbors) (not (adjacent-junction-p tile neighbors)))
               #'walkablep
               :start `(,x1 ,y1)
               :end `(,(- x2 width) ,(- y2 height)))))))

(defun create-upstairs ()
  "Create the entrance staircase."
  (let ((tile (rng 'elt :list (upstairs-choices))))
    (pushnew :stairs-up (map-features tile))
    (setf (distance tile) 0)
    tile))

(defun create-downstairs (upstairs)
  (let ((queue (make-queue))
        (downstairs upstairs))
    (enqueue upstairs queue)
    (loop :until (queue-empty-p queue)
          :do (loop :with current = (dequeue queue)
                    :with neighbors = (walkable-neighbors current)
                    :for neighbor :in neighbors
                    :when (= (distance neighbor) -1)
                      :do (setf (distance neighbor) (1+ (distance current)))
                          (enqueue neighbor queue)
                          (unless (or (member :corridor (map-features neighbor))
                                      (member :junction (map-features neighbor))
                                      (adjacent-junction-p neighbor)
                                      (< (distance neighbor) (distance downstairs)))
                            (setf downstairs neighbor))))
    (pushnew :stairs-down (map-features downstairs))))

(defun create-stairs ()
  (create-downstairs (create-upstairs)))
