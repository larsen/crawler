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

(defun stairs-valid-p ()
  "Get a list of tiles where stairs are allowed to be placed."
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
  (let ((tile (rng 'elt :list (stairs-valid-p))))
    (setf (map-feature tile) :stairs-up
          (distance tile) 0)
    tile))

(defun create-downstairs (upstairs)
  (let ((queue (make-queue))
        (downstairs upstairs))
    (enqueue upstairs queue)
    (loop :until (queue-empty-p queue)
          :do (loop :with current = (dequeue queue)
                    :for neighbor :in (walkable-neighbors current)
                    :when (= (distance neighbor) -1)
                      :do (setf (distance neighbor) (1+ (distance current)))
                          (enqueue neighbor queue)
                          (unless (or (adjacent-junction-p neighbor)
                                       (< (distance neighbor) (distance downstairs)))
                            (setf downstairs neighbor))))
    (setf (map-feature downstairs) :stairs-down)))

(defun create-stairs ()
  (create-downstairs (create-upstairs)))
