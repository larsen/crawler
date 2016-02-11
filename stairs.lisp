(in-package :crawler)

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

(defun pick-downstairs (tile)
  "Pick a valid location for the exit staircase adjacent to the given tile."
  (with-slots (n s e w ne nw sw se) (get-neighbors tile #'walkablep)
    (let ((walkable-neighbors (remove nil (list n s e w ne nw sw se))))
      (when (and (not (adjacent-junction-p tile))
                 (> (length walkable-neighbors) 6))
        tile))))

(defun create-upstairs ()
  "Create the entrance staircase."
  (let ((tile (rng 'elt :list (stairs-valid-p))))
    (setf (map-feature tile) :stairs-up
          (distance tile) 0)
    tile))

(defun create-downstairs (upstairs)
  "Create the exit staircase."
  (let ((frontier (make-instance 'priority-queue)))
    (with-slots (distance) upstairs
      (enqueue frontier upstairs distance)
      (loop :while (peep-at-queue frontier)
            :for tile = (dequeue frontier)
            :for selected = (pick-downstairs tile)
            :for stairs = (or selected stairs)
            :do (with-slots (n s e w) (get-neighbors tile #'identity)
                 (loop :with distance = (1+ (distance tile))
                       :for neighbor :in (list n s e w)
                       :when (and (walkablep neighbor)
                                 (> (distance neighbor) distance))
                         :do (setf (distance neighbor) distance)
                            (enqueue frontier neighbor (distance neighbor))))
            :finally (setf (map-feature stairs) :stairs-down)))))


(defun create-stairs ()
  "Create the entrance and exit staircases."
  (create-downstairs (create-upstairs)))
