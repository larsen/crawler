(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)
   (tile-size :reader tile-size
              :initarg :tile-size)
   (rooms :accessor rooms
          :initform nil)
   (regions :accessor regions
            :initform (make-hash-table))
   (current-region :accessor current-region
                   :initform 0)
   (tile-map :accessor tile-map
             :initarg :tile-map)))

(defun make-dungeon (w h tile-size &rest attrs)
  "Top-level dungeon creator."
  (make-generator attrs)
  (when (and (oddp w)
             (oddp h)
             (>= w (* (attr 'room-size-max) 2))
             (>= h (* (attr 'room-size-max) 2)))
    (setf *dungeon* (make-instance 'dungeon
                                   :w w
                                   :h h
                                   :tile-size tile-size
                                   :tile-map (make-array (* w h))))
    (format t "Random seed: ~a~%" (random-seed *generator*))
    (generate attrs)))

(defun generate (attrs)
  "Generate all parts of the dungeon."
  (create-walls)
  (create-rooms)
  (create-corridors)
  (create-connectors)
  (combine-regions)
  (remove-dead-ends)
  *dungeon*)

(defun create-walls ()
  "Fill the dungeon with all wall tiles."
  (with-slots (width height tile-map) *dungeon*
    (loop for x below width
          do (loop for y below height
                   do (setf (tile x y) (make-tile x y))))))

(defun create-rooms ()
  "Create rooms until the desired density is reached."
  (loop with max-rooms = (calculate-room-count (attr 'room-density))
        with tries = 0
        until (or (= (length (rooms *dungeon*)) max-rooms)
                  (>= tries 1000))
        do (create-room)
           (incf tries)))

(defun create-corridors ()
  "Carve out corridors from the remaining walls around rooms."
  (map-tiles #'carvablep #'walkablep #'carve :start '(1 1) :end '(-1 -1)))

(defun create-connectors ()
  "Mark all tiles that can connect two different regions."
  (map-tiles #'connectorp #'region-id #'make-connector :start '(1 1) :end '(-1 -1)))

(defun combine-regions ()
  "Join all regions by carving some connectors into junctions."
  (with-slots (regions) *dungeon*
    (loop with region-id = (rng 'elt :list (hash-table-keys regions))
          while (connectors (gethash region-id regions))
          for connector = (random-connector region-id)
          do (merge-region region-id connector))
    (map-tiles #'connectorp #'region-id #'make-extra-junction :start '(1 1) :end '(-1 -1))))

(defun remove-dead-ends ()
  "Remove all dead-end tiles, resulting in corridors that only lead to rooms and other corridors."
  (let ((dead-ends))
    (map-tiles
     #'dead-end-p
     #'walkablep
     (lambda (tile neighbors) (push (list tile neighbors) dead-ends))
     :start '(1 1) :end '(-1 -1))
    (loop while dead-ends
          do (loop with (tile neighbors) = (pop dead-ends)
                   while (dead-end-p tile neighbors)
                   for new = (contract-dead-end tile neighbors)
                   when new
                     do (push new dead-ends)))))

(defun contract-dead-end (tile neighbors)
  "Remove a dead-end tile, and return the next dead-end adjacent to it."
  (setf (walkablep tile) nil
        (region-id tile) nil)
  (with-slots (x y) tile
    (with-slots (n s e w) neighbors
      (let* ((dirs (remove-if #'null `((,(tile x (1- y)) . ,n)
                                       (,(tile x (1+ y)) . ,s)
                                       (,(tile (1+ x) y) . ,e)
                                       (,(tile (1- x) y) . ,w))
                              :key #'cdr))
             (new-neighbors (get-neighbors (caar dirs) #'walkablep '(1 1) '(-1 -1))))
        (when (and (= (length dirs) 1)
                   (dead-end-p (caar dirs) new-neighbors))
          (list (caar dirs) new-neighbors))))))
