(in-package :crawler)

(defclass tile ()
  ((x :reader x
      :initarg :x)
   (y :reader y
      :initarg :y)
   (walkablep :accessor walkablep
              :initarg :walkablep)
   (region-id :accessor region-id
              :initarg :region-id)
   (adjacent-regions :accessor adjacent-regions
                     :initform nil)
   (junctionp :accessor junctionp
              :initform nil)
   (attrs :accessor attrs
          :initform nil)))

(defstruct (neighbor-data
            (:conc-name nil))
  n s e w nw ne se sw)

(defun make-tile (x y &key walkablep region-id)
  "Make a tile instance."
  (make-instance 'tile :x x :y y :walkablep walkablep :region-id region-id))

(defun tile (x y)
  "Get a tile instance with the specified coordinates from the tile map array."
  (with-slots (width tile-map) *dungeon*
    (aref tile-map (+ x (* y width)))))

(defun (setf tile) (tile x y)
  "Add a tile instance with the specified coordinates to the tile map array."
  (with-slots (width tile-map) *dungeon*
    (setf (aref tile-map (+ x (* y width))) tile)))

(defun get-tile-data (tile func start end)
  "Get data for a tile that is within a specified area."
  (with-slots (width height) *dungeon*
    (with-slots (x y) tile
      (when (and (>= x (first start))
                 (>= y (second start))
                 (<= x (+ (1- width) (first end)))
                 (<= y (+ (1- height) (second end))))
        (funcall func tile)))))

(defun get-neighbors (tile func start end)
  "Create a structure containing data for all of a tile's neighbors that are within a specified
area."
  (with-slots (x y) tile
    (make-neighbor-data
     :n (get-tile-data (tile x (1- y)) func start end)
     :s (get-tile-data (tile x (1+ y)) func start end)
     :e (get-tile-data (tile (1+ x) y) func start end)
     :w (get-tile-data (tile (1- x) y) func start end)
     :nw (get-tile-data (tile (1- x) (1- y)) func start end)
     :ne (get-tile-data (tile (1+ x) (1- y)) func start end)
     :se (get-tile-data (tile (1+ x) (1+ y)) func start end)
     :sw (get-tile-data (tile (1- x) (1+ y)) func start end))))

(defun map-tiles (filter func effect &key (start '(1 1)) (end '(-1 -1)))
  "Loop over all tiles within a specified area, calling an effect for each tile that passes through
a filter. The default as defined by start and end parameters is all non-edge map tiles."
  (with-slots (width height) *dungeon*
    (loop with map-affected-p
          for x from (first start) below (+ width (first end))
          do (loop for y from (second start) below (+ height (second end))
                   for tile = (tile x y)
                   for neighbors = (get-neighbors tile func start end)
                   when (funcall filter tile neighbors)
                     do (let ((value (funcall effect tile neighbors)))
                          (setf map-affected-p (or map-affected-p value))))
          finally (return map-affected-p))))

(defun collect-tiles (filter func processor &key (start '(1 1)) (end '(-1 -1)))
  "Collect filtered map tiles into a list, and run a processor on them."
  (let ((tiles))
    (map-tiles
     filter
     func
     (lambda (tile neighbors) (push (list tile neighbors) tiles))
     :start start
     :end end)
    (loop while tiles
          do (loop with (tile neighbors) = (pop tiles)
                   while (funcall filter tile neighbors)
                   for new = (funcall processor tile neighbors)
                   when new
                     do (push new tiles)))))

(defun carvablep (tile neighbors)
  "Check if a tile and all of its neighbors are unwalkable."
  (with-slots (n s e w nw ne se sw) neighbors
    (every #'null (list (walkablep tile) n s e w nw ne se sw))))

(defun connectorp (tile neighbors)
  "Check if a tile can connect two different regions."
  (with-slots (n s e w) neighbors
    (and (not (region-id tile))
         (or (and (not (eql n s)) n s)
             (and (not (eql e w)) e w)))))

(defun dead-end-p (tile neighbors)
  "Check if a tile is surrounded by three or more unwalkable tiles."
  (with-slots (n s e w) neighbors
    (let ((dirs (remove-if #'identity (list n s e w))))
      (and (walkablep tile)
           (>= (length dirs) 3)))))

(defun erode-dead-end (tile neighbors)
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

(defun make-connector (tile neighbors)
  "Mark a tile as a connector between two regions."
  (with-slots (n s e w) neighbors
    (with-slots (connectors) *dungeon*
      (setf (adjacent-regions tile) (remove nil (list n s e w)))
      (dolist (region-id (adjacent-regions tile))
        (push tile (connectors (get-region region-id)))))))

(defun make-wall (tile neighbors)
  "Mark a tile as unwalkable and remove its region."
  (declare (ignore neighbors))
  (setf (walkablep tile) nil
        (region-id tile) nil)
  tile)

(defun adjacent-junction-p (tile)
  "Check if a tile has a junction adjacent to it."
  (with-slots (x y) tile
    (or (junctionp (tile x (1- y)))
        (junctionp (tile x (1+ y)))
        (junctionp (tile (1- x) y))
        (junctionp (tile (1+ x) y)))))

(defun make-junction (tile)
  "Mark a tile as a junction between two regions if it has no adjacent junctions."
  (unless (adjacent-junction-p tile)
    (setf (region-id tile) 0
          (walkablep tile) t
          (junctionp tile) t)))

(defun make-extra-junction (tile neighbors)
  "Check if a tile should become an extra junction, and mark it as such if so."
  (declare (ignore neighbors))
  (when (< (rng 'range-i) (attr 'junction-rate))
    (make-junction tile)))
