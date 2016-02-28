(in-package :crawler)

(defclass tile ()
  ((x :reader x
      :initarg :x)
   (y :reader y
      :initarg :y)
   (walkablep :accessor walkablep
              :initarg :walkablep
              :initform nil)
   (region-id :accessor region-id
              :initarg :region-id
              :initform nil)
   (adjacent-regions :accessor adjacent-regions
                     :initform nil)
   (map-features :accessor map-features
                :initform '(:wall))
   (distance :accessor distance
             :initform -1)))

(defstruct (neighbor-data
            (:conc-name nil))
  n s e w nw ne se sw)

(defun make-tile (x y buffer)
  (setf (tile x y :buffer buffer) (make-instance 'tile :x x :y y)))

(defun tile (x y &key buffer)
  (with-slots (tiles) *dungeon*
    (aref tiles x y (or buffer (current-buffer)))))

(defun (setf tile) (tile x y &key buffer)
  (with-slots (tiles) *dungeon*
    (setf (aref tiles x y (or buffer (next-buffer))) tile)))

(defun tile-count (&key (percent 1) (perimeterp t))
  (with-attrs (width height) :dungeon
    (let ((i (if perimeterp 0 2)))
      (round (* (- width i) (- height i) percent)))))

(defun random-tile (&key (perimeterp t))
  (with-attrs (width height) :dungeon
    (let* ((min (if perimeterp 0 1))
           (max (if perimeterp 1 2))
           (x (rng 'range-int :min min :max (- width max)))
           (y (rng 'range-int :min min :max (- height max))))
      (tile x y))))

(defun get-neighbors (tile func)
  (with-slots (x y) tile
    (make-neighbor-data
     :n (funcall func (tile x (1- y)))
     :s (funcall func (tile x (1+ y)))
     :e (funcall func (tile (1+ x) y))
     :w (funcall func (tile (1- x) y))
     :nw (funcall func (tile (1- x) (1- y)))
     :ne (funcall func (tile (1+ x) (1- y)))
     :se (funcall func (tile (1+ x) (1+ y)))
     :sw (funcall func (tile (1- x) (1+ y))))))

(defmacro with-neighbors (neighbors &body body)
  `(with-slots (n ne e se s sw w nw) ,neighbors
     ,@body))

(defun walkable-neighbors (tile)
  (with-neighbors (get-neighbors tile #'identity)
    (remove-if (lambda (x) (not (walkablep x))) (list n s e w))))

(defun map-tiles (filter func effect &key (start '(1 1)) (end '(-1 -1)))
  (with-attrs (width height) :dungeon
    (loop :with map-affected-p
          :for x :from (first start) :below (+ width (first end))
          :do (loop :for y :from (second start) :below (+ height (second end))
                    :for tile = (tile x y)
                    :for neighbors = (get-neighbors tile func)
                    :when (funcall filter tile neighbors)
                      :do (let ((value (funcall effect tile neighbors)))
                            (setf map-affected-p (or map-affected-p value))))
          :finally (return map-affected-p))))

(defun collect-tiles (filter func &key (start '(1 1)) (end '(-1 -1)))
  (let ((tiles))
    (map-tiles
     filter
     func
     (lambda (tile neighbors) (push (list tile neighbors) tiles))
     :start start
     :end end)
    tiles))

(defun process-tiles (filter func processor)
  (loop :with tiles = (collect-tiles filter func)
        :while tiles
        :do (loop :with (tile neighbors) = (pop tiles)
                  :while (funcall filter tile neighbors)
                  :for new = (funcall processor tile neighbors)
                  :when new
                    :do (push new tiles))))

(defun floorp (tile &optional neighbors)
  (declare (ignore neighbors))
  (featuresp tile '(:floor)))

(defun wallp (tile &optional neighbors)
  (declare (ignore neighbors))
  (featuresp tile '(:wall)))

(defun add-feature (tile feature)
  (remove-feature tile :wall)
  (pushnew feature (map-features tile)))

(defun remove-feature (tile feature)
  (deletef (map-features tile) feature))

(defun featuresp (tile features)
  (some (lambda (x) (member x features)) (map-features tile)))

(defun carvablep (tile neighbors)
  (with-neighbors neighbors
    (every #'null (list (walkablep tile) n s e w nw ne se sw))))

(defun connectorp (tile neighbors)
  (with-neighbors neighbors
    (and (not (region-id tile))
         (or (and (not (eql n s)) n s)
             (and (not (eql e w)) e w)))))

(defun dead-end-p (tile neighbors)
  (with-neighbors neighbors
    (let ((dirs (remove-if #'identity (list n s e w))))
      (and (walkablep tile)
           (>= (length dirs) 3)))))

(defun erode-dead-end (tile neighbors)
  (make-wall tile neighbors)
  (with-slots (x y) tile
    (with-neighbors neighbors
      (let ((dirs (remove nil `(((,x ,(1- y)) . ,n)
                                ((,x ,(1+ y)) . ,s)
                                ((,(1+ x) ,y) . ,e)
                                ((,(1- x) ,y) . ,w))
                          :key #'cdr)))
        (when (= (length dirs) 1)
          (let* ((next-tile (apply #'tile (caar dirs)))
                 (next-neighbors (get-neighbors next-tile #'walkablep)))
            (when (dead-end-p next-tile next-neighbors)
              (list next-tile next-neighbors))))))))

(defun make-connector (tile neighbors)
  (with-neighbors neighbors
    (setf (adjacent-regions tile) (remove nil (list n s e w)))
    (dolist (region-id (adjacent-regions tile))
      (push tile (connectors (get-region region-id))))))

(defun make-wall (tile &optional neighbors)
  (declare (ignore neighbors))
  (with-slots (walkablep region-id map-features) tile
    (setf walkablep nil
          region-id nil
          map-features '(:wall)))
  tile)

(defun make-floor (tile &optional neighbors)
  (declare (ignore neighbors))
  (with-slots (walkablep) tile
    (setf walkablep t)
    (remove-feature tile :wall)
    (add-feature tile :floor)))

(defun junctionp (tile)
  (featuresp tile '(:junction)))

(defun adjacent-junction-p (tile)
  (with-slots (x y) tile
    (or (junctionp (tile x (1- y)))
        (junctionp (tile x (1+ y)))
        (junctionp (tile (1- x) y))
        (junctionp (tile (1+ x) y)))))

(defun make-junction (tile)
  (unless (adjacent-junction-p tile)
    (with-slots (region-id) tile
      (add-feature tile :junction)
      (make-floor tile)
      (setf region-id nil))))

(defun make-extra-junction (tile neighbors)
  (declare (ignore neighbors))
  (when (< (rng 'range-inc) (clamp (attr :mine :junction-rate) 0 1))
    (make-junction tile)))
