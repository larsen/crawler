(in-package :crawler)

(defclass tile ()
  ((x :reader x
      :initarg :x)
   (y :reader y
      :initarg :y)
   (terrain :accessor terrain
            :initarg :terrain)
   (region-id :accessor region-id
              :initarg :region-id)
   (visitedp :accessor visitedp
             :initform nil)
   (doorp :accessor doorp
          :initform nil)
   (connectorp :accessor connectorp
               :initform nil)))

(defun make-tile (x y &key (terrain :wall) region-id)
  (let ((tile (make-instance 'tile :x x :y y :terrain terrain :region-id region-id)))
    (when (eq (terrain tile) :room)
      (setf (visitedp tile) t))
    tile))

(defun on-tile-map (filter neighbor-func effect)
  (with-slots (w h data) *dungeon*
    (loop for x from 1 below (1- w)
          do (loop for y from 1 below (1- h)
                   for tile = (aref data x y)
                   for n = (funcall neighbor-func (aref data x (1- y)))
                   for ne = (funcall neighbor-func (aref data (1+ x) (1- y)))
                   for e = (funcall neighbor-func (aref data (1+ x) y))
                   for se = (funcall neighbor-func (aref data (1+ x) (1+ y)))
                   for s = (funcall neighbor-func (aref data x (1+ y)))
                   for sw = (funcall neighbor-func (aref data (1- x) (1+ y)))
                   for w = (funcall neighbor-func (aref data (1- x) y))
                   for nw = (funcall neighbor-func (aref data (1- x) (1- y)))
                   when (funcall filter tile n ne e se s sw w nw)
                     do (funcall effect tile n ne e se s sw w nw)))))

(defun create-connectors ()
  (on-tile-map #'possible-connector-p #'region-id #'add-connector))

(defun possible-connector-p (tile &optional n ne e se s sw w nw)
  (declare (ignore ne se sw nw))
  (and (not (region-id tile))
       (or (and (not (eql n s)) n s)
           (and (not (eql e w)) e w))))

(defun add-connector (tile &optional n ne e se s sw w nw)
  (declare (ignore ne se sw nw))
  (with-slots (connectorp) tile
    (setf connectorp (sort (remove nil (list n s e w)) #'<))
    (dolist (region-id connectorp)
      (push tile (connectors (get-region region-id))))))

(defun remove-dead-ends ()
  (loop while (dead-ends-p *dungeon*)
        do (setf (dead-ends-p *dungeon*) nil)
           (on-tile-map #'dead-end-p #'terrain #'make-wall)))

(defun dead-end-p (tile &optional n ne e se s sw w nw)
  (declare (ignore ne se sw nw))
  (let ((dirs (remove-if-not
               #'(lambda (x) (eq x :wall))
               (list n s e w))))
    (and (not (eq (terrain tile) :wall))
         (= (length dirs) 3))))

(defun make-wall (tile &optional n ne e se s sw w nw)
  (declare (ignore n ne e se s sw w nw))
  (setf (dead-ends-p *dungeon*) t)
  (setf (terrain tile) :wall)
  (setf (region-id tile) nil))

(defclass region ()
  ((id :reader id
       :initarg :id
       :initform nil)
   (mergedp :accessor mergedp
            :initform nil)
   (adjacent :accessor adjacent
             :initform nil)
   (connectors :accessor connectors
               :initform nil)
   (tiles :accessor tiles
          :initform nil)))

(defun get-region (id)
  (gethash id (regions *dungeon*)))

(defun random-connector (region-id)
  (with-slots (connectors) (get-region region-id)
    (elt connectors (random (length connectors)))))

(defun open-connector (region-id)
  (let ((connector (random-connector region-id)))
    (setf (terrain connector) :door)
    connector))

(defun get-connected-region (region-id connector)
  (first (remove region-id (connectorp connector))))

(defun draw-merged (region-id)
  (dolist (tile (tiles (get-region region-id)))
    (setf (terrain tile) :region)))

(defun remove-extra-connectors (region-id connected-id)
  (dolist (connector (connectors (get-region region-id)))
    (when (and (member region-id (connectorp connector))
               (member connected-id (connectorp connector)))
      (setf (connectorp connector) nil)
      (deletef (connectors (get-region region-id)) connector)
      (deletef (connectors (get-region connected-id)) connector))))

(defun move-connectors (from to)
  (let ((from-region (get-region from))
        (to-region (get-region to)))
    (dolist (connector (connectors from-region))
      (with-slots (connectorp) connector
        (setf connectorp (substitute to from connectorp))
        (push connector (connectors to-region))))))

(defun merge-region (region-id)
  (let ((merged (get-connected-region region-id (open-connector region-id))))
    (remove-extra-connectors region-id merged)
    (move-connectors merged region-id)
    (draw-merged merged)))

(defun merge-all (region-id)
  (draw-merged region-id)
  (loop with region = (get-region region-id)
        while (connectors region)
        do (merge-region region-id)))
