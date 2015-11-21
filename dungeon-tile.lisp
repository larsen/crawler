(in-package :crawler)

(defclass tile ()
  ((x :reader x
      :initarg :x)
   (y :reader y
      :initarg :y)
   (terrain :accessor terrain
            :initarg :terrain)
   (region :accessor region
           :initarg :region)
   (visitedp :accessor visitedp
             :initform nil)
   (doorp :accessor doorp
          :initform nil)
   (connectorp :accessor connectorp
               :initform nil)))

(defun make-tile (x y &key (terrain :wall) region)
  (let ((tile (make-instance 'tile :x x :y y :terrain terrain :region region)))
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
  (let ((connected))
    (with-slots (connectors) *dungeon*
      (on-tile-map #'possible-connector-p #'region #'add-connector)
      #++(dolist (connector (hash-table-keys connectors))
        (let ((tiles (shuffle (gethash connector connectors))))
          (unless (and (member (first connector) connected)
                       (member (second connector) connected))
            (appendf connected connector)
            (setf (terrain (first tiles)) :door)
            ))))))

(defun possible-connector-p (tile &optional n ne e se s sw w nw)
  (declare (ignore ne se sw nw))
  (and (not (region tile))
       (or (and (not (eql n s)) n s)
           (and (not (eql e w)) e w))))

(defun add-connector (tile &optional n ne e se s sw w nw)
  (declare (ignore ne se sw nw))
  (with-slots (connectorp region) tile
    (setf connectorp (sort (remove nil (list n s e w)) #'<))
    #++(push tile (gethash connectorp (connectors *dungeon*)))))

(defun remove-dead-ends ()
  (on-tile-map #'dead-end-p #'terrain #'make-wall))

(defun dead-end-p (tile &optional n ne e se s sw w nw)
  (declare (ignore ne se sw nw))
  (let ((dirs (remove-if-not
               #'(lambda (x) (eq x :wall))
               (list n s e w))))
    (and (eq (terrain tile) :corridor)
         (= (length dirs) 3))))

(defun make-wall (tile &optional n ne e se s sw w nw)
  (declare (ignore n ne e se s sw w nw))
  (setf (terrain tile) :wall))
