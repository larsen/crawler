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
              :initarg :region-id)
   (attrs :accessor attrs
          :initform nil)))

(defstruct (neighbor-data
            (:conc-name nil))
  n s e w nw ne se sw)

(defun make-tile (x y &key walkablep region-id)
  (make-instance 'tile :x x :y y :walkablep walkablep :region-id region-id))

(defun on-tile-map (filter func effect)
  (with-slots (width height tile-map) *dungeon*
    (flet ((neighbor-data (x y)
             (funcall func (aref tile-map x y))))
      (loop for x from 1 below (1- width)
            do (loop for y from 1 below (1- height)
                     for tile = (aref tile-map x y)
                     for neighbor-data = (make-neighbor-data
                                          :n (neighbor-data x (1- y))
                                          :s (neighbor-data x (1+ y))
                                          :e (neighbor-data (1+ x) y)
                                          :w (neighbor-data (1- x) y)
                                          :nw (neighbor-data (1- x) (1- y))
                                          :ne (neighbor-data (1+ x) (1- y))
                                          :se (neighbor-data (1+ x) (1+ y))
                                          :sw (neighbor-data (1- x) (1+ y)))
                     when (funcall filter tile neighbor-data)
                       do (funcall effect tile neighbor-data))))))

(defun possible-connector-p (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (and (not (region-id tile))
         (or (and (not (eql n s)) n s)
             (and (not (eql e w)) e w)))))

(defun add-connector (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (setf (gethash tile (connectors *dungeon*)) (sort (remove nil (list n s e w)) #'<))
    (dolist (region-id (gethash tile (connectors *dungeon*)))
      (push tile (connectors (get-region region-id))))))

(defun dead-end-p (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (let ((dirs (remove-if #'identity (list n s e w))))
      (and (walkablep tile)
           (>= (length dirs) 3)))))

(defun make-wall (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (setf (dead-ends-p *dungeon*) t
          (walkablep tile) nil
          (region-id tile) nil)))
