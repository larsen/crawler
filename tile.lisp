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
   (attrs :accessor attrs
          :initform nil)))

(defstruct (neighbor-data
            (:conc-name nil))
  n s e w nw ne se sw)

(defun make-tile (x y &key walkablep region-id)
  (make-instance 'tile :x x :y y :walkablep walkablep :region-id region-id))

(defun tile (x y)
  (with-slots (width tile-map) *dungeon*
    (aref tile-map (+ x (* y width)))))

(defun (setf tile) (tile x y)
  (with-slots (width tile-map) *dungeon*
    (setf (aref tile-map (+ x (* y width))) tile)))

(defun on-tile-map (filter func effect &key (start '(0 0)) (end '(0 0)))
  (with-slots (width height tile-map) *dungeon*
    (flet ((neighbor-data (x y)
             (when (and (>= x 0)
                        (>= y 0)
                        (<= x (+ (1- width) (first end)))
                        (<= y (+ (1- height) (second end))))
               (funcall func (tile x y)))))
      (loop with map-affected-p
            for x from (first start) below (+ width (first end))
            do (loop for y from (second start) below (+ height (second end))
                     for tile = (tile x y)
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
                       do (let ((value (funcall effect tile neighbor-data)))
                            (setf map-affected-p (or map-affected-p value))))
            finally (return map-affected-p)))))

(defmacro converge (&body body)
  `(loop while ,@body))

(defun possible-connector-p (tile neighbors)
  (with-slots (n s e w) neighbors
    (and (not (region-id tile))
         (or (and (not (eql n s)) n s)
             (and (not (eql e w)) e w)))))

(defun add-connector (tile neighbors)
  (with-slots (n s e w) neighbors
    (with-slots (connectors) *dungeon*
      (setf (gethash tile connectors) (sort (remove nil (list n s e w)) #'<))
      (dolist (region-id (gethash tile connectors))
        (push tile (connectors (get-region region-id)))))))

(defun dead-end-p (tile neighbors)
  (with-slots (n s e w) neighbors
    (let ((dirs (remove-if #'identity (list n s e w))))
      (and (walkablep tile)
           (>= (length dirs) 3)))))

(defun make-wall (tile neighbors)
  (declare (ignore neighbors))
  (setf (walkablep tile) nil
        (region-id tile) nil)
  tile)
