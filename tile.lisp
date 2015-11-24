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

(defclass neighbor-data ()
  ((n :accessor n
      :initarg :n
      :initform nil)
   (s :accessor s
      :initarg :s
      :initform nil)
   (e :accessor e
      :initarg :e
      :initform nil)
   (w :accessor w
      :initarg :w
      :initform nil)
   (nw :accessor nw
       :initarg :nw
       :initform nil)
   (ne :accessor ne
       :initarg :ne
       :initform nil)
   (se :accessor se
       :initarg :se
       :initform nil)
   (sw :accessor sw
       :initarg :sw
       :initform nil)))

(defun make-tile (x y &key (terrain :wall) region-id)
  (let ((tile (make-instance 'tile :x x :y y :terrain terrain :region-id region-id)))
    (when (eq (terrain tile) :room)
      (setf (visitedp tile) t))
    tile))

(defun on-tile-map (filter func effect)
  (with-slots (width height data) *dungeon*
    (flet ((neighbor-data (x y)
             (funcall func (aref data x y))))
      (loop for x from 1 below (1- width)
            do (loop for y from 1 below (1- height)
                     for tile = (aref data x y)
                     for neighbor-data = (make-instance 'neighbor-data
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
    (with-slots (connectorp) tile
      (setf connectorp (sort (remove nil (list n s e w)) #'<))
      (dolist (region-id connectorp)
        (push tile (connectors (get-region region-id)))))))

(defun dead-end-p (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (let ((dirs (remove-if-not
                 #'(lambda (x) (eq x :wall))
                 (list n s e w))))
      (and (not (eq (terrain tile) :wall))
           (= (length dirs) 3)))))

(defun make-wall (tile neighbors)
  (with-slots (n s e w nw ne se sw) neighbors
    (setf (dead-ends-p *dungeon*) t
          (terrain tile) :wall
          (region-id tile) nil)))
