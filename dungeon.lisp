(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((width :reader width
          :initarg :w)
   (height :reader height
           :initarg :h)
   (tile-map :accessor tile-map
             :initarg :tile-map)))

(defun create-walls ()
  "Fill the dungeon with all wall tiles."
  (with-slots (width height) *dungeon*
    (loop :for x :below width
          do (loop :for y :below height
                   :do (setf (tile x y) (make-instance 'tile :x x :y y))))))

(defmethod make-tile-map :around (type)
  (with-slots (width height tile-map) *dungeon*
    (call-next-method)
    (setf tile-map (make-array `(,width ,height) :initial-element (make-instance 'tile)))))

(defgeneric build (type))

(defun make-dungeon (type width height &rest attrs)
  (setf *dungeon* (make-instance (intern (string type) :crawler) :w width :h height))
  (load-data)
  (make-generator type attrs)
  (make-tile-map type)
  (build type)
  *dungeon*)
