(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((buffers :accessor buffers
            :initform (list 0))
   (tiles :accessor tiles
          :initarg :tiles)))

(defun make-dungeon (type)
  (setf *dungeon* (make-instance (intern (string type) :crawler))))

(defmethod make-buffers (type))

(defmethod create-walls ()
  (with-attrs (width height) :dungeon
    (dolist (buffer (buffers *dungeon*))
      (dotimes (x width)
        (dotimes (y height)
          (make-tile x y buffer))))))

(defmethod build :around (type &rest attrs)
  (make-dungeon type)
  (load-data)
  (make-generator type attrs)
  (make-buffers type)
  (create-walls)
  (call-next-method)
  *dungeon*)
