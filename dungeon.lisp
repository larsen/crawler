(in-package :crawler)

(defvar *dungeon* nil)

(defclass dungeon ()
  ((buffers :accessor buffers
            :initform (list 0))
   (tiles :accessor tiles
          :initarg :tiles)))

(defmethod create-walls :around (&key)
  (dolist (i (buffers *dungeon*))
    (call-next-method :buffer i)))

(defmethod create-walls (&key buffer)
  (with-attrs (width height) :dungeon
    (loop :for x :below width
          :do (loop :for y :below height
                    :do (make-tile x y buffer)))))

(defmethod build :around (type)
  (create-walls)
  (call-next-method))

(defun make-dungeon (type &rest attrs)
  (setf *dungeon* (make-instance (intern (string type) :crawler)))
  (load-data)
  (make-generator type attrs)
  (make-buffers type)
  (build type)
  *dungeon*)
