(in-package :crawler)

(defclass cave (dungeon)
  ())

(defmethod make-tile-map ((type (eql :cave))))

(defmethod build ((type (eql :cave)))
  (create-walls))
