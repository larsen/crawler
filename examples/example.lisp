(in-package :crawler-examples)

(defvar *tile-size* 10)

(defmethod draw-tile (type x y)
  (with-pen (make-pen :fill (select-color type x y))
    (rect (* x *tile-size*)
          (* y *tile-size*)
          (1- *tile-size*)
          (1- *tile-size*))))

(defmethod mousebutton-event :after (window state ts button x y)
  (when (and (eq state :MOUSEBUTTONUP)
             (= button 1))
    (regenerate window)))

(defmethod regenerate :around (window &key)
  (with-slots (width height) *dungeon*
    (call-next-method window :width width :height height)))

(defmethod run :around (type width height &rest attrs)
  (when (apply #'make-dungeon type width height attrs)
    (call-next-method)))
