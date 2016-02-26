(in-package :crawler-examples)

(defvar *tile-size* 10)

(defmethod render (type)
  (background (gray 0.2))
  (with-attrs (width height) :dungeon
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile type x y)))))

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

(defmethod run :around (type &rest attrs)
  (when (apply #'make-dungeon type attrs)
    (call-next-method)))
