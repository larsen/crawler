(in-package :crawler-examples)

(defvar *tile-size* 10)

(defmethod draw ()
  (background (gray 0.2))
  (with-attrs (width height) :dungeon
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile x y)))))

(defmethod draw-tile (x y)
  (with-pen (make-pen :fill (select-color x y))
    (rect (* x *tile-size*)
          (* y *tile-size*)
          (1- *tile-size*)
          (1- *tile-size*))))

(defmethod select-color (x y)
  (let ((tile (tile x y)))
    (cond
      ((featuresp tile '(:stairs-up))
       (rgb 1 0 0))
      ((featuresp tile '(:stairs-down))
       (rgb 0 1 0))
      ((featuresp tile '(:junction))
       (rgb 0.1 0.5 1))
      ((walkablep tile)
       (gray 1)))))

(defmethod mousebutton-event :after (window state ts button x y)
  (when (and (eq state :MOUSEBUTTONUP)
             (= button 1))
    (regenerate window)))

(defmethod run (type &rest attrs)
  (when (apply #'build type attrs)
    (make-instance type)))
