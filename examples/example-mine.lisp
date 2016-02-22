(in-package :crawler-examples)

(defsketch :mine (:title "Example Mine"
                  :width (* *tile-size* (width *dungeon*))
                  :height (* *tile-size* (height *dungeon*))
                  :debug :scancode-grave)
    ()
  (background (gray 0.2))
  (with-slots (width height) *dungeon*
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile :mine x y)))))

(defmethod select-color ((type (eql :mine)) x y)
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

(defmethod regenerate ((window :mine) &key width height)
  (apply #'make-dungeon :mine width height (attrs-plist :mine)))

(defmethod run ((type (eql :mine)) width height &key)
  (make-instance :mine))
