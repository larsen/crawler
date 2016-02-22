(in-package :crawler-examples)

(defsketch :cave (:title "Example Cave"
                  :width (* *tile-size* (width *dungeon*))
                  :height (* *tile-size* (height *dungeon*))
                  :debug :scancode-grave)
    ()
  (background (gray 0.2))
  (with-slots (width height) *dungeon*
    (dotimes (x width)
      (dotimes (y height)
        (draw-tile :mine x y)))))

(defmethod select-color ((type (eql :cave)) x y)
  (let ((tile (tile x y)))
    (cond
      ((walkablep tile)
       (gray 1)))))

(defmethod regenerate ((window :cave) &key width height)
  (apply #'make-dungeon :cave width height (attrs-plist :cave)))

(defmethod run ((type (eql :cave)) width height &key)
  (make-instance :cave))
