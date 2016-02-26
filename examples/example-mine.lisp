(in-package :crawler-examples)

(defsketch :mine (:title "Example Mine"
                  :width (* *tile-size* (attr :dungeon :width))
                  :height (* *tile-size* (attr :dungeon :height))
                  :debug :scancode-grave)
    ()
  (render :mine))

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

(defmethod regenerate ((window :mine))
  (apply #'make-dungeon :mine (append (attrs-plist :mine)
                                      (attrs-plist :dungeon))))

(defmethod run ((type (eql :mine)) &key)
  (make-instance :mine))
