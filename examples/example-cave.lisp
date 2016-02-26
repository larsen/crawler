(in-package :crawler-examples)

(defsketch :cave (:title "Example Cave"
                  :width (* *tile-size* (attr :dungeon :width))
                  :height (* *tile-size* (attr :dungeon :height))
                  :debug :scancode-grave)
    ()
  (render :cave))

(defmethod select-color ((type (eql :cave)) x y)
  (let ((tile (tile x y)))
    (cond
      ((walkablep tile)
       (gray 1)))))

(defmethod regenerate ((window :cave))
  (apply #'make-dungeon :cave (append (attrs-plist :cave)
                                      (attrs-plist :dungeon))))

(defmethod run ((type (eql :cave)) &key)
  (make-instance :cave))
