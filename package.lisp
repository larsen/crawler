(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:alexandria
        #:cl-variates)
  (:export #:make-dungeon
           #:*dungeon*
           #:width
           #:height
           #:tile-map
           #:tile-size
           #:walkablep
           #:region-id
           #:*generator*
           #:windiness
           #:door-rate
           #:room-size-min
           #:room-size-max
           #:room-density
           #:seed
           #:attrs))

(in-package :crawler)

(setf *system-name* :crawler)
