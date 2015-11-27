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
           #:windiness
           #:door-rate
           #:room-density
           #:seed
           #:attrs))

(in-package :crawler)

(setf *system-name* :crawler)
