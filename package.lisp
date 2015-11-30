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
           #:seed
           #:attr
           #:windiness
           #:room-density
           #:room-size-min
           #:room-size-max
           #:door-rate
           #:set-attrs
           #:get-attrs))

(in-package :crawler)

(setf *system-name* :crawler)
