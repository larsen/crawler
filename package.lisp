(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:alexandria
        #:cl-variates
        #:cl-heap)
  (:export #:make-dungeon
           #:*dungeon*
           #:width
           #:height
           #:tile
           #:tile-map
           #:tile-size
           #:walkablep
           #:region-id
           #:map-feature
           #:get-attrs))

(in-package :crawler)

(setf *system-name* :crawler)
