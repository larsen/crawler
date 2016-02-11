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
           #:attrs-plist
           #:tile
           #:tile-map
           #:tile-size
           #:walkablep
           #:region-id
           #:map-feature))

(in-package :crawler)
