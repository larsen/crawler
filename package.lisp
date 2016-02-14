(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:alexandria
        #:cl-variates)
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
           #:map-features
           #:featuresp))

(in-package :crawler)
