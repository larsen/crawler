(in-package :cl-user)

(defpackage #:crawler
  (:use #:cl
        #:alexandria
        #:cl-variates)
  (:export #:make-dungeon
           #:*dungeon*
           #:attr
           #:with-attrs
           #:attrs-plist
           #:tile
           #:tiles
           #:tile-size
           #:walkablep
           #:region-id
           #:map-features
           #:featuresp))

(in-package :crawler)
