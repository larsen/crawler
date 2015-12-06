(defsystem #:crawler
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A dungeon exploration game."
  :depends-on (#:alexandria
               #:cl-variates
               #:cl-heap)
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "generator")
               (:file "tile")
               (:file "room")
               (:file "corridor")
               (:file "region")
               (:file "stairs")
               (:file "dungeon")))
