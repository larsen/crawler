(defsystem #:crawler
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A dungeon exploration game."
  :depends-on (#:alexandria
               #:cl-variates)
  :serial t
  :components ((:file "package")
               (:file "generator")
               (:file "tile")
               (:file "dungeon")
               (:file "room")
               (:file "corridor")
               (:file "region")))
