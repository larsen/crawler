(defsystem #:crawler
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A dungeon exploration game."
  :depends-on (#:alexandria)
  :serial t
  :components ((:file "package")
               (:file "dungeon")
               (:file "tile")
               (:file "room")
               (:file "corridor")
               (:file "region")))
