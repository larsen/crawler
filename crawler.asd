(defsystem #:crawler
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A dungeon exploration game."
  :depends-on (#:ax.misc.fs
               #:ax.game
               #:sdl2kit
               #:sketch)
  :serial t
  :components ((:file "package")
               (:file "dungeon")
               (:file "dungeon-room")
               (:file "test-draw")
               (:file "game")
               (:file "window")))
