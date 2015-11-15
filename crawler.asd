(defsystem #:crawler
  :name "Crawler"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "A dungeon exploration game."
  :depends-on (#:ax.misc.fs
               #:alexandria
               #:ax.game
               #:sdl2kit)
  :serial t
  :components ((:file "package")
               (:file "dungeon")
               (:file "dungeon-tile")
               (:file "dungeon-room")
               (:file "dungeon-corridor")
               (:file "game")
               (:file "window")))
