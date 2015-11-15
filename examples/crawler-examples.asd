(defsystem #:crawler-examples
  :name "Crawler Examples"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "Examples for the Crawler game."
  :depends-on (#:crawler
               #:sdl2kit
               #:sketch)
  :serial t
  :components ((:file "package")
               (:file "random-dungeon")))
