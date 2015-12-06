(defsystem #:crawler-examples
  :name "Crawler Examples"
  :author "Michael Fiano <michael.fiano@gmail.com>"
  :version "0.1"
  :license "MIT"
  :description "Crawler dungeon generator examples."
  :depends-on (#:crawler
               #:sketch)
  :serial t
  :pathname "examples"
  :components ((:file "package")
               (:file "random-dungeon")))
