;;;; terpsiscottean.asd

(asdf:defsystem #:terpsiscottean
  :serial t
  :description "A Scottish country dancing devising tool and simulator"
  :author "Tim Macdonald <tsmacdonald@gmail.com>"
  :license "MIT License"
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "terpsiscottean")))

