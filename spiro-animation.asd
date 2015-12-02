;;;; spiro-animation.asd
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(asdf:defsystem #:spiro-animation
  :description "Describe spiro-animation here"
  :author "Jeremiah LaRocco <jeremiah.larocco@gmail.com>"
  :license "ISC (BSD-like)"
  :depends-on (#:cl-cairo2
               #:lparallel
               #:mpg123-ffi
               #:bordeaux-fft
               #:uiop
               #:cl-spark)
  :serial t
  :components ((:file "package")
               (:file "spiro-animation")))

