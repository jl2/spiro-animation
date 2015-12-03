;;;; package.lisp
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(defpackage #:spiro-animation
  (:use #:cl)
  (:export
   #:make-spirograph
   #:spirograph-a
   #:spirograph-b
   #:spirograph-h
   #:spirograph-dt
   #:spirograph-dt-type

   #:epitrochoid-x
   #:epitrochoid-y
   #:hypotrochoid-x
   #:hypotrochoid-y
   #:draw-spirograph
   #:animate-spirograph

   #:from-mp3
   #:make-fixed-type2
   #:calculate-next-type2
))

