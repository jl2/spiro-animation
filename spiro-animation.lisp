;;;; spiro-animation.lisp
;;;;
;;;; Copyright (c) 2015 Jeremiah LaRocco <jeremiah.larocco@gmail.com>

(in-package #:spiro-animation)

(defun interpolate (a b cur-step steps &optional (looping nil))
  "Linearly interpolate between a and b over a number of steps.
   If looping is t, interpolates between a and b when cur-step is less than 
   steps/2, and between b and a when cur-step is greater than steps/2."
  (if (not looping)
      (let ((da (/ (- b a) steps)))
        (+ a (* da cur-step)))
      (if (< cur-step (/ steps 2))
          (let ((da (/ (- b a) (/ steps 2))))
            (+ a (* da cur-step)))
          (let ((da (/ (- a b) (/ steps 2))))
            (+ b (* da (- cur-step (/ steps 2)) ))))))
      

;; map-val is used to map logical coordinates to screen coordinates.
(defun map-val (x xmin xmax new-xmin new-xmax)
  "Map a value from the range xmin,xmax to the range new-xmin,new-xmax"
  (+ (* (/ (- x xmin) (- xmax xmin)) (- new-xmax new-xmin)) new-xmin))

;; Other interesting values:
;; a 11.50, b 4.0, h 12.0, steps 2400, dt 0.230
;; a 40.00, b 7.00, h 40.00, steps 3200, dt 0.181
;; a 59.00, b 4.27, h 35.22, steps 3200, dt 1.536
;; a 30.00, b 2.45, h 23.00, steps 2000, dt 1.300
;; a 30.00, b 5.00, h 39.00, steps 1230, dt=pi/dt, dt >= 30, animated
(defstruct spirograph
  (steps 1200 :type (unsigned-byte 32))
  (a 30.0 :type single-float)
  (b 2.0 :type single-float)
  (h 22.0 :type single-float)
  (dt 0.5 :type single-float)
  (dt-type :normal))

(defun interpolate-spirograph (a b cur-step steps &optional (looping nil))
  (make-spirograph :steps (spirograph-steps a)
                   :dt-type (spirograph-dt-type a)
                   :a (interpolate (spirograph-a a) (spirograph-a b)
                                   cur-step steps looping)
                   :b (interpolate (spirograph-b a) (spirograph-b b)
                                   cur-step steps looping)
                   :h (interpolate (spirograph-h a) (spirograph-h b)
                                   cur-step steps looping)
                   :dt (interpolate (spirograph-dt a) (spirograph-dt b)
                                    cur-step steps looping)))

(defun epitrochoid-x (spiro tv)
  "X component of the parametric equation for an epitrochoid curve."
  (- 
   (* (+ (spirograph-a spiro) (spirograph-b spiro)) (cos tv))
   (* (spirograph-h spiro)
      (cos (* tv
              (/ (+ (spirograph-a spiro) (spirograph-b spiro))
                 (spirograph-b spiro)))))))

(defun epitrochoid-y (spiro tv)
  "Y component of the parametric equation for an epitrochoid curve."
  (-
   (* (+ (spirograph-a spiro) (spirograph-b spiro)) (sin tv))
   (* (spirograph-h spiro)
      (sin (* tv
              (/ (+ (spirograph-a spiro) (spirograph-b spiro))
                 (spirograph-b spiro)))))))

(defun hypotrochoid-x (spiro tv)
  "X component of the parametric equation for an hypotrochoid curve."
  (+ 
   (* (- (spirograph-a spiro) (spirograph-b spiro)) (cos tv))
   (* (spirograph-h spiro)
      (cos (* tv 
              (/ (- (spirograph-a spiro) (spirograph-b spiro))
                 (spirograph-b spiro)))))))

(defun hypotrochoid-y (spiro tv)
  "Y component of the parametric equation for an hypotrochoid curve."
  (+
   (* (- (spirograph-a spiro) (spirograph-b spiro)) (sin tv))
   (* (spirograph-h spiro)
      (sin (* tv 
              (/ (- (spirograph-a spiro) (spirograph-b spiro))
                 (spirograph-b spiro)))))))

(defun inner-draw-spirograph (sp width height line-function
                              x-function y-function)
  "Draw the spirograph sp using the given functions.  line-function should 
   be a function taking four parameters (x1, y1, x2, y2)."

  ;; Set max-radius to an estimate on the maximum radius of the curve, given the
  ;; current values of a, b, and h.
  (let ((max-radius (* 1.1 (+ (spirograph-a sp)
                              (spirograph-b sp)
                              (spirograph-h sp))))

        ;; real-dt is the actual increase in tv each iteration
        (real-dt (if (eql (spirograph-dt-type sp) :normal)
                 (spirograph-dt sp)
                 (/ pi (spirograph-dt sp)))))
    
    ;; Define some local functions for convenience
    (flet (
           ;; xmapper maps logical x coordinates in the range x-min to x-max to
           ;; screen coordinates in the range 0 to width
           (xmapper (x) (map-val x (- max-radius) max-radius 0 width))

           ;; ymapper does the same thing, but for y coordinates
           (ymapper (y) (map-val y (- max-radius) max-radius 0 height))
           
           ;; spirograph-x and spirograph-y hide funcall and make the code
           ;; easier to read below
           (spirograph-x (tv) (funcall x-function sp tv))
           (spirograph-y (tv) (funcall y-function sp tv)))
      
        ;; Draw the curve
        (loop
           for i below (spirograph-steps sp)
           for cur-t = 0.0 then (* i real-dt)
           do
             (funcall
              line-function
              (truncate (xmapper (spirograph-x cur-t)))
              (truncate (ymapper (spirograph-y cur-t)))
              (truncate (xmapper (spirograph-x
                                  (+ (spirograph-dt sp) cur-t))))
              (truncate (ymapper (spirograph-y
                                  (+ (spirograph-dt sp) cur-t)))))))))


(defun cairo-draw-spirograph (file-name spiro width height
                              x-function y-function)
  "Use Cairo to draw a spirograph, saving in the specified file."
  (cl-cairo2:with-png-file (file-name :argb32 width height)

    (cl-cairo2:set-source-rgba 0.0 0.0 0.0 1.0)
    (cl-cairo2:paint)
    
    (cl-cairo2:scale 1 1)
    (cl-cairo2:set-line-width 0.5)
    (cl-cairo2:set-source-rgba 0.0 0.8 0.0 0.95)
    (flet ((cairo-line (x1 y1 x2 y2)
             (cl-cairo2:move-to x1 y1)
             (cl-cairo2:line-to x2 y2)))
      (inner-draw-spirograph spiro width height #'cairo-line
                             x-function y-function))
    (cl-cairo2:stroke)))

(defun animate-spirograph (&key
                             begin end
                             run-time
                             (fps 30)
                             (looping nil)
                             (verbose t)
                             (threads 4)
                             output-directory
                             (width 1200) (height 1200)
                             (x-function #'epitrochoid-x)
                             (y-function #'epitrochoid-y))
  "Generate a series of images containing an animation interpolating between
    begin and end spirographs."
  (let* ((real-dir-name (ensure-directories-exist
                         (if (char=  #\/ (aref output-directory 
                                               (- (length output-directory) 1)))
                             output-directory
                             (concatenate 'string output-directory "/"))))
         (total-frames (ceiling (* run-time fps)))
         (kernel (lparallel:make-kernel threads))
         (futures nil))

    (setf lparallel:*kernel* kernel)
    (unwind-protect
         (dotimes (cur-frame total-frames)
           (let ((file-name (format nil "~aframe~5,'0d.png" real-dir-name cur-frame))
                 (spiro (interpolate-spirograph begin end cur-frame total-frames looping)))
             (setf futures
                   (cons
                    (lparallel:future
                      (cairo-draw-spirograph file-name spiro width height x-function y-function))
                    futures))))
      (when futures (dolist (fut futures) (lparallel:force fut)))
      (when kernel (lparallel:end-kernel :wait t)))))


(defun draw-spirograph (&key
                          spiro
                          file-name
                          (width 1200) (height 1200)
                          (x-function #'epitrochoid-x)
                          (y-function #'epitrochoid-y))
  "Draw a spirograph to an image file."
  (cairo-draw-spirograph file-name spiro width height :x-function x-function :y-function y-function))
