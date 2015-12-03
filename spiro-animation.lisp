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
                              color-function
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
            color-function
            0.0
            (+ 0.2 (/ (* i 0.6) (spirograph-steps sp)))
            0.0
            0.95)
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
             (cl-cairo2:line-to x2 y2)
             (cl-cairo2:stroke)))


      (inner-draw-spirograph spiro width height
                             #'cairo-line #'cl-cairo2:set-source-rgba
                             x-function y-function))))

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
           (let ((file-name (format nil
                                    "~aframe~5,'0d.png" real-dir-name cur-frame))
                 (spiro (interpolate-spirograph begin end
                                                cur-frame total-frames
                                                looping)))
             (setf futures
                   (cons
                    (lparallel:future (cairo-draw-spirograph file-name spiro
                                                             width height
                                                             x-function
                                                             y-function))
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
  (cairo-draw-spirograph file-name spiro
                         width height :x-function x-function :y-function y-function))


(defstruct mp3-file
  (left-channel)
  (right-channel)
  (samples)
  (sample-rate 44100 :type (unsigned-byte 32))
  (channels 2 :type (unsigned-byte 32))
  (mpg123-type 208 :type (unsigned-byte 32)))

(defun next-power-of-2 (num)
  (loop
     for power from 0
     for cn = num then (floor (/ cn 2))
     until (< cn 2)
     finally (return (ash 1 (+ 1 power)))))


(defun read-mp3-file (fname)
  "Read the specified mp3 file into an mp3-file structure."
  (multiple-value-bind
        (samples sample-rate channels mt)
      (mpg123:decode-mp3-file fname :character-encoding :utf-8)
    
    (let* ((samples-per-channel (/ (length samples) channels))
           (left-channel (make-array samples-per-channel
                                     :element-type '(complex double-float)
                                     :initial-element (coerce 0.0 '(complex double-float))))
           (right-channel (make-array samples-per-channel
                                     :element-type '(complex double-float)
                                     :initial-element (coerce 0.0 '(complex double-float)))))
      (loop for i below samples-per-channel
         do
           (let ((left-raw (/ (aref samples (* 2 i)) 32768.0))
                 (right-raw (/ (aref samples (+ 1 (* 2 i))) 32768.0)))
             
             (setf (aref left-channel i)
                   (coerce left-raw '(complex double-float)))
             (setf (aref right-channel i)
                   (coerce right-raw '(complex double-float)))))
      (make-mp3-file
       :samples samples
       :left-channel left-channel
       :right-channel right-channel
       :sample-rate sample-rate
       :channels channels
       :mpg123-type mt))))

(defun duration-in-seconds (mp3)
  "Compute the duration of an mp3-file in seconds."
  (/ (length (mp3-file-samples mp3)) 
     (* (mp3-file-channels mp3) (mp3-file-sample-rate mp3))))

(defun make-movie (directory mp3-name final-name tmp-name &optional (remove-tmp t))
  "Run ffmpeg to create a movie with audio."
  (if (probe-file tmp-name)
      (delete-file tmp-name))

  (let ((movie-command
         (format nil 
                 "ffmpeg -r 30 -i \"~aframe%05d.png\" -b 2400 -q 4 \"~a\""
                 directory tmp-name))
        (audio-command
         (format nil
                 "ffmpeg -i \"~a\" -i \"~a\" -codec copy -shortest \"~a\""
                 tmp-name mp3-name final-name)))
    
    (format t "~a~%" movie-command)
    (uiop:run-program movie-command)
    (if (probe-file final-name)
        (delete-file final-name))

    (format t "~a~%" audio-command)
    (uiop:run-program audio-command)
    (if remove-tmp
        (delete-file tmp-name))))

(defun fix-directory (directory-name)
  "Make sure directory exists and has a / at the end."
  (ensure-directories-exist
   (if (char=  #\/ (aref directory-name (- (length directory-name) 1)))
       directory-name
       (concatenate 'string directory-name "/"))))


(defun average-between (fft-data start end)
  (loop for idx from start below end
     summing (aref fft-data idx) into total
     finally (return (coerce (/ (abs total) (- end start)) 'single-float))))

(defun calculate-next-type2 (previous fft-data samples scale-factor)
  (coerce (+ previous (* scale-factor (abs (average-between fft-data (car samples) (cadr samples))))) 'single-float))

(defun make-fixed-type2 (samples scale-factor)
  (lambda (previous fft-data)
    (calculate-next-type2 previous fft-data samples scale-factor)))

(defun calculate-next-type1 (previous fft-data o-value samples scale-factor)
  (declare (ignore previous))
  (coerce (+ o-value (* scale-factor (abs (average-between fft-data (car samples) (cadr samples))))) 'single-float))

(defun make-fixed-type1 (o-value samples scale-factor)
  (lambda (previous fft-data)
    (calculate-next-type1 previous fft-data o-value samples scale-factor)))


;; Sampled call:
;; (time (spiro-animation:from-mp3 :mp3-file-name "/mnt/externalhd/PhotoBackup_backup/my_music/silence/encre/01 - Flocon.mp3"
;;                                    :output-directory "/home/jeremiah/spirographs/attempt36/"
;;                                    :width 800 :height 800
;;                                    :num-steps 420
;;                                    :a-base 39.0
;;                                    :b-base 7.0
;;                                    :h-base 32.0
;;                                    :dt-base 70.0
;;                                    :movie-duration 30
;;                                    :fft-window-size 1024
;;                                    :a-transform (spiro-animation:make-fixed-type2 '(1 10) 0.001)
;;                                    :b-transform (spiro-animation:make-fixed-type2 '(11 20) 0.001)
;;                                    :h-transform (spiro-animation:make-fixed-type2 '(21 30) 0.001)
                                   :dt-transform (spiro-animation:make-fixed-type2 '(22 23) 0.001)))
(defun from-mp3 (&key
                   mp3-file-name output-directory
                   (movie-file-name "spirograph_with_sound.mpg")
                   (keep-pngs nil)
                   (keep-soundless nil)

                   (width 800) (height 800)
                   (num-steps 240) (a-base 51.0) (b-base 7.0) (h-base 29.0)
                   (dt-base 70.0)
                   (fps 30)
                   (verbose t)
                   (threads 4)
                   (fft-window-size 1024)
                   (a-transform (make-fixed-type2 '(1 10) (/ 1 250.0)))
                   (b-transform (make-fixed-type2 '(11 20) (/ 1 250.0)))
                   (h-transform (make-fixed-type2 '(21 30) (/ 1 250.0)))
                   (dt-transform (make-fixed-type2 '(31 40) (/ 1 250.0)))
                   (movie-duration nil)
                   (tmp-movie-name "spirograph.mpg")
                   (x-function #'epitrochoid-x) (y-function #'epitrochoid-y))
  "Generate an animation from an MP3 file."
  
  (let* ((real-dir-name (fix-directory output-directory))

         (mp3-file (read-mp3-file mp3-file-name))

         (song-duration (duration-in-seconds mp3-file))
         (real-movie-duration (if movie-duration
                                  (min song-duration movie-duration)
                                  song-duration))
         
         (total-frames (ceiling (* real-movie-duration fps)))

         (files-created nil)
         (spiros nil)

         (full-movie-name (format nil "~a~a" real-dir-name movie-file-name))
         (full-tmp-movie-name (format nil "~a~a" real-dir-name tmp-movie-name))

         (spiro (make-spirograph :steps num-steps
                                 :a a-base
                                 :b b-base
                                 :h h-base
                                 :dt dt-base :dt-type :over-pi))
         (kernel (lparallel:make-kernel threads))
         (futures nil))
    
    (when verbose (format t "Creating animation with ~a frames." total-frames))

    (dotimes (cur-frame total-frames)
      (let* ((file-name (format nil
                                "~aframe~5,'0d.png" real-dir-name cur-frame))
        
             (win-center (ceiling (* 44100 (interpolate 0.0 song-duration
                                                        cur-frame total-frames))))
             (fft-data (bordeaux-fft:windowed-fft (mp3-file-left-channel mp3-file) win-center fft-window-size)))
        
        ;; (when verbose (format t "~a ~a~%" (mapcar #'abs (coerce fft-data 'list)) (spark:spark (mapcar #'abs (coerce fft-data 'list)))))

        (setf spiro (make-spirograph
                     :steps (spirograph-steps spiro)
                     :dt-type (spirograph-dt-type spiro)
                     :a (funcall a-transform (spirograph-a spiro)
                                 fft-data)
                     :b (funcall b-transform (spirograph-b spiro)
                                 fft-data)
                     :h (funcall h-transform (spirograph-h spiro)
                                 fft-data)
                     :dt (funcall dt-transform (spirograph-dt spiro)
                                  fft-data)))
        ;; (when verbose (format t "~a~%" spiro))
        (push (cons (copy-structure spiro) file-name) spiros)))

    (setf lparallel:*kernel* kernel)
    (unwind-protect

         (dolist (nspiro spiros)
           (push (lparallel:future
                   (cairo-draw-spirograph (cdr nspiro)
                                          (car nspiro)
                                          width height
                                          x-function y-function)
                   (push (cdr nspiro) files-created))
                 futures))
      (when futures (dolist (fut futures) (lparallel:force fut)))
      (when kernel (lparallel:end-kernel :wait t)))

    (make-movie real-dir-name mp3-file-name full-movie-name
                full-tmp-movie-name (not keep-soundless))
    (if (not keep-pngs)
        (dolist (fname files-created)
          (delete-file fname)))))

