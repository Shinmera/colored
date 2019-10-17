#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(defmacro decode-color (integer channel-size bytes)
  (let ((int-size (* channel-size (length bytes))))
    `(color ,@(loop for channel in '(r g b a)
                    for pos = (position channel bytes)
                    collect (cond (pos
                                   `(/ (ldb (byte ,channel-size ,(- int-size (* channel-size (1+ pos)))) ,integer)
                                       255f0))
                                  ((eq channel 'a) 1f0)
                                  (T 0f0))))))

(defmacro encode-color (color channel-size bytes)
  (let ((int-size (* channel-size (length bytes))))
    `(+ ,@(loop for channel in '(r g b a)
                for pos = (position channel bytes)
                when pos
                collect `(ash (max 0 (min 255 (floor (* 255 (,channel ,color)))))
                              ,(- int-size (* channel-size (1+ pos))))))))

(defun rgb (integer) (decode-color integer 8 (r g b)))
(defun bgr (integer) (decode-color integer 8 (b g r)))
(defun argb (integer) (decode-color integer 8 (a r g b)))
(defun rgba (integer) (decode-color integer 8 (r g b a)))
(defun bgra (integer) (decode-color integer 8 (b g r a)))
(defun abgr (integer) (decode-color integer 8 (a b g r)))
(defun to-rgb (color) (encode-color color 8 (r g b)))
(defun to-bgr (color) (encode-color color 8 (b g r)))
(defun to-argb (color) (encode-color color 8 (a r g b)))
(defun to-rgba (color) (encode-color color 8 (r g b a)))
(defun to-bgra (color) (encode-color color 8 (b g r a)))
(defun to-abgr (color) (encode-color color 8 (a b g r)))

(defun convert-hue-space (h c x m alpha)
  (cond ((<= h 0) (color m m m alpha))
        ((<= h 1) (color (+ m c) (+ m x) (+ m 0) alpha))
        ((<= h 2) (color (+ m x) (+ m c) (+ m 0) alpha))
        ((<= h 3) (color (+ m 0) (+ m c) (+ m x) alpha))
        ((<= h 4) (color (+ m 0) (+ m x) (+ m c) alpha))
        ((<= h 5) (color (+ m x) (+ m 0) (+ m c) alpha))
        (T        (color (+ m c) (+ m 0) (+ m x) alpha))))

(defun hsv (hue saturation value &optional (alpha 1))
  (flet ((f (n)
           (let ((k (mod (+ n (/ hue 60)) 6)))
             (- value (* value saturation (max 0 (min 1 k (- 4 k))))))))
    (color (f 5) (f 3) (f 1) alpha)))

(defun hsl (hue saturation lightness &optional (alpha 1))
  (let ((a (* saturation (min lightness (- 1 lightness)))))
    (flet ((f (n)
             (let ((k (mod (+ n (/ hue 30)) 12)))
               (- lightness (* a (max -1 (min 1 (- k 3) (- k 9))))))))
      (color (f 0) (f 8) (f 4) alpha))))

(defun hsi (hue saturation intensity &optional (alpha 1))
  (let* ((h (/ hue 60))
         (z (- 1 (abs (1- (mod h 2)))))
         (c (/ (* 3 intensity saturation) (1+ z)))
         (x (* c z))
         (m (* intensity (- 1 saturation))))
    (convert-hue-space h c x m alpha)))

(defun hcl (hue chroma luma &optional (alpha 1))
  (let* ((h (/ hue 60))
         (x (* chroma (- 1 (abs (- (mod h 2) 1)))))
         (c (convert-hue-space h chroma x 0 alpha))
         (m (- luma (+ (* .3 (r c)) (* .59 (g c)) (* .11 (b c))))))
    (color (+ m (r c)) (+ m (g c)) (+ m (b c)) alpha)))

;;; Calculate an RGB triplet from a colour temperature,
;;; according to real sunlight scales. This is based on
;;; http://www.zombieprototypes.com/?p=210
;;; This should be usable in the region 0K to 40'000K.
;;; Colours in the region 0K - 1'000K are just linearly
;;; scaled from 1'000K towards black.
;;; Daylight occurs in the region 5'000K - 6'500K.
(defun temperature-color (kelvin &optional (alpha 1))
  (let* ((kelvin (coerce kelvin 'double-float))
         (temp (max 1000d0 kelvin)))
    (declare (type (double-float 1d0) temp))
    (declare (optimize speed))
    (flet ((e (a b c d)
             (declare (type double-float a b c d))
             (let ((x (- (/ temp 100d0) d)))
               (coerce (* (max 0d0 (min 1d0 (/ (+ a (* b x) (* c (the double-float (log x)))) 255)))
                          (/ (min kelvin 1000d0) 1000d0))
                       'single-float))))
      (color (e 351.97690566805693d0
                0.114206453784165d0
                -40.25366309332127d0
                55d0)
             (if (< temp 6600)
                 (e -155.25485562709179d0
                    -0.44596950469579133d0
                    104.49216199393888d0
                    2d0)
                 (e 325.4494125711974d0
                    0.07943456536662342d0
                    0.07943456536662342d0
                    50d0))
             (e
              -254.76935184120902d0
              -254.76935184120902d0
              115.67994401066147d0
              10d0)
             alpha))))

(defun %hue (color max min)
  (flet ((f (x y)
           (let ((h (* 60 (+ x (/ y (- max min))))))
             (if (<= 0 h) 0 (+ h 360)))))
    (cond ((= (r color) (g color) (b color)) 0)
          ((= max (r color)) (f 0 (- (g color) (b color))))
          ((= max (g color)) (f 2 (- (b color) (r color))))
          (T                 (f 4 (- (r color) (g color)))))))

(defun to-hsv (color)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (list (%hue color max min)
          (if (= 0 max) 0 (/ (- max min) max))
          (max (r color) (g color) (b color)))))

(defun to-hsl (color)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (list (%hue color max min)
          (cond ((= 0 max) 0)
                ((= 1 min) 0)
                (T         (/ (- max min) (- 1 (abs (1- (+ max min)))))))
          (/ (+ max min) 2))))

(defun to-hsi (color)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (list (%hue color max min)
          (- 1 (* 3 min))
          (/ (+ (r color) (g color) (b color)) 3))))

(defun to-hcl (color)
  (let* ((max (max (r color) (g color) (b color)))
         (min (min (r color) (g color) (b color)))
         (a (/ min max 100))
         (g 3)
         (q (exp (* a g))))
    (list (%hue color max min)
          (/ (* q (+ (abs (- (r color) (g color)))
                     (abs (- (g color) (b color)))
                     (abs (- (b color) (r color)))))
             3)
          (/ (+ (* q max) (* (- 1 q) min)) 2))))

(defun red (color)
  (r color))

(defun green (color)
  (g color))

(defun blue (color)
  (b color))

(defun alpha (color)
  (a color))

(defun hue (color)
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (flet ((f (x)
             (let ((h (* 60 (+ x (/ y (- max min))))))
               (if (<= 0 h) 0 (+ h 360)))))
      (cond ((= (r color) (g color) (b color)) 0)
            ((= max (r color)) (f 0 (- (g color) (b color))))
            ((= max (g color)) (f 2 (- (b color) (r color))))
            (T                 (f 4 (- (r color) (g color))))))))

(defun saturation (color)
  ;; Note that this is HSV saturation.
  (let ((max (max (r color) (g color) (b color)))
        (min (min (r color) (g color) (b color))))
    (if (= 0 max) 0 (/ (- max min) max))))

(defun value (color)
  (max (r color) (g color) (b color)))

(defun lightness (color)
  (/ (+ (max (r color) (g color) (b color))
        (min (r color) (g color) (b color)))
     2))

(defun intensity (color)
  (/ (+ (r color) (g color) (b color)) 3))

(defun chroma (color)
  (let* ((max (max (r color) (g color) (b color)))
         (min (min (r color) (g color) (b color)))
         (a (/ min max 100))
         (g 3)
         (q (exp (* a g))))
    (/ (* q (+ (abs (- (r color) (g color)))
               (abs (- (g color) (b color)))
               (abs (- (b color) (r color)))))
       3)))

(defun luma (color)
  (let* ((max (max (r color) (g color) (b color)))
         (min (min (r color) (g color) (b color)))
         (a (/ min max 100))
         (g 3)
         (q (exp (* a g))))
    (/ (+ (* q max) (* (- 1 q) min)) 2)))
