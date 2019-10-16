#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(defun convert-space (h c x m alpha)
  (cond ((<= h 0) (color m m m alpha))
        ((<= h 1) (color (+ m c) (+ m x) (+ m 0) alpha))
        ((<= h 2) (color (+ m x) (+ m c) (+ m 0) alpha))
        ((<= h 3) (color (+ m 0) (+ m c) (+ m x) alpha))
        ((<= h 4) (color (+ m 0) (+ m x) (+ m c) alpha))
        ((<= h 5) (color (+ m x) (+ m 0) (+ m c) alpha))
        ((<= h 6) (color (+ m c) (+ m 0) (+ m x) alpha))))

(defun rgb (red green blue &optional (alpha 1))
  (color red green blue alpha))

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
    (convert-space h c x m alpha)))

(defun hcl (hue chroma luma &optional (alpha 1))
  (let* ((h (/ hue 60))
         (x (* chroma (- 1 (abs (- (mod h 2) 1)))))
         (c (convert-space h chroma x 0 alpha))
         (m (- luma (+ (* .3 (r c)) (* .59 (g c)) (* .11 (b c))))))
    (color (+ m (r c)) (+ m (g c)) (+ m (b c)) alpha)))

(defun cmyk (cyan magenta yellow black &optional (alpha 1)))

;;; Calculate an RGB triplet from a colour temperature,
;;; according to real sunlight scales. This is based on
;;; http://www.zombieprototypes.com/?p=210
;;; This should be usable in the region 1'000K to 40'000K.
;;; Daylight occurs in the region 5'000K - 6'500K.
(defun temp (kelvin &optional (alpha 1))
  (let ((temp (coerce kelvin 'double-float)))
    (declare (type (double-float 1d0) temp))
    (declare (optimize speed))
    (flet ((e (a b c d)
             (declare (type double-float a b c d))
             (let ((x (- (/ temp 100) d)))
               (coerce (max 0d0 (min 1d0 (/ (+ a (* b x) (* c (the double-float (log x)))) 255)))
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


(defun to-rgb (color))

(defun to-hsv (color))

(defun to-hsl (color))

(defun to-hsi (color))

(defun to-cmyk (color))

(defun to-hcl (color))


(defun red (color)
  (r color))

(defun green (color)
  (g color))

(defun blue (color)
  (b color))

(defun alpha (color)
  (a color))

(defun hue (color))

(defun saturation (color))

(defun value (color))

(defun lightness (color))

(defun intensity (color))

(defun chroma (color))

(defun luma (color))

(defun cyan (color))

(defun magenta (color))

(defun yellow (color))

(defun black (color))

(defun temperature (color))
