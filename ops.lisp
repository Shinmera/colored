#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(defun rgb (red green blue &optional (alpha 1))
  (color red green blue alpha))

(defun hsv (hue saturation value &optional (alpha 1)))

(defun hsl (hue saturation lightness &optional (alpha 1)))

(defun hsi (hue saturation intensity &optional (alpha 1)))

(defun hslu (hue saturation luma &optional (alpha 1)))

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

(defun to-hslu (color))


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

(defun luma (color))

(defun cyan (color))

(defun magenta (color))

(defun yellow (color))

(defun black (color))

(defun temperature (color))
