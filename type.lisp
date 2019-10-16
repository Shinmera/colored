#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(defstruct (color
            (:include fill)
            (:constructor %color (r g b &optional (a 1.0f0)))
            (:conc-name NIL)
            (:predicate NIL))
  (r 0.0f0 :type single-float)
  (g 0.0f0 :type single-float)
  (b 0.0f0 :type single-float)
  (a 1.0f0 :type single-float))

(defmethod print-object ((color color) stream)
  (format stream "~s" (list 'color (r color) (g color) (b color) (a color))))

(defmethod make-load-form ((color color) &optional env)
  (declare (ignore env))
  (list '%color (r color) (g color) (b color) (a color)))

(defun color (r g b &optional (a 1.0f0))
  (%color (float r 0f0) (float g 0f0) (float b 0f0) (float a 0f0)))

(define-compiler-macro color (r g b &optional (a 1) &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0f0))
               `(float ,arg 0f0))))
    `(%color ,(fold r) ,(fold g) ,(fold b) ,(fold a))))

;; FIXME: extra type for CMYK and conversion functions (?)
;; FIXME: also, CIE XYZ conversions and spaces
