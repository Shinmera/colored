#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.colored.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:colored #:org.shirakumo.alloy.colored)
   (#:colors #:org.shirakumo.alloy.colored.colors))
  (:export #:colored))

(in-package #:org.shirakumo.alloy.colored.test)

(defun ~= (a b)
  (< (abs (- b a)) 0.0001))

(defun color~= (a b)
  (loop for channel in (colored:channels a)
        always (~= (funcall channel a)
                   (funcall channel b))))

(define-test colored)

(define-test type
  :parent colored
  (finish (colored:color 0 0 0))
  (finish (colored:color 0 0 0 1))
  (finish (colored:color 2 2 2 2))
  (of-type colored:color (colored:color 0 0 0))
  (is = 1.0 (colored:r (colored:color 1 2 3)))
  (is = 2.0 (colored:g (colored:color 1 2 3)))
  (is = 3.0 (colored:b (colored:color 1 2 3)))
  (is = 1.0 (colored:a (colored:color 1 2 3)))
  (is = 0.0 (colored:a (colored:color 0 0 0 0)))
  (is colored:color= (colored:color 1 0 0) colors:red)
  (is colored:color= (colored:color 0 1 0) colors:green)
  (isnt colored:color= (colored:color 0 0 0 0) (colored:color 0 0 0 1))
  (is colored:color-equal (colored:color 0 0 0 0) (colored:color 0 0 0 1))
  (let ((a 1))
    (true (colored:color= (colored:color 1 0 0) colors:red (colored:color a 0 0)))
    (false (colored:color= (colored:color 0 0 0) colors:red (colored:color a 0 0)))))

(define-test integer
  :parent colored
  :depends-on (type)
  (is = #xFFFF (colored:encode-color colors:red 16 '(r)))
  (is = #x0000 (colored:encode-color colors:red 16 '(g)))
  (is = #x00FF (colored:encode-color colors:red 8 '(g r)))
  (is colored:color= colors:black (colored:decode-color 0 8 '(r g b)))
  (is colored:color= colors:black (colored:decode-color 0 8 '(r)))
  (is colored:color= colors:black (colored:decode-color 0 8 ()))
  (is colored:color= colors:red (colored:decode-color #xFF 8 '(r)))
  (is colored:color= colors:red (colored:decode-color #xFFFF 16 '(r)))
  (is colored:color= colors:red (colored:decode-color #xFF00 8 '(r g))))

(define-test mapping
  :parent colored
  :depends-on (type))

(defun round-trip (color type)
  (colored:convert (colored:convert color type) (type-of color)))

(define-test hsv
  :parent mapping
  (is = 0 (colored:hue colors:red))
  (is = 120 (colored:hue colors:green))
  (is = 240 (colored:hue colors:blue))
  (is = 0 (colored:hue colors:white))
  (is = 0 (colored:hue colors:black))
  (is = 1 (colored:saturation colors:red))
  (is = 0 (colored:saturation colors:white))
  (is = 0 (colored:saturation colors:black))
  (is = 0 (colored:value colors:red))
  (is = 0 (colored:value colors:white))
  (is = 0 (colored:value colors:black))
  (is colored:color= colors:red (round-trip colors:red 'colored:hsv))
  (loop repeat 10
        for color = (colored:rgb (random 1) (random 1) (random 1))
        do (is color~= color (round-trip color 'colored:hsv))))

(define-test hsl
  :parent mapping
  (is = 0.5 (colored:lightness colors:red))
  (is = 0.5 (colored:lightness colors:green))
  (is = 1 (colored:lightness colors:white))
  (is = 0 (colored:lightness colors:black))
  (is colored:color= colors:red (round-trip colors:red 'colored:hsl))
  (loop repeat 10
        for color = (colored:rgb (random 1) (random 1) (random 1))
        do (is color~= color (round-trip color 'colored:hsl))))

(define-test hsi
  :parent mapping
  (is ~= 1/3 (colored:intensity colors:red))
  (is ~= 1/3 (colored:intensity colors:green))
  (is = 1 (colored:intensity colors:white))
  (is = 0 (colored:intensity colors:black))
  (is colored:color= colors:red (round-trip colors:red 'colored:hsi))
  (loop repeat 10
        for color = (colored:rgb (random 1) (random 1) (random 1))
        do (is color~= color (round-trip color 'colored:hsi))))
