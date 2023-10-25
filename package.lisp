(defpackage #:org.shirakumo.alloy.colored
  (:use #:cl)
  ;; type.lisp
  (:export
   #:convert
   #:channels
   #:color
   #:a
   #:rgb
   #:srgb
   #:r
   #:g
   #:b
   #:hsv
   #:h
   #:s
   #:v
   #:hsl
   #:l
   #:hsi
   #:i
   #:cmyk
   #:c
   #:m
   #:y
   #:k
   #:xyz
   #:x*
   #:y*
   #:z*
   #:lab
   #:l*
   #:a*
   #:b*
   #:oklab
   #:color=
   #:color-equal)
  ;; ops.lisp
  (:export
   #:encode
   #:decode
   #:temperature-color
   #:red
   #:green
   #:blue
   #:alpha
   #:hue
   #:saturation
   #:value
   #:lightness
   #:intensity
   #:cyan
   #:magenta
   #:yellow
   #:black
   #:lerp
   #:gradient
   #:c*
   #:c/
   #:c+
   #:c-)
  ;; constants.lisp
  (:export
   #:define-color
   #:list-colors)
  ;; conversion-matrices.lisp
  (:export
   #:conversion-matrix
   #:define-conversion)
  ;; whitepoints.lisp
  (:export
   #:whitepoint
   #:define-whitepoint
   #:compute-d-whitepoint))

(unless (find-package '#:org.shirakumo.alloy.colored.colors)
  (make-package '#:org.shirakumo.alloy.colored.colors :use ()))
