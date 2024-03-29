(in-package #:org.shirakumo.alloy.colored)

(defmacro define-color (name int &optional (channels '(r g b)))
  (let* ((std (find-package '#:org.shirakumo.alloy.colored.colors))
         (name (if (symbol-package name)
                   name
                   (intern (string name) std))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(when (eq std (symbol-package name))
           `((export ',name ,(symbol-package name))))
       (unless (boundp ',name)
         (defconstant ,name (decode ,int :channels ',channels))))))

(defun list-colors ()
  (loop for symbol being the external-symbols of '#:org.shirakumo.alloy.colored.colors
        when (boundp symbol)
        collect (cons (symbol-name symbol) (symbol-value symbol))))

(define-color #:transparent #x00000000 (r g b a))
(define-color #:black #x000000)
(define-color #:white #xFFFFFF)
(define-color #:red #xFF0000)
(define-color #:green #x00FF00)
(define-color #:blue #x0000FF)
(define-color #:yellow #xFFFF00)
(define-color #:silver #xC0C0C0)
(define-color #:gray #x808080)
(define-color #:maroon #x800000)
(define-color #:olive #x808000)
(define-color #:lime #x008000)
(define-color #:purple #x800080)
(define-color #:teal #x008080)
(define-color #:navy #x000080)
(define-color #:dark-red #x8B0000)
(define-color #:brown #xA52A2A)
(define-color #:firebrick #xB22222)
(define-color #:crimson #xDC143C)
(define-color #:tomato #xFF6347)
(define-color #:coral #xFF7F50)
(define-color #:indian-red #xCD5C5C)
(define-color #:light-coral #xF08080)
(define-color #:dark-salmon #xE9967A)
(define-color #:salmon #xFA8072)
(define-color #:light-salmon #xFFA07A)
(define-color #:orange-red #xFF4500)
(define-color #:dark-orange #xFF8C00)
(define-color #:orange #xFFA500)
(define-color #:gold #xFFD700)
(define-color #:dark-golden-rod #xB8860B)
(define-color #:golden-rod #xDAA520)
(define-color #:pale-golden-rod #xEEE8AA)
(define-color #:dark-khaki #xBDB76B)
(define-color #:khaki #xF0E68C)
(define-color #:yellow-green #x9ACD32)
(define-color #:dark-olive-green #x556B2F)
(define-color #:olive-drab #x6B8E23)
(define-color #:lawn-green #x7CFC00)
(define-color #:chart-reuse #x7FFF00)
(define-color #:green-yellow #xADFF2F)
(define-color #:dark-green #x006400)
(define-color #:forest-green #x228B22)
(define-color #:lime-green #x32CD32)
(define-color #:light-green #x90EE90)
(define-color #:pale-green #x98FB98)
(define-color #:dark-sea-green #x8FBC8F)
(define-color #:medium-spring-green #x00FA9A)
(define-color #:spring-green #x00FF7F)
(define-color #:sea-green #x2E8B57)
(define-color #:medium-aqua-marine #x66CDAA)
(define-color #:medium-sea-green #x3CB371)
(define-color #:light-sea-green #x20B2AA)
(define-color #:dark-slate-gray #x2F4F4F)
(define-color #:dark-cyan #x008B8B)
(define-color #:aqua #x00FFFF)
(define-color #:cyan #x00FFFF)
(define-color #:light-cyan #xE0FFFF)
(define-color #:dark-turquoise #x00CED1)
(define-color #:turquoise #x40E0D0)
(define-color #:medium-turquoise #x48D1CC)
(define-color #:pale-turquoise #xAFEEEE)
(define-color #:aqua-marine #x7FFFD4)
(define-color #:powder-blue #xB0E0E6)
(define-color #:cadet-blue #x5F9EA0)
(define-color #:steel-blue #x4682B4)
(define-color #:corn-flower-blue #x6495ED)
(define-color #:deep-sky-blue #x00BFFF)
(define-color #:dodger-blue #x1E90FF)
(define-color #:light-blue #xADD8E6)
(define-color #:sky-blue #x87CEEB)
(define-color #:light-sky-blue #x87CEFA)
(define-color #:midnight-blue #x191970)
(define-color #:dark-blue #x00008B)
(define-color #:medium-blue #x0000CD)
(define-color #:royal-blue #x4169E1)
(define-color #:blue-violet #x8A2BE2)
(define-color #:indigo #x4B0082)
(define-color #:dark-slate-blue #x483D8B)
(define-color #:slate-blue #x6A5ACD)
(define-color #:medium-slate-blue #x7B68EE)
(define-color #:medium-purple #x9370DB)
(define-color #:dark-magenta #x8B008B)
(define-color #:dark-violet #x9400D3)
(define-color #:dark-orchid #x9932CC)
(define-color #:medium-orchid #xBA55D3)
(define-color #:thistle #xD8BFD8)
(define-color #:plum #xDDA0DD)
(define-color #:violet #xEE82EE)
(define-color #:magenta #xFF00FF)
(define-color #:orchid #xDA70D6)
(define-color #:medium-violet-red #xC71585)
(define-color #:pale-violet-red #xDB7093)
(define-color #:deep-pink #xFF1493)
(define-color #:hot-pink #xFF69B4)
(define-color #:light-pink #xFFB6C1)
(define-color #:pink #xFFC0CB)
(define-color #:antique-white #xFAEBD7)
(define-color #:beige #xF5F5DC)
(define-color #:bisque #xFFE4C4)
(define-color #:blanched-almond #xFFEBCD)
(define-color #:wheat #xF5DEB3)
(define-color #:corn-silk #xFFF8DC)
(define-color #:lemon-chiffon #xFFFACD)
(define-color #:light-golden-rod-yellow #xFAFAD2)
(define-color #:light-yellow #xFFFFE0)
(define-color #:saddle-brown #x8B4513)
(define-color #:sienna #xA0522D)
(define-color #:chocolate #xD2691E)
(define-color #:peru #xCD853F)
(define-color #:sandy-brown #xF4A460)
(define-color #:burly-wood #xDEB887)
(define-color #:tan #xD2B48C)
(define-color #:rosy-brown #xBC8F8F)
(define-color #:moccasin #xFFE4B5)
(define-color #:navajo-white #xFFDEAD)
(define-color #:peach-puff #xFFDAB9)
(define-color #:misty-rose #xFFE4E1)
(define-color #:lavender-blush #xFFF0F5)
(define-color #:linen #xFAF0E6)
(define-color #:old-lace #xFDF5E6)
(define-color #:papaya-whip #xFFEFD5)
(define-color #:sea-shell #xFFF5EE)
(define-color #:mint-cream #xF5FFFA)
(define-color #:slate-gray #x708090)
(define-color #:light-slate-gray #x778899)
(define-color #:light-steel-blue #xB0C4DE)
(define-color #:lavender #xE6E6FA)
(define-color #:floral-white #xFFFAF0)
(define-color #:alice-blue #xF0F8FF)
(define-color #:ghost-white #xF8F8FF)
(define-color #:honeydew #xF0FFF0)
(define-color #:ivory #xFFFFF0)
(define-color #:azure #xF0FFFF)
(define-color #:snow #xFFFAFA)
(define-color #:dim-gray #x696969)
(define-color #:dark-gray #xA9A9A9)
(define-color #:light-gray #xD3D3D3)
(define-color #:gainsboro #xDCDCDC)
(define-color #:white-smoke #xF5F5F5)
