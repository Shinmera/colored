#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(declaim (inline %color))
(defstruct (color
            (:constructor %color (r g b a))
            (:conc-name NIL)
            (:predicate NIL)
            (:copier NIL))
  (r 0.0f0 :type single-float :read-only T)
  (g 0.0f0 :type single-float :read-only T)
  (b 0.0f0 :type single-float :read-only T)
  (a 1.0f0 :type single-float :read-only T))

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
    (if (and (constantp r env) (constantp g env) (constantp b env) (constantp a env))
        `(load-time-value (%color (float ,r 0f0) (float ,g 0f0) (float ,b 0f0) (float ,a 0f0)))
        `(%color ,(fold r) ,(fold g) ,(fold b) ,(fold a)))))

(defun 2color= (a b)
  (and (= (r a) (r b))
       (= (g a) (g b))
       (= (b a) (b b))
       (= (a a) (a b))))

(defun color= (color &rest more)
  (loop for other in more
        always (2color= color other)))

(define-compiler-macro color= (color &rest more)
  (if more
      (let ((colorg (gensym "COLOR")))
        `(let ((,colorg ,color))
           (and ,@(loop for other in more
                        collect `(2color= ,colorg ,other)))))
      T))

;; TODO: ICC Color space conversions http://www.color.org/specification/ICC1v43_2010-12.pdf
;;       This would also include CMYK colors and the conversion between RGB<->CMYK.
