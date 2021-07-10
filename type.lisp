#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(defgeneric convert (color target-type &key &allow-other-keys))
(defgeneric channels (color))
(defgeneric 2color= (a b))
(defgeneric 2color-equal (a b))

(defstruct (color
            (:constructor _color (a))
            (:conc-name NIL)
            (:predicate NIL)
            (:copier NIL))
  (a 1.0f0 :type single-float :read-only T))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod channels ((_ color)) NIL)
  (defmethod channels ((_ (eql 'color))) NIL))

(defmacro define-color-type (name fields &optional (super 'color))
  (macrolet ((ftransform (transform)
               `(loop for field in fields
                      collect ,transform)))
    (let ((%name (intern (format NIL "%~a" name)))
          (sfields (channels super)))
      `(progn
         (declaim (inline ,%name))
         (defstruct (,name
                     (:include ,super)
                     (:constructor ,%name (,@fields a))
                     (:conc-name NIL)
                     (:predicate NIL)
                     (:copier NIL))
           ,@(loop for field in fields
                   unless (find field sfields)
                   collect `(,field 0.0f0 :type single-float :read-only T)))

         (defmethod print-object ((color ,name) stream)
           (format stream "~s" (list ',name ,@(ftransform `(,field color)) (a color))))

         (defmethod make-load-form ((color ,name) &optional env)
           (declare (ignore env))
           (list ',%name ,@(ftransform `(,field color)) (a color)))

         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defmethod channels ((_ ,name)) ',fields)
           (defmethod channels ((_ (eql ',name))) ',fields))

         (defun ,name (,@fields &optional (a 1.0f0))
           (,%name ,@(ftransform `(float ,field 0f0))
                   (float a 0f0)))

         (define-compiler-macro ,name (,@fields &optional (a 1f0) &environment env)
           (flet ((fold (arg)
                    (if (constantp arg env)
                        `(load-time-value (float ,arg 0f0))
                        `(float ,arg 0f0))))
             (if (and ,@(ftransform `(constantp ,field env)) (constantp a env))
                 (list 'load-time-value (list ',%name ,@(ftransform ``(float ,,field 0f0)) `(float ,a 0f0)))
                 (list ',%name ,@(ftransform `(fold ,field)) (fold a)))))

         (defmethod convert ((color ,name) (_ (eql ',name)) &key) color)

         (defmethod 2color= ((a ,name) (b ,name))
           (and ,@(ftransform `(= (,field a) (,field b)))
                (= (a a) (a b))))

         (defmethod 2color-equal ((a ,name) (b ,name))
           (and ,@(ftransform `(= (,field a) (,field b)))))))))

(define-color-type rgb (r g b))
(define-color-type srgb (r g b) rgb)
(define-color-type hue-type (h s))
(define-color-type hsv (h s v) hue-type)
(define-color-type hsl (h s l) hue-type)
(define-color-type hsi (h s i) hue-type)
(define-color-type cmyk (c m y k))
(define-color-type xyz (x* y* z*))
(define-color-type lab (l* a* b*))

(defun color (r g b &optional (a 1f0))
  (%rgb (float r 0f0) (float g 0f0) (float b 0f0) (float a 0f0)))

(define-compiler-macro color (r g b &optional (a 1.0) &environment env)
  (flet ((fold (arg)
           (if (constantp arg env)
               `(load-time-value (float ,arg 0.0))
               `(float ,arg 0.0))))
    (if (and (constantp r env) (constantp g env) (constantp b env) (constantp a env))
        (list 'load-time-value
              (list '%rgb `(float ,r 0.0) `(float ,g 0.0) `(float ,b 0.0)
                    `(float ,a 0.0)))
        (list '%rgb (fold r) (fold g) (fold b) (fold a)))))

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

(defun color-equal (color &rest more)
  (loop for other in more
        always (2color-equal color other)))

(define-compiler-macro color-equal (color &rest more)
  (if more
      (let ((colorg (gensym "COLOR")))
        `(let ((,colorg ,color))
           (and ,@(loop for other in more
                        collect `(2color-equal ,colorg ,other)))))
      T))


;; TODO: ICC Color space conversions http://www.color.org/specification/ICC1v43_2010-12.pdf
;;       This would also include CMYK colors and the conversion between RGB<->CMYK.
;; TODO: YUV, YCbCr mappings
;; TODO: Test suite
