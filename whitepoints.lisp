#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.alloy.colored)

(defvar *whitepoints* ())

(defun whitepoint (name)
  (let ((entry (assoc name *whitepoints*)))
    (if entry
        (cdr entry)
        (error "No whitepoint named ~s known." name))))

(defun xy-to-XYZ (x y)
  (list (float (* (/ 100 y) x) 0f0)
        100f0
        (float (* (/ 100 y) (- 1 x y)) 0f0)))

(defun (setf whitepoint) (values name)
  (destructuring-bind (x y) values
    (let ((values (make-array 3 :element-type 'single-float :initial-contents (xy-to-XYZ x y)))
          (entry (assoc name *whitepoints*)))
      (if entry
          (setf (cdr entry) values)
          (push (cons name values) *whitepoints*))
      values)))

(defmacro define-whitepoint (name xy)
  `(setf (whitepoint ',name) ,xy))

(defun compute-d-whitepoint (cct)
  (let ((x (cond ((<= 4000 cct 7000)
                  (+ 0.244063
                     (* 0.09911 (/ (expt 10 3) cct))
                     (* 2.9678 (/ (expt 10 6) (expt cct 2)))
                     (* -4.607 (/ (expt 10 9) (expt cct 3)))))
                 ((<= 7000 cct 25000)
                  (+ 0.23704
                     (* 0.24748 (/ (expt 10 3) cct))
                     (* 1.9018 (/ (expt 10 6) (expt cct 2)))
                     (* -2.0064 (/ (expt 10 9) (expt cct 3))))))))
    (list x
          (+ (* -3 x x)
             (* 2.87 x)
             (- 0.275)))))

(define-whitepoint :A '(0.44758 0.40745))
(define-whitepoint :B '(0.34842 0.35161))
(define-whitepoint :C '(0.31006	0.31616))
(define-whitepoint :D50 '(0.34567 0.35850))
(define-whitepoint :D55 '(0.33242 0.34743))
(define-whitepoint :D60 (compute-d-whitepoint 6000))
(define-whitepoint :D65 '(0.31271 0.32902))
(define-whitepoint :D70 (compute-d-whitepoint 7000))
(define-whitepoint :D75 '(0.29902 0.31485))
(define-whitepoint :E '(1/3 1/3))
(define-whitepoint :F1 '(0.31310 0.33727))
(define-whitepoint :F2 '(0.37208 0.37529))
(define-whitepoint :F3 '(0.40910 0.39430))
(define-whitepoint :F4 '(0.44018 0.40329))
(define-whitepoint :F5 '(0.31379 0.34531))
(define-whitepoint :F6 '(0.37790 0.38835))
(define-whitepoint :F7 '(0.31292 0.32933))
(define-whitepoint :F8 '(0.34588 0.35875))
(define-whitepoint :F9 '(0.37417 0.37281))
(define-whitepoint :F10 '(0.34609 0.35986))
(define-whitepoint :F11 '(0.38052 0.37713))
(define-whitepoint :F12 '(0.43695 0.40441))
(define-whitepoint :LED-B1 '(0.4560 0.4078))
(define-whitepoint :LED-B2 '(0.4357 0.4012))
(define-whitepoint :LED-B3 '(0.3756 0.3723))
(define-whitepoint :LED-B4 '(0.3422 0.3502))
(define-whitepoint :LED-B5 '(0.3118 0.3236))
(define-whitepoint :LED-BH1 '(0.4474 0.4066))
(define-whitepoint :LED-RGB1 '(0.4557 0.4211))
(define-whitepoint :LED-V1 '(0.4560 0.4548))
(define-whitepoint :LED-V2 '(0.3781 0.3775))
