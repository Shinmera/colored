#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.shirakumo.alloy.colored.test
  (:use #:cl #:parachute)
  (:local-nicknames
   (#:colored #:org.shirakumo.alloy.colored))
  (:export #:colored))

(in-package #:org.shirakumo.alloy.colored.test)

(define-test colored)

(define-test type
  :parent colored
  )

(define-test integer
  :parent colored
  :depends-on (type)
  )

(define-test mapping
  :parent colored
  :depends-on (type)
  )

(define-test hdr
  :parent colored
  :depends-on (type)
  )
