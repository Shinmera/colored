#|
 This file is a part of Colored
 (c) 2019 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem colored
  :version "0.0.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "System for RGB colour representation, conversion, and operation."
  :homepage "https://github.com/Shinmera/colored"
  :serial T
  :components ((:file "package")
               (:file "whitepoints")
               (:file "conversion-matrices")
               (:file "type")
               (:file "ops")
               (:file "constants")
               (:file "documentation"))
  :depends-on (:documentation-utils)
  :in-order-to ((asdf:test-op (asdf:test-op :colored-test))))
