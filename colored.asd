(asdf:defsystem colored
  :version "1.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "System for colour representation, conversion, and operation."
  :homepage "https://shinmera.github.io/colored"
  :bug-tracker "https://github.com/Shinmera/colored/issues"
  :source-control (:git "https://github.com/Shinmera/colored.git")
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
