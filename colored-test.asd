(asdf:defsystem colored-test
  :version "0.0.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Tests for the Colored colour library."
  :homepage "https://github.com/Shinmera/colored"
  :serial T
  :components ((:file "test"))
  :depends-on (:colored
               :parachute)
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :org.shirakumo.alloy.colored.test)))
