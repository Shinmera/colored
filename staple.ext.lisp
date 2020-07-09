(asdf:load-system :staple-markless)

(defpackage "colored-docs"
  (:use #:cl)
  (:local-nicknames
   (#:colored #:org.shirakumo.alloy.colored)))

(defclass page* (staple:simple-page)
  ()
  (:default-initargs :document-package (find-package "colored-docs")))

(defmethod staple:page-type ((system (eql (asdf:find-system :colored))))
  'page*)

(defmethod staple:definition-wanted-p ((_ definitions:source-transform) (page page*)) NIL)
