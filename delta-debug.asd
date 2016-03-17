(defsystem :delta-debug
  :description "implementation of delta debugging in common lisp"
  :version "0.0.0"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :licence "Public Domain"
  :depends-on (alexandria curry-compose-reader-macros)
  :components
  ((:file "package")
   (:file "delta-debug" :depends-on ("package"))))
