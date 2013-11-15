(defsystem :delta-debug
  :description "implementation of delta debugging in common lisp"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria curry-compose-reader-macros)
  :components
  ((:file "package")
   (:file "delta-debug" :depends-on ("package"))))
