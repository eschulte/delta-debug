(defsystem :delta-debug
  :description "implementation of delta debugging in common lisp"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               curry-compose-reader-macros
               cl-launch
               trivial-shell
               split-sequence)
  :components
  ((:file "package")
   (:file "delta-debug" :depends-on ("package"))))
