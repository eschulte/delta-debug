(defsystem :delta-debug-test
  :description "test the delta-debug package"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               curry-compose-reader-macros
               delta-debug
               stefil)
  :components
  ((:file "package-test")
   (:file "delta-debug-test" :depends-on ("package-test"))))
