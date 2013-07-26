(defsystem :delta-debug-exe
  :description "compile a delta-debugging command line executable"
  :version "0.0.0"
  :licence "GPL V3"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               delta-debug
               cl-launch
               trivial-shell
               split-sequence)
  :components
  ((:file "package-exe")
   (:file "delta-debug-exe" :depends-on ("package-exe"))))