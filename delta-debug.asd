(defsystem "delta-debug"
  :description "implementation of delta debugging in common lisp"
  :version "0.0.0"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :licence "Public Domain"
  :depends-on (alexandria named-readtables curry-compose-reader-macros)
  :components
  ((:file "package")
   (:file "delta-debug" :depends-on ("package"))))

(defsystem "delta-debug/delta"
  :description "compile a delta-debugging command line executable"
  :version "0.0.0"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :licence "Public Domain"
  :build-operation "asdf:program-op"
  :build-pathname "bin/delta"
  :entry-point "delta-debug/delta:main"
  :depends-on (alexandria
               metabang-bind
               curry-compose-reader-macros
               delta-debug
               diff
               trivial-shell
               split-sequence)
  :components
  ((:file "package-exe")
   (:file "delta-debug-exe" :depends-on ("package-exe"))))

(defsystem "delta-debug/test"
  :description "test the delta-debug package"
  :version "0.0.0"
  :licence "Public Domain"
  :depends-on (alexandria
               curry-compose-reader-macros
               delta-debug
               stefil)
  :components
  ((:file "package-test")
   (:file "delta-debug-test" :depends-on ("package-test"))))
