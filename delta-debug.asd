(defsystem "delta-debug"
  :description "implementation of delta debugging in common lisp"
  :version "0.0.0"
  :author "Eric Schulte <schulte.eric@gmail.com>"
  :licence "Public Domain"
  :depends-on (alexandria named-readtables curry-compose-reader-macros)
  :components
  ((:file "delta-debug")))

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
  ((:file "delta-debug-exe")))

(defsystem "delta-debug/test"
  :description "test the delta-debug package"
  :version "0.0.0"
  :licence "Public Domain"
  :depends-on (alexandria
               curry-compose-reader-macros
               delta-debug
               stefil)
  :components
  ((:file "delta-debug-test")))
