(defpackage #:delta-debug-exe
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :curry-compose-reader-macros
        :delta-debug
        :diff
        :cl-launch
        :trivial-shell
        :split-sequence)
  (:export :main))
