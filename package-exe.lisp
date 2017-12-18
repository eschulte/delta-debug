(defpackage #:delta-debug/exe
  (:use :common-lisp
        :alexandria
        :metabang-bind
        :named-readtables
        :curry-compose-reader-macros
        :delta-debug
        :diff
        :trivial-shell
        :split-sequence)
  (:export :main))
