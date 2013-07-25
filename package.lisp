(defpackage #:delta-debug
  (:use :common-lisp
        :alexandria
        :curry-compose-reader-macros
        :cl-launch
        :trivial-shell
        :split-sequence)
  (:export :minimize :delta))
