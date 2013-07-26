;;; test.lisp --- tests for delta-debug.lisp

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :delta-debug-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defsuite test)
(in-suite test)

(defvar numbers nil)

(defixture numbers
  (:setup (setf numbers '(1 2 3 4 5 6 7 8 9)))
  (:teardown (setf numbers nil)))

(deftest split-minimization ()
  (flet ((one-and-eight (it) (and (member 1 it) (member 8 it))))
    (with-fixture numbers
      (is (tree-equal (minimize numbers #'one-and-eight)
                      '(1 8))))))

(deftest should-memoize ()
  (let (all)
    (flet ((recording (it)
             (push it all)
             (> (reduce #'+ it) 16)))
      (with-fixture numbers
        (is (tree-equal (minimize numbers #'recording)
                        '(8 9)))
        (is (= (length all)
               (length (remove-duplicates all :test #'tree-equal))))))))
