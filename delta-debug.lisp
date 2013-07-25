;;; delta-debug.lisp --- implementation of delta debugging in common lisp

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary

;; Implementation of the TSE delta debugging algorithm.

;; @article{zeller2002simplifying,
;;   title={Simplifying and isolating failure-inducing input},
;;   author={Zeller, Andreas and Hildebrandt, Ralf},
;;   journal={Software Engineering, IEEE Transactions on},
;;   volume={28},
;;   number={2},
;;   pages={183--200},
;;   year={2002},
;;   publisher={IEEE}
;; }

;;; Code:
(in-package :delta-debug)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defun minimize (original test &key (var-eql #'tree-equal) &aux memory)
  "Returns minimal subset of ORIGINAL which passes TEST.
Optional argument VAR-EQL should be a function used to test equality
between subsets of ORIGINAL for memoization of calls to TEST."
  (minimize- original
             (lambda (arg)
               (when (or (cdr (assoc arg memory :test var-eql))
                         (if (funcall test arg)
                             (prog1 t (push (cons arg t) memory))
                             (prog1 nil (push (cons arg nil) memory))))
                 arg))
             2))

(defun minimize- (original test n &aux passed (subsets (split original n)))
  (cond
    ((= 1 (length original)) original)
    ;; divide and conquer on failing subset
    ((setf passed (some test subsets))
     (minimize- passed test 2))
    ;; recurse with n-1 on failing complement
    ((setf passed (some [test {remove-if {member _ original}}] subsets))
     (minimize- passed test (1- n)))
    ;; increase granularity and try again
    ((< n (length original))
     (minimize- original test (min (length original) (* 2 n))))
    ;; otherwise we already have the smallest possible diff
    (t original)))

(defun first-which (func list)
  (mapc (lambda (el) (when (funcall func el) (return-from first-which el)))
        list) nil)

(defun split (obj n &aux (i 0) (chunk (/ (length obj) n)))
  (loop :for j :below n :collect
     (subseq obj (floor i) (min (floor (incf i chunk)) (length obj)))))
