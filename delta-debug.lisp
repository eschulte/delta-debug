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
             (lambda (variant)
               (or (cdr (assoc variant memory :test var-eql))
                   (cddr (push (cons variant (apply test variant)) memory))))
             2))

(defun minimize- (original test n &aux passed (subsets (split original n)))
  (cond
    ;; divide and conquer on failing subset
    ((setf passed (some test subsets))
     (minimize- original f test 2))
    ;; recurse with n-1 on failing complement
    ((setf passed (some [test {remove-if {member _ original}}] subsets))
     (minimize- original f test (1- n)))
    ;; increase granularity and try again
    ((< n (length diff))
     (minimize- original diff test (min (length diff) (* 2 n))))
    ;; otherwise we already have the smallest possible diff
    (t diff)))

(defun split (diff n &aux (i 0) (chunk (/ (length diff) n)))
  (loop :for j :below n :collect
     (subseq diff (floor i) (min (floor (incf i chunk)) (length diff)))))
