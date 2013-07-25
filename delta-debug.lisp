;;; delta-debug.lisp --- implementation of delta debugging in common lisp

;; Copyright (C) Eric Schulte

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

(defun minimize (original diff test)
  "Returns minimal subset of DIFF against ORIGINAL which passes TEST.
DIFF should be a list of atomic changes to ORIGINAL."
  (minimize- original diff test 2))

;; Requires that test is memoized.
(defun minimize- (original diff test n &aux failed (ds (split diff n)))
  (cond
    ;; divide and conquer on failing subset
    ((setf failed (some {apply-diff original} ds))
     (minimize- original failed test 2))
    ;; recurse with n-1 on failing complement
    ((setf failed (some [{apply-diff original} {remove-if {member _ diff}}] ds))
     (minimize- original failed test (1- n)))
    ;; increase granularity and try again
    ((< n (length diff))
     (minimize- original diff test (min (length diff) (* 2 n))))
    ;; otherwise we already have the smallest possible diff
    (t diff)))

(defun split (diff n &aux (i 0) (chunk (/ (length diff) n)))
  (loop :for j :below n :collect
     (subseq diff (floor i) (min (floor (incf i chunk)) (length diff)))))

(defun apply-patch (original patch) #| TODO: implement |#)

(defun apply-diff (original diff)
  (loop :for patch :in diff :do (setf original (apply-patch original patch)))
  original)
