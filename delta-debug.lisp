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

(defun minimize (original diff test )
  "Returns minimal subset of DIFF against ORIGINAL which fails TEST.
DIFF should be a list of atomic changes to ORIGINAL."
  (minimize- original diff test 2))

;; Requires that test is memoized.
(defun minimize- (original diff test n)
  (block nil
    (if (atomic diff) (return diff))
    ;; 2. try complements and recurse with n-1 on failure
    (mapc (lambda (sub-diff)
            (when (test (apply-diff original sub-diff))
              (return (minimize- original sub-diff test (1- n)))))
          (mapcar {complement diff}
                  ;; 1. try sub-diffs and divide and conquer on failure
                  (mapc (lambda (sub-diff)
                          (when (test (apply-diff original sub-diff))
                            (return (minimize- original sub-diff test 2))))
                        (split diff n))))
    ;; 3. we've learned nothing, increase n and try again
    (if (= n (length diff))
        (return diff)
        (minimize- original diff test (min (length diff) (* 2 n))))))

(defun apply-patch (original patch))

(defun apply-diff (original diff &aux (result (copy-tree original)))
  (loop :for patch :in diff :do (setf result (apply-patch result patch)))
  result)
