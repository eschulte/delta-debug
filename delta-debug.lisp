;;; delta-debug.lisp --- implementation of delta debugging in common lisp

;; Copyright (C) 2013 Eric Schulte

;; Placed in the public domain.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary

;; Implementation of the delta debugging algorithm from the following.
;;
;;   Zeller, Andreas, and Ralf Hildebrandt. "Simplifying and isolating
;;   failure-inducing input." Software Engineering, IEEE Transactions
;;   on 28.2 (2002): 183-200.

;;; Code:
(in-package :delta-debug)
(in-readtable :curry-compose-reader-macros)

(defun minimize (original test &aux memory)
  "Returns minimal subset of ORIGINAL (a list) which passes TEST."
  (assert (listp original) (original) "Original must be a list")
  (assert (funcall test original) (test original) "Original should pass test")
  (assert (not (funcall test nil)) (test) "`nil' should not pass test")
  (let ((array (coerce original 'vector)))
    (mapcar {aref array}
            (minimize-
             (loop :for ind :below (length original) :collect ind)
             (lambda (indexes)
               (let ((already (assoc indexes memory :test #'tree-equal)))
                 (when (or (if already
                               (cdr already)
                               (if (funcall test (mapcar {aref array} indexes))
                                   (prog1 t (push (cons indexes t) memory))
                                   (prog1 nil (push (cons indexes nil) memory)))))
                   indexes)))
             2))))

(defun minimize- (original test n &aux passed (subsets (split original n)))
  (flet ((all-but (set subset) (remove-if {member _ subset} set)))
    (cond
      ((= 1 (length original)) original)
      ;; divide and conquer on failing subset
      ((setf passed (some test subsets))
       (minimize- passed test 2))
      ;; recurse with n-1 on failing complement
      ((setf passed (some [test {all-but original}] subsets))
       (minimize- passed test (1- n)))
      ;; increase granularity and try again
      ((< n (length original))
       (minimize- original test (min (length original) (* 2 n))))
      ;; otherwise we already have the smallest possible diff
      (t original))))

(defun split (obj n &aux (i 0) (chunk (/ (length obj) n)))
  (loop :for j :below n :collect
     (subseq obj (floor i) (min (floor (incf i chunk)) (length obj)))))
