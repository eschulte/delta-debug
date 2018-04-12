;;; test.lisp --- tests for delta-debug.lisp

;; Copyright (C) Eric Schulte 2013

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

;;; Code:
(defpackage #:delta-debug/test
  (:use :common-lisp
        :alexandria
        :named-readtables
        :curry-compose-reader-macros
        :delta-debug
        :stefil)
  (:export :test))
(in-package :delta-debug/test)
(in-readtable :curry-compose-reader-macros)

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
