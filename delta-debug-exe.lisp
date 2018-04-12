;;; delta-debug-exe.lisp --- command line delta debugging executable

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
(in-package :delta-debug/exe)
(in-readtable :curry-compose-reader-macros)

(defvar out t "Output stream to write results.")
(defvar verbose nil "Verbose output")
(defvar keep nil "Keep temporary files")
(defvar script nil "Test script.")

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)))))

;;; Taken from http://cl-cookbook.sourceforge.net/os.html
(defun getenv (name &optional default)
  #+CMU
  (let ((x (assoc name ext:*environment-list*
                  :test #'string=)))
    (if x (cdr x) default))
  #-CMU
  (or
   #+Allegro (sys:getenv name)
   #+CLISP (ext:getenv name)
   #+ECL (si:getenv name)
   #+SBCL (sb-unix::posix-getenv name)
   #+LISPWORKS (lispworks:environment-variable name)
   default))

(defun in-path-p (exe)
  (some (lambda (base) (probe-file (concatenate 'string base "/" exe)))
        (split-sequence #\: (getenv "PATH"))))

#+sbcl
(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

#+ccl
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base dir) (prefix prefix))
    (ccl:get-foreign-namestring (#_tempnam base prefix))))

(defun temp-file-name (&optional ext)
  (let ((base #+clisp
          (let ((stream (gensym)))
            (eval `(with-open-stream (,stream (ext:mkstemp nil))
                     (pathname ,stream))))
          #+(or sbcl ccl)
          (tempnam nil nil)
          #-(or sbcl clisp ccl)
          (error "no temporary file backend for this lisp.")))
    (if ext
        (concatenate 'string base "." ext)
        base)))

(defmacro with-temp-file (spec &rest body)
  "SPEC holds the variable used to reference the file w/optional extension.
After BODY is executed the temporary file is removed."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn ,@body)
       (when (and (probe-file ,(car spec)) (not keep))
         (delete-file ,(car spec))))))

(defun file-to-lines (file)
  (split-sequence #\Newline
    (with-open-file (in file)
      (let ((seq (make-string (file-length in))))
        (read-sequence seq in)
        seq))))

(defun lines-to-file (lines file)
  (with-open-file (out file :direction :output)
    (format out "~{~a~^~%~}~%" lines)))

(defun script-over-lines (lines)
  (with-temp-file (file)
    (lines-to-file lines file)
    (multiple-value-bind (out err errno)
        (shell-command
         (if (in-path-p (car (split-sequence #\Space script)))
             (format nil "~a ~a" script file)
             (format nil "./~a ~a" script file))
         :input #+ccl "" #+sbcl nil)
      (declare (ignorable out err))
      (when verbose
        (format t ";; ~a ~a -> ~a~%" script file errno))
      (zerop errno))))

(defun minimize-lines (file)
  "Minimize the lines of FILE."
  (minimize (file-to-lines file) #'script-over-lines))

(defun minimize-diffs (diff)
  "Minimize the diffs in PATCH-FILE."
  (let ((base (file-to-lines (original-pathname diff))))
    (setf (diff-windows diff)
          (minimize (diff-windows diff)
                    (lambda (windows)
                      (script-over-lines
                       (car (reduce
                             (lambda-bind ((seq offset) window)
                               (multiple-value-call #'list
                                 (apply-seq-window seq window :offset offset)))
                             windows :initial-value (list base 0)))))))
    diff))

(defun main (args)
  (let ((self (pop args)))
    (when (or (not args) (< (length args) 2)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t "Usage: ~a TEST-SCRIPT FILE [OPTIONS...]
 Minimize the contents of FILE as much as possible,
 such that TEST-SCRIPT continues to exit successfully
 when passed FILE as its only argument.  If FILE has
 a \".patch\" extension, treat it as a unified diff
 (e.g., as generated with \"diff -u\").

Options:
 -o,--out FILE ------ write output to FILE
 -p,--patch --------- force treating FILE as a patch
 -k,--keep ---------- keep temporary files
 -v,--verbose ------- verbose output~%" self) (quit)))

  (setf script (pop args))
  (let ((in-file (pop args)) (out *standard-output*) patch)

    (getopts
      ("-o" "--out"     (setf out (open (pop args) :direction :output)))
      ("-p" "--patch"   (setf patch t))
      ("-k" "--keep"    (setf keep t))
      ("-v" "--verbose" (setf verbose t)))

    (if (or patch (string= (pathname-type (pathname in-file)) "patch"))
        (let ((patches (diff::read-patches-from-file in-file)))
          (unless patches
            (format *error-output* "file ~s not a recognizable patch~%" in-file)
            (quit 1))
          (render-diff (minimize-diffs (diff (first patches))) out))
        (format out "~{~a~^~%~}~%" (minimize-lines in-file)))

    (when (not (equalp out *standard-output*)) (close out))))
