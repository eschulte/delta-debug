;;; delta-debug-exe.lisp --- command line delta debugging executable

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :delta-debug-exe)

(defvar out t "Output stream to write results.")
(defvar verbose nil "Verbose output")
(defvar keep nil "Keep temporary files")

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)))))

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

(defun main (&optional (args *arguments*))
  (when (or (not args) (< (length args) 2)
            (string= (subseq (car args) 0 2) "-h")
            (string= (subseq (car args) 0 3) "--h"))
    (format t "Usage: delta TEST-SCRIPT FILE
 Minimize the contents of FILE as much as possible,
 such that TEST-SCRIPT continues to exit successfully
 when passed FILE as its only argument.

Options:
 -o,--out FILE ------ write output to FILE
 -k,--keep ---------- keep temporary files
 -v,--verbose ------- verbose output~%") (quit))

  (let ((script (pop args))
        (original (split-sequence #\Newline
                    (with-open-file (in (pop args))
                      (let ((seq (make-string (file-length in))))
                        (read-sequence seq in)
                        seq)))))

    (getopts
     ("-o" "--out"     (setf out (open (pop args) :direction :output)))
     ("-v" "--verbose" (setf verbose t))
     ("-k" "--keep"    (setf keep t)))

    (format out "~{~a~^~%~}~%"
            (minimize original
                      (lambda (contents)
                        (with-temp-file (file)
                          (with-open-file (out file :direction :output)
                            (format out "~{~a~^~%~}~%" contents))
                          (multiple-value-bind (out err errno)
                              (shell-command
                               (if (pathname-directory (pathname script))
                                   (format nil "~a ~a" script file)
                                   (format nil "./~a ~a" script file)))
                            (declare (ignorable out err))
                            (when verbose
                              (format t ";; ~a ~a -> ~a~%" script file errno))
                            (zerop errno))))))

    (when (streamp out) (close out))))
