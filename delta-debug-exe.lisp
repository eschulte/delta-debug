;;; delta-debug-exe.lisp --- command line delta debugging executable

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :delta-debug-exe)

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)))))

(defun main (&optional (args *arguments*))
  (when (or (not args) (< (length args) 2)
            (string= (subseq (car args) 0 2) "-h")
            (string= (subseq (car args) 0 3) "--h"))
    (format t "Usage: delta FILE TEST-SCRIPT
 Minimize the contents of FILE as much as possible,
 such that TEST-SCRIPT continues to exit successfully
 when passed FILE as its only argument.

Options:
 -o,--out FILE ------ write output to FILE~%") (quit))

  (let ((original (split-sequence #\Newline
                    (with-open-file (in (pop args))
                      (let ((seq (make-string (file-length in))))
                        (read-sequence seq in)
                        seq))))
        (script (pop args))
        (out t))

    (getopts
     ("-o" "--out" (setf out (open (pop args) :direction :output))))

    (format out "~{~a~^~%~}"
            (minimize original
                      (lambda (file)
                        (multiple-value-bind (out err errno)
                            (shell-command (format nil "~a ~a"  script file))
                          (declare (ignorable out err))
                          errno))))

    (when (streamp out) (close out))))
