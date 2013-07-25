;;; delta.lisp --- compiles to a delta debugging executable

;; Copyright (C) Eric Schulte 2013

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :delta-debug)

(defun delta (&optional (args *arguments*))
  (let ((help ))
    (when (or (not args) (< (length args) 2)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t "Usage: delta FILE TEST-SCRIPT
 Minimize the contents of FILE as much as possible,
 such that TEST-SCRIPT continues to exit successfully
 when passed FILE as its only argument.~%") (quit))

    (let ((original (split-sequence #\Newline
                      (with-open-file (in (pop args))
                        (let ((seq (make-string (file-length in))))
                          (read-sequence seq in)
                          seq))))
          (script (pop args)))

      (format t "~{~a~^~%~}"
              (minimize original
                        (lambda (file)
                          (multiple-value-bind (out err errno)
                              (shell-command (format nil "~a ~a"  script file))
                            (declare (ignorable out err))
                            errno)))))))
