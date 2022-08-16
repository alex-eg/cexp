(load "~/.sbclrc")
(ql:quickload :cexp :silent t)
(in-package :cexp)
(let ((forms
        (eval-when (:compile-toplevel :load-toplevel :execute)
          (setf (readtable-case *readtable*) :preserve)
          (PROG1
              (UIOP:READ-FILE-FORMS (CADR SB-EXT:*POSIX-ARGV*))
            (SETF (READTABLE-CASE *READTABLE*) :UPCASE)))))
  (format t "~a" (cexp::process forms)))
