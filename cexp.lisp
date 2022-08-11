(defpackage :cexp
  (:use :cl))

(in-package :cexp)

(defparameter syms '((include "<stdio.h>")

                     (fn main () -> int
                      (int i 12)
                      (int j 34)
                      (lambda foo [i &j] (int )
                              (set i 56)
                              (set j 78)
                              (printf "Inside foo: %d %d\n" i j))
                      (printf "Outside foo: %d %d\n" i j)
;                      (foo)
                      (return 0))))

(defparameter *lambdas* (list))

(defun flatten (&rest strings)
  (reduce (lambda (acc str)
            (concatenate 'string acc
                         (if (listp str)
                           (apply #'flatten str)
                           str)))
          strings :initial-value ""))

(defun parse-arglist (form)
  (nreverse
   (reduce (lambda (acc token)
             (cons (cond ((symbolp token)
                          (to-str token))
                         ((numberp token)
                          (to-str-num token))
                         ((stringp token)
                          (format nil "~s" token)))
                   acc))
           form :initial-value (list))))

(defun parse-funcall (form)
  (format nil "~a(~{~a~^, ~});" (to-str (car form)) (parse-arglist (cdr form))))

(defun to-str (sym)
  (sb-unicode:lowercase (string sym)))

(defun to-str-num (num)
  (format nil "~S" num))

(defun parse-args (form)
  "(void)")

(defun parse-lambda (form)
  "")

(defun parse-statement (form)
  (case (car form)
    (int
       (flatten "int " (to-str (cadr form))
                (if (> (length form) 2)
                  (flatten " = " (to-str-num (caddr form)) ";")
                  ";")))
    (lambda (parse-lambda (cdr form)))
    (otherwise (parse-funcall form))))

(defun parse-fn (form)
  (let ((fn-strings (list)))
    (push (sb-unicode:lowercase (string (cadddr form))) fn-strings)
    (push (sb-unicode:lowercase (string (car form))) fn-strings)
    (push (parse-args (cadr form)) fn-strings)
    (push "{" fn-strings)
    (dolist (line (nthcdr 4 form))
      (push (parse-statement line) fn-strings))
    (push "}" fn-strings)))


(defun process (program)
  (let ((program-strings (list)))
    (dolist (form program)
      (case (car form)
        (include (push "#include" program-strings)
           (push (format nil "~s" (cadr form)) program-strings)
           (push #\Newline program-strings))
        (fn (setf program-strings (nconc (parse-fn (cdr form)) program-strings)))
        (otherwise (format t "Unknown key ~S (package ~S)~%" (car form) (symbol-package (car form))))))
    (format nil "~{~a~^ ~}" (nreverse program-strings))))
