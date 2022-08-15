(defpackage :cexp
  (:use :cl))

(in-package :cexp)

(defvar *type-keywords* '(|char| |int| |long| |short|
                          |float| |double|
                          |_Bool|
                          |signed| |unsigned|))

(defvar *qualifier-keywords* '(|static| |auto| |restrict|
                               |_Atomic| |_Thread_local|
                               |const| |volatile|))

(defun produce-typedef (form)
  (dolist (var (cdr form))
    (progn)))

(defun sort-type-declaration (form)
  (sort form (lambda (a b)
               (declare (ignore a))
               (listp b))))

(defun produce-type-declaration (form)
  ;; TODO: checks! no more than 1 ptr per level,
  ;; no more than 1 type in the whole form, check for
  ;; incompatible type kws and qualificators
  (labels ((parse (form acc)
             (if (null form) acc
               (if (listp (car form))
                 (progn
                   (assert (eq 'ptr (caar form)))
                   (cons (nreverse (parse (cdar form) (list)))
                         (cons "*" acc)))
                 (parse (cdr form)
                        (cons (to-str (car form)) acc))))))
    (parse (sort-type-declaration form) (list))))

(defun produce-variable-declaration (form)
  (assert (eq 'var (car form)))
  (let ((form (cdr form)))
    (str+ (flatten (produce-type-declaration (cdr form)) :delimiter " ")
          " " (to-str (car form)) ";")))

(defun str+ (&rest strs)
  (apply #'concatenate 'string strs))

(defun flatten (strings &key (delimiter ""))
  (let ((args
          (nreverse
           (reduce (lambda (acc arg)
                     (if (eq (car acc) :delimiter)
                       (cdr acc)
                       (cons arg acc)))
                   strings :initial-value (list)))))
    (labels ((process (acc strs delimiter)
               (reduce (lambda (acc str)
                         (concatenate 'string acc
                                      (if (listp str)
                                        (process "" str delimiter)
                                        (str+ str delimiter))))
                       strs :initial-value acc)))
      (let ((res (process "" args delimiter)))
        (subseq res 0 (- (length res)
                         (length delimiter)))))))

(defun parse-arglist (form)
  (nreverse
   (reduce (lambda (acc token)
             (cons (cond ((symbolp token)
                          (to-str token))
                         ((numberp token)
                          (to-str-num token))
                         ((stringp token)
                          (format nil "\"~a\"" (map 'string (lambda (char)
                                                          (if (char= char #\~)
                                                            #\\
                                                            char))
                                                token))))
                   acc))
           form :initial-value (list))))

(defun parse-funcall (form)
  (format nil "~a(~{~a~^, ~});" (to-str (car form)) (parse-arglist (cdr form))))

(defun to-str (sym)
  (sb-unicode:lowercase (string sym)))

(defun to-str-num (num)
  (format nil "~S" num))

(defun parse-statement (form)
  (case (car form)
    (var
       (produce-variable-declaration form))
    (otherwise (parse-funcall form))))

(defun parse-fn (form)
  (let ((fn-strings (list)))
    (push (sb-unicode:lowercase (string (cadddr form))) fn-strings)
    (push (sb-unicode:lowercase (string (car form))) fn-strings)
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
