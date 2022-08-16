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

(defun produce-ptr-declaration (form)
  (assert (eq '|ptr| (car form)))
  (list (produce-type-declaration (cdr form)) "*"))

(defun produce-type-declaration (form)
  ;; TODO: checks! no more than 1 ptr per level,
  ;; no more than 1 type in the whole form, check for
  ;; incompatible type kws and qualificators
  (labels ((parse (form acc)
             (if (null form) acc
               (if (listp (car form))
                 (parse (cdr form)
                        (cons (produce-ptr-declaration (car form)) acc))
                 (parse (cdr form)
                        (cons (to-str (car form)) acc))))))
    (parse form (list))))

(defun produce-variable-name (name)
  (map 'string
       (lambda (char)
         (if (char= #\- char)
           #\_
           char))
       (to-str name)))

(defun produce-expression (form)
  (labels ((process (form acc)
             nil))))

(defun produce-variable-declaration (form)
  ;; (var name (<type>) [<value>])
  (assert (eq '|var| (car form)))
  (let ((name (second form))
        (type (third form)))
    (append
     (produce-type-declaration type)
     (list (produce-variable-name name))
     (when (= 4 (length form))
       (produce-expression (fourth form)))
     (list ";"))))

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

(defun process-string (string)
  (format nil "\"~a\""
          (map 'string (lambda (char)
                         (if (char= char #\~)
                           #\\
                           char))
               string)))

(defun parse-arglist (form)
  (nreverse
   (reduce (lambda (acc token)
             (cons (cond ((symbolp token)
                          (to-str token))
                         ((numberp token)
                          (to-str-num token))
                         ((stringp token)
                          (process-string token)))
                   acc))
           form :initial-value (list))))

(defun parse-funcall (form)
  (format nil "~a(~{~a~^, ~});" (to-str (car form)) (parse-arglist (cdr form))))

(defun to-str (sym)
  (string sym))

(defun to-str-num (num)
  (format nil "~S" num))

(defun parse-statement (form)
  (case (car form)
    (|var|
       (flatten (produce-variable-declaration form) :delimiter " "))
    (otherwise (parse-funcall form))))

(defun parse-fn (form)
  (let ((fn-strings (list)))
    (push (string (cadddr form)) fn-strings)
    (push (string (car form)) fn-strings)
    (push "{" fn-strings)
    (dolist (line (nthcdr 4 form))
      (push (parse-statement line) fn-strings))
    (push "}" fn-strings)))


(defun process (program)
  (let ((program-strings (list)))
    (dolist (form program)
      (case (car form)
        (|include| (push "#include" program-strings)
           (push (format nil "~s" (cadr form)) program-strings)
           (push #\Newline program-strings))
        (|fn| (setf program-strings (nconc (parse-fn (cdr form)) program-strings)))
        (otherwise (format t "Unknown key ~S (package ~S)~%" (car form) (symbol-package (car form))))))
    (format nil "~{~a~^ ~}" (nreverse program-strings))))
