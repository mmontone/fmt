(in-package #:fmt)

(defvar *fmt-destination* nil)
(defvar *escape* nil)

(defun call-with-fmt-destination (destination function &rest args)
  (etypecase destination
    (null
     (with-output-to-string (stream)
       (apply function stream args)))
    (string
     (with-output-to-string (stream destination)
       (apply function stream args)))
    ((member t)
     (apply function *standard-output* args)
     nil)
    (stream
     (apply function destination args)
     nil)))

(defmacro with-fmt-destination ((var &optional (destination *fmt-destination*))
                                &body body)
  `(call-with-fmt-destination ,destination
                              (lambda (,var)
                                ,@body)))

(defmacro with-fmt ((&optional (destination '*fmt-destination*))
                    &body body)
  "Format clauses in BODY to DESTINATION"
  (alexandria:with-unique-names (stream)
    `(with-fmt-destination (,stream ,destination)
       (let ((*fmt-destination* ,stream))
         (macrolet ((:fmt (&rest clauses)
                      `(fmt ,',stream ,@clauses)))
           ,@(loop for clause in body
                collect
                  (compile-clause stream clause)))))))

(defmacro fmt (destination &rest clauses)
  (alexandria:with-unique-names (stream)
    `(with-fmt-destination (,stream ,destination)
       (macrolet ((:fmt (&rest clauses)
                    `(fmt ,',stream ,@clauses)))
         ,@(loop for clause in clauses
              collect
                (compile-clause stream clause))))))

(defun fmt* (&optional (destination *fmt-destination*) &rest clauses)
  (apply #'call-with-fmt-destination
         destination #'%fmt* clauses))

(defun %fmt* (destination &rest clauses)
  (loop for clause in clauses
     do (format-clause destination clause)))

;; Special arguments

(defvar *_* nil)

(defun read-arg (arg)
  (cond
    ((and (symbolp arg)
          (equalp (string arg) "_"))
     *_*)
    (t arg)))

;; Format operations

(defvar *format-operations* (make-hash-table :test #'equalp))

(defstruct format-operation
  name
  keywords
  format
  compile
  documentation)

(defmacro define-format-operation (name &body options)
  "Defines a new format operation"
  (flet ((extract-option (name)
           (cdr (find name options :key #'car))))
    (alexandria:with-unique-names (format-operation)
      `(let ((,format-operation (make-format-operation
                                 :name ',name
                                 :keywords ',(first (extract-option :keywords))
                                 :format ,(let ((format (extract-option :format)))
                                               `(lambda ,(first format)
                                                  ,@(rest format)))
                                 :compile ,(let ((compile (extract-option :compile)))
                                                `(lambda ,(first compile)
                                                   ,@(rest compile)))
                                 :documentation ,(first (extract-option :documentation)))))
         ,@(loop for keyword in (first (extract-option :keywords))
              collect `(setf (gethash ,keyword *format-operations*)
                             ,format-operation))))))

(defun find-format-operation (keyword)
  (gethash keyword *format-operations*))

(defvar *format-filters* (make-hash-table :test #'equalp))

(defstruct format-filter
  name
  keywords
  apply
  compile
  documentation)

(defmacro define-format-filter (name &body options)
  "Defines a new format filter"
  (flet ((extract-option (name)
           (cdr (find name options :key #'car))))
    (alexandria:with-unique-names (format-filter)
      `(let ((,format-filter (make-format-filter
                              :name ',name
                              :keywords ',(first (extract-option :keywords))
                              :apply ,(let ((apply (extract-option :apply)))
                                           `(lambda ,(first apply)
                                              ,@(rest apply)))
                              :compile ,(let ((compile (extract-option :compile)))
                                             `(lambda ,(first compile)
                                                ,@(rest compile)))
                              :documentation ,(first (extract-option :documentation)))))
         ,@(loop for keyword in (first (extract-option :keywords))
              collect `(setf (gethash ,keyword *format-filters*)
                             ,format-filter))))))

(defun find-format-filter (keyword &optional (error-p t))
  (or
   (gethash keyword *format-filters*)
   (error "Invalid filter: ~S" keyword)))

(defun apply-format-filter (keyword-or-cons arg)
  (etypecase keyword-or-cons
    (symbol
     (let ((filter (find-format-filter keyword-or-cons)))
       (funcall (format-filter-apply filter) arg)))
    (cons
     (if (equalp (first keyword-or-cons) 'function)
         (funcall (symbol-function (cadr keyword-or-cons)) arg)
         (let ((filter (find-format-filter (first keyword-or-cons))))
           (apply (format-filter-apply filter) arg (cdr keyword-or-cons)))))
    (function
     (funcall keyword-or-cons arg))))

(defun compile-format-filter (keyword-or-cons arg)
  (etypecase keyword-or-cons
    (symbol
     (let ((filter (find-format-filter keyword-or-cons)))
       (funcall (format-filter-compile filter) arg)))
    (cons
     (if (equalp (first keyword-or-cons) 'function)
         (list (cadr keyword-or-cons) arg)
         (let ((filter (find-format-filter (first keyword-or-cons))))
           (apply (format-filter-compile filter) arg (cdr keyword-or-cons)))))))

(define-format-filter upcase
  (:keywords (:up :upcase))
  (:apply (arg)
          (string-upcase arg))
  (:compile (arg)
            `(string-upcase ,arg))
  (:documentation "String upcase"))

(define-format-filter downcase
  (:keywords (:down :downcase))
  (:apply (arg)
          (string-downcase arg))
  (:compile (arg)
            `(string-downcase ,arg))
  (:documentation "String downcase"))

(define-format-filter capitalize
  (:keywords (:capitalize))
  (:apply (arg)
          (string-capitalize arg))
  (:compile (arg)
            `(string-capitalize ,arg))
  (:documentation "String capitalize"))

(define-format-filter trim
  (:keywords (:trim))
  (:apply (arg &rest chars)
          (string-trim (or chars (list #\ )) arg))
  (:compile (arg &rest chars)
            (let ((chars-bag (or chars (list #\ ))))
              `(string-trim ',chars-bag ,arg)))
  (:documentation "String trim filter"))

(define-format-filter default
  (:keywords (:default))
  (:apply (arg default)
          (or arg default))
  (:compile (arg default)
            `(or ,arg ,default))
  (:documentation "Default value filter"))

(define-format-filter truncate
  (:keywords (:truncate))
  (:apply (arg n &optional (rest "..."))
          (if (> (length arg) n)
              (concatenate 'string (subseq arg 0 n) rest)
              arg))
  (:compile (arg n &optional (rest "..."))
            `(if (> (length ,arg) ,n)
                 (concatenate 'string (subseq ,arg 0 ,n) ,rest)
                 ,arg))
  (:documentation "Truncate filter"))

(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
       for old-pos = 0 then (+ pos part-length)
       for pos = (search part string
			 :start2 old-pos
			 :test test)
       do (write-string string out
			:start old-pos
			:end (or pos (length string)))
       when pos do (write-string replacement out)
       while pos)))

(define-format-filter replace
  (:keywords (:replace))
  (:apply (arg part replacement)
          (replace-all arg part replacement))
  (:compile (arg part replacement)
	    `(replace-all ,arg ,part ,replacement))
  (:documentation "Replace filter"))

(defgeneric format-clause (destination clause))

(defmethod format-clause :around (destination clause)
  (when (not *escape*)
    (call-next-method)))

(defmethod format-clause (destination (clause string))
  (write-string clause destination))

(defmethod format-clause (destination (clause character))
  (write-char clause destination))

(defmethod format-clause (destination (clause (eql :newline)))
  (terpri destination))

(defmethod format-clause (destination (clause cons))
  (let ((format-operation (find-format-operation (first clause))))
    (if format-operation
        (funcall (format-operation-format format-operation)
                 destination
                 clause)
        (error "Invalid format operation: ~A" (first clause)))))

(defmethod format-clause (destination clause)
  (princ clause destination))

(defmethod compile-clause (destination (clause string))
  `(write-string ,clause ,destination))

(defmethod compile-clause (destination (clause character))
  `(write-char ,clause ,destination))

(defmethod compile-clause (destination (clause cons))
  (if (keywordp (first clause))
      (let ((format-operation (find-format-operation (first clause))))
        (if format-operation
            (funcall (format-operation-compile format-operation)
                     destination
                     clause)
            (error "Invalid format operation: ~A" (first clause))))
      `(princ ,clause ,destination)))

(defmethod compile-clause (destination clause)
  `(princ ,clause ,destination))

(define-format-operation standard
  (:keywords (:s :std :standard))
  (:format (destination clause)
           (destructuring-bind (_ arg &rest filters) clause
             (declare (ignore _))
             (let ((arg (read-arg arg)))
               (loop for filter in filters
                  do (setf arg (apply-format-filter filter arg)))
               (prin1 arg destination))))
  (:compile (destination clause)
            (alexandria:with-unique-names (read-arg)
              (destructuring-bind (_ arg &rest filters) clause
                (declare (ignore _))
                `(let ((,read-arg (read-arg ,arg)))
                   ,@(loop for filter in filters
                        collect `(setf ,read-arg ,(compile-format-filter filter read-arg)))
                   (prin1 ,read-arg ,destination)))))
  (:documentation "Standard print"))

(define-format-operation aesthetic
  (:keywords (:a :aesthetic))
  (:format (destination clause)
           (destructuring-bind (_ arg &rest filters) clause
             (declare (ignore _))
             (let ((arg (read-arg arg)))
               (loop for filter in filters
                  do (setf arg (apply-format-filter filter arg)))
               (princ arg destination))))
  (:compile (destination clause)
            (alexandria:with-unique-names (read-arg)
              (destructuring-bind (_ arg &rest filters) clause
                (declare (ignore _))
                `(let ((,read-arg (read-arg ,arg)))
                   ,@(loop for filter in filters
                        collect `(setf ,read-arg ,(compile-format-filter filter read-arg)))
                   (princ ,read-arg ,destination)))))
  (:documentation "Aesthetic print"))

(define-format-operation when
  (:keywords (:w :when))
  (:format (destination clause)
           (destructuring-bind (_ condition &rest body) clause
             (declare (ignore _))
             (when condition
               (loop for clause in body
                  do (format-clause destination clause)))))
  (:compile (destination clause)
            (destructuring-bind (_ condition &rest body) clause
              (declare (ignore _))
              `(when ,condition
                 ,@(loop for clause in body
                      collect (compile-clause destination clause)))))
  (:documentation "Format iff condition is true"))

(define-format-operation join
  (:keywords (:+ :j :join :concat :slice))
  (:format (destination clause)
           (destructuring-bind (_ separator args &optional format) clause
             (declare (ignore _))
             (multiple-value-bind (separator last-separator)
                 (if (listp separator)
                     (values (first separator)
                             (second separator))
                     (values separator separator))
               (when args
                 (let ((arg (first args)))
                   (if format
                       (let ((*_* arg))
                         (format-clause destination format))
                       (format-clause destination arg))
                   (when (cdr args)
                     (loop
                        for arg in (butlast (cdr args))
                        do
                          (format-clause destination separator)
                          (if format
                              (let ((*_* arg))
                                (format-clause destination format))
                              (format-clause destination arg))
                        finally
                          (format-clause destination last-separator)
                          (let ((arg (car (last args))))
                            (if format
                                (let ((*_* arg))
                                  (format-clause destination format))
                                (format-clause destination arg))))))))))
  (:compile (destination clause)
            (destructuring-bind (_ separator args &optional format) clause
              (declare (ignore _))
              (alexandria:with-unique-names (arg)
                (alexandria:once-only (args)
                  (multiple-value-bind (separator last-separator)
                      (if (listp separator)
                          (values (first separator)
                                  (second separator))
                          (values separator separator))
                    `(when ,args
                       (let ((,arg (first ,args)))
                         ,(if format
                              `(let ((*_* ,arg))
                                 (format-clause ,destination ',format))
                              `(format-clause ,destination ,arg)))
                       (when (cdr ,args)
                         (loop
                            for ,arg in (butlast (cdr ,args))
                            do
                              (format-clause ,destination ,separator)
                              ,(if format
                                   `(let ((*_* ,arg))
                                      (format-clause ,destination ',format))
                                   `(format-clause ,destination ,arg))
                            finally
                              (format-clause ,destination ,last-separator)
                              (let ((,arg (car (last ,args))))
                                ,(if format
                                     `(let ((*_* ,arg))
                                        (format-clause ,destination ',format))
                                     `(format-clause ,destination ,arg)))))))))))
  (:documentation "Join with separator"))

(define-format-operation format
  (:keywords (:format))
  (:format (destination clause)
           (destructuring-bind (_ control-string &rest args) clause
             (declare (ignore _))
             (apply #'format destination control-string args)))
  (:compile (destination clause)
            (destructuring-bind (_ control-string &rest args) clause
              (declare (ignore _))
              `(format ,destination ,control-string ,@args)))
  (:documentation "Format using Common Lisp format function"))

(define-format-operation do
  (:keywords (:do))
  (:format (destination clause)
           (error "No interpreted :do operation"))
  (:compile (destination clause)
            (destructuring-bind (_ (var list) &body clauses) clause
              (declare (ignore _))
              `(loop
                  :for ,var :in ,list
                  :do
                  ,@(loop for clause in clauses
                       collect (compile-clause destination clause)))))
  (:documentation "Iteration operation"))

(define-format-operation escape
  (:keywords (:esc :escape))
  (:format (destination clause)
           (let ((*escape* t))
             (loop for clause in (rest clause)
                do (format-clause destination clause))))
  (:compile (destination clause)
            `(progn ,@(rest clause)))
  (:documentation "Escape clauses formatting"))

(define-format-operation radix
  (:keywords (:r :radix))
  (:format (destination clause)
           (destructuring-bind (_ arg &optional (n :cardinal)) clause
             (declare (ignore _))
             (etypecase n
               (integer
                (format destination (format nil "~~~AR" n)
                        arg))
               (symbol
                (ecase n
                  (:cardinal
                   (format destination "~R" arg))
                  (:ordinal
                   (format destination "~:R" arg))
                  (:roman
                   (format destination "~@R" arg))
                  (:old-roman
                   (format destination "~:@R" arg)))))))
  (:compile (destination clause)
            (destructuring-bind (_ arg &optional (n :cardinal)) clause
              (declare (ignore _))
              (etypecase n
                (integer
                 `(format ,destination ,(format nil "~~~AR" n)
                          ,arg))
                (symbol
                 (ecase n
                   (:cardinal
                    `(format ,destination "~R" ,arg))
                   (:ordinal
                    `(format ,destination "~:R" ,arg))
                   (:roman
                    `(format ,destination "~@R" ,arg))
                   (:old-roman
                    `(format ,destination "~:@R" ,arg)))))))
  (:documentation "Radix operation"))

(define-format-operation decimal
  (:keywords (:d :decimal))
  (:format (destination clause)
           (format destination "~D" (second clause)))
  (:compile (destination clause)
            `(format ,destination "~D" ,(second clause)))
  (:documentation "Decimal operation"))

(define-format-operation binary
  (:keywords (:b :binary))
  (:format (destination clause)
           (format destination "~B" (second clause)))
  (:compile (destination clause)
            `(format ,destination "~B" ,(second clause)))
  (:documentation "Binary operation"))

(define-format-operation octal
  (:keywords (:o :oct :octal))
  (:format (destination clause)
           (format destination "~O" (second clause)))
  (:compile (destination clause)
            `(format ,destination "~O" ,(second clause)))
  (:documentation "Octal operation"))

(define-format-operation hexadecimal
  (:keywords (:x :hex :hexadecimal))
  (:format (destination clause)
           (format destination "~X" (second clause)))
  (:compile (destination clause)
            `(format ,destination "~X" ,(second clause)))
  (:documentation "Hexadecimal operation"))

(define-format-operation float
  (:keywords (:f :float))
  (:format (destination clause)
           (destructuring-bind (_ float &rest args &key width digits scale
                                  overflowchar padchar) clause
             (declare (ignore _))
             (let ((control-string (format nil "~~~{~A~^,~}F" (remove-if #'null args))))
               (format destination control-string float))))
  (:compile (destination clause)
            (destructuring-bind (_ float &key width digits scale
                                   overflowchar padchar) clause
              (declare (ignore _))
              (let ((args (list width digits scale overflowchar padchar)))
                (let ((control-string (format nil "~~~{~A~^,~}F" (remove-if #'null args))))
                  `(format ,destination ,control-string ,float)))))
  (:documentation "Float operation"))

(defun collect-then-and-else (clauses)
  (let ((then
         (loop
            for clause = (pop clauses)
            while (and clause (not (equalp clause :else)))
            collect clause)))
    (values then clauses)))

(define-format-operation if
  (:keywords (:if))
  (:format (destination clause)
           (error "No interpreted :if operation"))
  (:compile (destination clause)
            (destructuring-bind (_ condition &body clauses) clause
              (declare (ignore _))
              (multiple-value-bind (then-clauses else-clauses)
                  (collect-then-and-else clauses)
                `(if ,condition
                     (progn
                       ,@(loop for clause in then-clauses
                            collect (compile-clause destination clause)))
                     (progn
                       ,@(loop for clause in else-clauses
                            collect (compile-clause destination clause)))))))
  (:documentation "Conditional operation"))

(define-format-operation times
  (:keywords (:times))
  (:format (destination clause)
           (destructuring-bind (_ what n) clause
             (declare (ignore _))
             (dotimes (i n)
               (format-clause destination what))))
  (:compile (destination clause)
            (destructuring-bind (_ what n) clause
              (declare (ignore _))
              (alexandria:with-unique-names (i)
                `(dotimes (,i ,n)
                   ,(compile-clause destination what)))))
  (:documentation "Format repeatedly"))

#+nil(defun describe-format-operation (format-operation &optional (stream *standard-output*))
       (with-fmt (stream)
         (format-operation-name format-operation) " FORMAT OPERATION"
         #\newline
         "Keywords: " (:join ", " (format-operation-keywords format-operation)
                             (:s :_))
         #\newline
         #\newline
         (format-operation-documentation format-operation)))
