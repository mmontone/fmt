(in-package #:format-ext)

(defvar *format-destination* nil)

(defun call-with-format-destination (destination function &rest args)
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

(defmacro with-format-destination ((var &optional (destination *format-destination*))
				      &body body)
  `(call-with-format-destination ,destination
				 (lambda (,var)
				   ,@body)))

(defmacro with-format ((&optional (destination '*format-destination*))
		       &body body)
  (alexandria:with-unique-names (stream)
    `(with-format-destination (,stream ,destination)
       ,@(loop for clause in body
	    collect
	      (compile-clause stream clause)))))

(defun format* (&optional (destination *format-destination*) &rest clauses)
  (apply #'call-with-format-destination 
	 destination #'%format* clauses))

(defun %format* (destination &rest clauses)
  (loop for clause in clauses
       do (format-clause destination clause)))

(defvar *format-operations* (make-hash-table :test #'equalp))

(defstruct format-operation
  name
  keywords
  format
  compile
  documentation)

(defmacro define-format-operation (name &body options)
  (flet ((extract-option (name)
	   (cdr (find name options :key #'car))))
    (alexandria:with-unique-names (format-operation keyword)
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

(defmethod compile-clause (destination (clause (eql :newline)))
  `(terpri ,destination))

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
	     (prin1 (cadr clause) destination))
    (:compile (destination clause)
	      `(prin1 ,(cadr clause) ,destination))
    (:documentation "Standard print"))

(define-format-operation aesthetic
  (:keywords (:a :aesthetic))
  (:format (destination clause)
	   (princ (cadr clause) destination))
  (:compile (destination clause)
	    `(princ ,(cadr clause) ,destination))
  (:documentation "Aesthetic print"))

(define-format-operation when
  (:keywords (:w :when))
  (:format (destination clause)
	   (destructuring-bind (_ condition &rest body) clause
	     (when condition
	       (loop for clause in body
		  do (format-clause destination clause)))))
  (:compile (destination clause)
	    (destructuring-bind (_ condition &rest body) clause
	      `(when ,condition
		 ,@(loop for clause in body
		      collect (compile-clause destination clause)))))
  (:documentation "Format iff condition is true"))

(define-format-operation join
  (:keywords (:+ :j :join :concat :slice))
  (:format (destination clause)
	   (destructuring-bind (_ separator args) clause
	     (when args
	       (loop 
		  for arg in (butlast args)
		  do 
		    (format-clause destination arg)
		    (format-clause destination separator)
		  finally (format-clause destination (car (last args)))))))
  (:compile (destination clause)
	    (destructuring-bind (_ separator args) clause
	      (alexandria:with-unique-names (arg)
		(alexandria:once-only (args)
		  `(when ,args
		     (loop 
			for ,arg in (butlast ,args)
			do 
			  (format-clause ,destination ,arg)
			  (format-clause ,destination ,separator)
			finally (format-clause ,destination (car (last ,args)))))))))
  (:documentation "Join with separator"))

(defun describe-format-operation (format-operation &optional (stream *standard-output*))
  (with-format (stream) 
    (format-operation-name format-operation) " FORMAT OPERATION"
    :newline
    "Keywords: " (:join ", " (format-operation-keywords format-operation))
    :newline
    :newline
    (format-operation-documentation format-operation)))
