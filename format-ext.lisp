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

(defmethod format-clause (destination (clause string))
  (write-string clause destination))

(defmethod format-clause (destination (clause character))
  (write-char clause destination))

(defmethod format-clause (destination (clause cons))
  (format-operation destination
		    (first clause)
		    clause))

(defmethod format-clause (destination clause)
  (princ clause destination))

(defgeneric format-operation (destination operation clause)
  (:method (destination operation clause)
    (error "Invalid format operation: ~S" operation)))
  
(defmethod format-operation (destination (operation (eql :s)) clause)
  (prin1 (cadr clause) destination))

(defmethod format-operation (destination (operation (eql :a)) clause)
  (princ (cadr clause) destination))

(defmethod format-operation (destination (operation (eql :when)) clause)
  (destructuring-bind (_ condition &rest body) clause
    (when condition
      (loop for clause in body
	   do (format-clause destination clause)))))

(defmethod format-operation (destination (operation (eql :join)) clause)
  (destructuring-bind (_ separator args) clause
    (when args
      (loop 
	 for arg in (butlast args)
	 do 
	   (format-clause destination arg)
	   (format-clause destination separator)
	 finally (format-clause destination (car (last args)))))))

(defmethod compile-clause (destination (clause string))
  `(write-string ,clause ,destination))

(defmethod compile-clause (destination (clause character))
  `(write-char ,clause ,destination))

(defmethod compile-clause (destination (clause cons))
  (if (keywordp (first clause))
      (compile-operation destination
			 (first clause)
			 clause)
      clause))

(defmethod compile-clause (destination clause)
  `(princ ,clause ,destination))

(defgeneric compile-operation (destination operation clause)
  (:method (destination operation clause)
    (error "Invalid format operation: ~S" operation)))
  
(defmethod compile-operation (destination (operation (eql :s)) clause)
  `(prin1 ,(cadr clause) ,destination))

(defmethod compile-operation (destination (operation (eql :a)) clause)
  `(princ ,(cadr clause) ,destination))

(defmethod compile-operation (destination (operation (eql :when)) clause)
  (destructuring-bind (_ condition &rest body) clause
    `(when ,condition
       ,@(loop for clause in body
	    collect (compile-clause destination clause)))))

(defmethod compile-operation (destination (operation (eql :join)) clause)
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
