(in-package :fmt)

(define-format-operation time
  (:keywords (:time))
  (:format (destination clause)
	   (destructuring-bind (_ timestamp &optional (format local-time:+iso-8601-format+)) clause
	       (declare (ignore _))
	       (let ((local-time (etypecase timestamp
				   (integer
				    (local-time:universal-to-timestamp timestamp))
				   (local-time:timestamp
				    timestamp))))
		 (local-time:format-timestring destination 
					       local-time
					       :format format))))
  (:compile (destination clause)
	    (destructuring-bind (_ timestamp &optional (format 'local-time:+iso-8601-format+)) clause
		(declare (ignore _))
		(alexandria:with-unique-names (local-time)
		  (alexandria:once-only (timestamp)
		  `(let ((,local-time (etypecase ,timestamp
					(integer
					 (local-time:universal-to-timestamp ,timestamp))
					(local-time:timestamp
					 ,timestamp))))
		     (local-time:format-timestring ,destination 
						   ,local-time
						   :format ,format))))))
  (:documentation "Time formatting"))
