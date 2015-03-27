(asdf:defsystem #:fmt-test
  :description "Extensible format-like facility tests"
  :author "Mariano Montone"
  :license "MIT"
  :serial t
  :components ((:module :t
			:components ((:file "package")
				     (:file "tests"))
			:serial t))
  :depends-on (:fmt :fiveam)
  :perform (asdf:test-op (op c)
			 (uiop:symbol-call :fmt-test :run-tests)))
