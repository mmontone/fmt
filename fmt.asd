(asdf:defsystem #:fmt
  :description "Extensible format-like facility"
  :author "Mariano Montone"
  :license "MIT"
  :homepage "https://github.com/mmontone/fmt"             
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))              
  :serial t
  :components ((:file "package")
               (:file "fmt"))
  :depends-on (:alexandria)
  :in-order-to ((asdf:test-op (asdf:test-op :fmt-test))))

