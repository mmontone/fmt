(asdf:defsystem #:fmt
  :description "Extensible format-like facility"
  :author "Mariano Montone"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "fmt"))
  :depends-on (:alexandria))

