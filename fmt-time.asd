(asdf:defsystem #:fmt-time
  :description "FMT time formatting extensions"
  :author "Mariano Montone"
  :license "MIT"
  :serial t
  :components ((:file "time"))
  :depends-on (:fmt :local-time))
