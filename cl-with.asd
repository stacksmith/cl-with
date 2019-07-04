;;;; cl-avec.asd

(asdf:defsystem #:cl-with
  :description "WITH- a runcible macro for bunching with- macros, allocating objects and rebinding slots"
  :author "stacksmith <fpgasm@apple2.x10.mx>"
  :license  "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
	       (:file "util")
	       (:file "with-foreign-slots")
               (:file "with")))
