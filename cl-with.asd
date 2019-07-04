;;;; cl-avec.asd

(asdf:defsystem #:cl-with
  :description "WITH- group with- macros, allocate objects and rebind slots"
  :author "stacksmith <fpgasm@apple2.x10.mx>"
  :license  "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :depends-on (#:cffi #:closer-mop)
  :components ((:file "package")
	       (:file "util")
	       (:file "with-foreign-slots")
               (:file "with")))
