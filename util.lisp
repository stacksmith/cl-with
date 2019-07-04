(in-package :with)

(defmacro listify (thing)
  `(if (listp ,thing)
       ,thing
       (list ,thing)))
;;------------------------------------------------------------------------------
;; create a single string from a thing, converting named objects such as
;; packages, symbols and foreign-types into strings.
;; Anything else? format it!
;; 
;;
(defun as-string (thing)
  "Create a string from thing.  If thing is a list or tree, flatten"
  (typecase thing
    (string thing)
    (symbol (symbol-name thing))
    (package (package-name thing))
    (cffi::foreign-type  (symbol-name (cffitype-symbol thing)))
    (t (format nil "~S" thing)))  )

;;------------------------------------------------------------------------------
;; catstring    create a string from a bunch of things
;;         Macro to avoid evaluation, so (catstring x y z) works
(defun catstring (&rest things)
  "Convert every thing to a string, and return concatenation."
  (apply #'concatenate 'string (mapcar #'as-string things)))
;;
;; symbolicate - create a symbol from a bunch of things
(defun symbolicate (&rest things)
  (intern (apply #'catstring things)))

(defun find-symbol-or-die (name package &rest rest)
  (or (find-symbol name package)
      (apply #'error rest)))
