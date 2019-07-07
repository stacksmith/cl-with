;;;; cl-with.lisp

(in-package #:with)


(defmacro with-this((&rest this) &body body)
  ;`(,@before (unwind-protect ,@body ,after ))
  `(,@this ,@body)
  )
(defparameter *banlist* nil)
;;============================================================================
(defmacro bancheck (inst descriptor)
  (declare (ignore inst descriptor))
  )
;; Type may be one of:
;; list specifying a cffi complex type like (:struct point)
;; keyword that stands for a cffi simple type like :int
;; symbol that stands for a class or structure type using (find-class)
;; an actual <TYPE> or <CLASS>.  Foreign classes (subtypep (type-of *) 'cffi::foreign-type)
;; Return (values <type> slots)

;; Remember, these are called during macroexpansion!
;;
;; (quote symbol) means and try again find class or structure;
;; (:struct ...) means parse cffi type and try again
(defmethod get-type-info ((type t) clause &optional recursing)
  (declare (ignore recursing))
  (error "with-: ~A is not a valid type for ~A" type clause))

(defmethod get-type-info ((type cons) clause &optional recursing )
  (declare (ignore clause recursing))
;;  (format t "~%get-type-info cons ~A" type)
  (let ((car (car type)))
    (if (symbolp car)       ; ( symbol ...
	(if (eq 'quote car) ; ( quote ??
	    (find-class (cadr type))
	    (if (keywordp car ) ;  (:struct ...) or something like that?
		(cffi::parse-type type)
		(let* ((withstring  (catstring "WITH-" car))
		       (withname (find-symbol-or-die
				  withstring *package*
				  "~A is not a valid WITH-macro" withstring)))
		  (cons  withname (cdr type) )
		  ))))))

;; symbol - get its value and try again.
(defmethod get-type-info ((type symbol) clause &optional recursing)
;;  (format t "~%get-type-info symbol ~A" type)
  (if (keywordp type)
      (cffi::parse-type type)
      (if recursing
	  (error "with: ~A does not specify a valid type for ~A"
		 type clause)
	  (get-type-info (or (symbol-value type)
			     (error "with-: ~A is not a valid foreign type of ~A"
				    type clause))
			 clause t))))

;; raw foreign type
(defmethod get-type-info ((type cffi::foreign-type) clause &optional recursing )
  (declare (ignore clause recursing))
;;  (format t "~%get-type-info foreign ~A" type)
  type)



;; raw structure-class
(defmethod get-type-info ((class structure-class) clause &optional recursing )
  (declare (ignore clause recursing))
;;  (format t "~%get-type-info structure-class ~A" class)
  class)

	
;; raw class class
(defmethod get-type-info ((class standard-class) clause &optional recursing )
  (declare (ignore clause recursing))
;;  (format t "~%get-type-info class ~A" class)
  class)

(defmethod get-slots ((class class) )
    (mapcar #'c2mop::slot-definition-name 
	    (c2mop:class-slots (c2mop:ensure-finalized class))))

(defmethod get-slots ((cffitype cffi::foreign-type))
  (cffi::foreign-slot-names cffitype))

;;------------------------------------------------------------------------------
;; FIX-LISP-BINDINGS
;;
;; Starting with a list of proposed bindings, prefix names and verify slotnames.
;; Proposed bindings are with-slots-style.  Rules are:
;;       FROM                      TO
;; A                         (fixed-name-A  fixed-slot-A)
;; (A B)                     (fixed-name-A  fixed-slot-B)
(defun fix-lisp-bindings (prefix binds slots)
  (flet ((fix-name (name)
	   (let ((prefixed (symbolicate prefix name)))
	     (bancheck prefixed binds)
	     prefixed))
	 (fix-slot (proposed)
	   (or (find (symbol-name proposed) slots :key #'symbol-name
		     :test #'string=)
	       (error "Bad slot ~A" proposed))))
    (loop for binding in binds
       collect (if (listp binding)
		   (let ((bname (first binding))
			 (bslot (second binding)))
		     (case (length binding)
		       (1 (list (fix-name bname) (fix-slot bname)))
		       (2 (list (fix-name bname) (fix-slot bslot)))
		       (t (error "malformed binding ~A" binding))))
		   (list (fix-name binding) (fix-slot binding))))))
;;------------------------------------------------------------------------------
;; FIX-CFFI-BINDINGS
;;
;; Starting with a list of proposed bindings, prefix names and verify slotnames.
;; Proposed bindings are CFFI with-foreign-slots-style.  Rules are:
;;       FROM                      TO
;; A                         (fixed-name-A  fixed-slot-A)
;; (:pointer A)              (fixed-name-A :pointer fixed-slot-A)
;; (A B)                     (fixed-name-A  fixed-slot-B)
;; (A :pointer B)            (fixed-name-A :pointer fixed-slot-B)
;; 
(defun fix-cffi-bindings (prefix binds slots)
  (flet ((fix-name (name)
	   (let ((prefixed (symbolicate prefix name)))
	     (bancheck prefixed binds)
	     prefixed))
	 (fix-slot (proposed)
	   (or (find (symbol-name proposed) slots :key #'symbol-name
		     :test #'string=)
	       (error "Bad slot ~A in ~A" proposed slots))))
    (loop for binding in binds
       collect (if (listp binding)
		   (let ((p1 (first binding))
			 (p2 (second binding))
			 (p3 (third binding)))
		     (case (length binding)
		       (1 (list (fix-name p1) (fix-slot p1)))
		       (2 (if (eq :pointer p1)
			      (list (fix-name p2) :pointer (fix-slot p2))
			      (list (fix-name p1) (fix-slot p2))))
		       (3 (unless (eq :pointer p2)
			    (error "malformed binding ~A" binding))
			  (list (fix-name p1) (fix-slot p3)))
		       (t (error "malformed binding ~A" binding))))
		   (list (fix-name binding) (fix-slot binding))))))
;;------------------------------------------------------------------------------
;; DEFAULT-BINDINGS
;;
;; Starting with slots, create a list of bindings, prefixing names in-package.
;; Since slots is authoritative, no checking is needed.
;;
(defun default-bindings (prefix slots)
  (loop for slotsym in slots
     collect
       (let ((name (symbolicate prefix slotsym)))
	 (bancheck name slots)
	 (list name slotsym))))

(defun clause-get-prefix-binds (params)
  (let ((p1 (caar params))
	(p2 (cdar params)))
      (let ((prefix (if (stringp p1) p1 ""))
	  (binds (if (stringp p1) p2 (car params))))
	(values prefix binds))))

;;==============================================================================
;; GET-OLD-CLAUSE
;;
(defmethod get-old-clause (inst (class class) body &rest rest)

  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots class))
	   (fixed-binds (if binds
			    (fix-lisp-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(with-slots ,fixed-binds ,inst
	 ,@body))))

(defmethod get-old-clause (inst (cffitype cffi::foreign-type) body &rest rest)
  (let ((unparsed-type (cffi::unparse-type cffitype)))
    (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
      (let* ((slots (get-slots cffitype))
	     (fixed-binds (if binds
			      (fix-cffi-bindings prefix binds slots)
			      (default-bindings prefix slots))))
	`(with-foreign-slots (,fixed-binds ,inst ,unparsed-type) 
	   ,@body)))))

(defmethod get-old-clause (inst (cffitype cffi::foreign-built-in-type) body &rest rest)
  (declare (ignore rest))
  `(progn
     ,@body))
;;==============================================================================
;; GET-NEW-CLAUSE
;;
(defmethod get-new-clause (inst (class class) body &rest rest)
  
  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots class))
	   (fixed-binds (if binds
			    (fix-lisp-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(let ((,inst (make-instance ,class)))
	 (with-slots ,fixed-binds ,inst
	   ,@body)))))

(defmethod get-new-clause (inst (cffitype cffi::foreign-type) body &rest rest)
  (let ((unparsed-type (cffi::unparse-type cffitype)))
    (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
      (let* ((slots (get-slots cffitype))
	     (fixed-binds (if binds
			      (fix-cffi-bindings prefix binds slots)
			      (default-bindings prefix slots)))
	     )
	`(let ((,inst (foreign-alloc ',unparsed-type)))
	   (with-foreign-slots (,fixed-binds ,inst ,unparsed-type) 
	     ,@body))))))
;; For built-in cffi types, such as :int, we unparse the raw <type> back to
;; :int form, since foreign-alloc chokes _sometimes_ (:initial_element...)
;; on the real type!
;; Note: do not quote the simple type
(defmethod get-new-clause (inst (cffitype cffi::foreign-built-in-type) body &rest rest)
  (let ((unparsed-type (cffi::unparse-type cffitype)))
    
    `(let ((,inst (foreign-alloc ,unparsed-type ,@(car rest))))
       ,@body)))

;;==============================================================================
;; GET-TEMP-CLAUSE
;;
(defmethod get-temp-clause (inst (class class) body &rest rest)
  
  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots class))
	   (fixed-binds (if binds
			    (fix-lisp-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(let ((,inst (make-instance ,class)))
	 (with-slots ,fixed-binds ,inst
	   ,@body)))))

(defmethod get-temp-clause (inst (cffitype cffi::foreign-type) body &rest rest)
  (let ((unparsed-type (cffi::unparse-type cffitype)))
    
    (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
      (let* ((slots (get-slots cffitype))
	     (fixed-binds (if binds
			      (fix-cffi-bindings prefix binds slots)
			      (default-bindings prefix slots))))
	`(with-foreign-object (,inst ',unparsed-type)
	   (with-foreign-slots (,fixed-binds ,inst ,unparsed-type) 
	     ,@body))))))

(defmethod get-temp-clause (inst (cffitype cffi::foreign-built-in-type) body &rest rest)
  `(with-foreign-object (,inst ,@(cffi::unparse-type cffitype) ,@(car rest))
      ,@body))

;;===============================================================================
(defmacro with-one (descriptor  &body body)
  (let ((p1 (first descriptor))
	(p2 (second descriptor))
	(p3 (third descriptor))
	(params (cdddr descriptor)))
    (if (keywordp p1) ; (:output-to-file
	(let ((withname (find-symbol-or-die
			 (catstring "WITH-" p1) *package*
			 "~A is not a valid WITH- symbol" )))
	  `(,withname ,@(cdr descriptor) ,@body))
	(if (symbolp p1) ;; (x :old type parms
	    (case p2
	      ((:old :existing)
	       (bancheck p1 descriptor)
	       (get-old-clause p1 (get-type-info p3 descriptor) body params))
	      (:new
	       (bancheck p1 descriptor)
	       (get-new-clause p1 (get-type-info p3 descriptor) body params))
	      (:temp
	       (bancheck p1 descriptor)
	       (get-temp-clause p1 (get-type-info p3 descriptor) body params))
	      (t  `(let ((,p1 ,@(cdr descriptor)))
		     ,@body)))
	    (if (listp p1) ; ((x y).. = mvb
		`(multiple-value-bind ,p1 ,@(cdr descriptor) ,@body)
		(error "Invalid WITH- descriptor ~A" descriptor))))))

;;==============================================================================
(defmacro with-many ( (descriptor &rest descriptors) &body body)
  `(with-one ,descriptor
     ,(if descriptors
	  `(with-many (,(car descriptors) ,@(cdr descriptors))
	     ,@body)
	  `(progn ,@body))))

;;==============================================================================
(defmacro with- (descriptor-or-descriptors &body body)
  (setf *banlist* nil)
  (let ((descriptors
	 (if (consp (car descriptor-or-descriptors))
	     descriptor-or-descriptors
	     (list descriptor-or-descriptors))))
    `(let (*banlist*)
       (with-many (,(car descriptors) ,@(cdr descriptors)) 
	 ,@body))))



(defstruct point x y)
(defcstruct (cpoint :class cpoint)
  (x :int)
  (y :int))

(defparameter cpoint (cffi::parse-type '(:struct cpoint)))



