;;;; cl-with.lisp

(in-package #:with)
(defparameter *banlist* nil)

(defun ban (symbol)
  (unless (member symbol *banlist*)
    (setf *banlist* (push symbol *banlist*))))
(defun banned? (symbol)
  (member symbol *banlist*))
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
(defmethod get-type-info ((type cons) )
  (format t "~%get-type-info cons ~A" type)
  (if (eq 'quote (car type))
      (find-class (cadr type)) 
      (cffi::parse-type type) ))

;; symbol - get its value and try again.
(defmethod get-type-info ((type symbol)  )
  (format t "~%get-type-info symbol ~A" type)
  (get-type-info (symbol-value type) ))

;; raw foreign type
(defmethod get-type-info ((type cffi::foreign-type)  )
  (format t "~%get-type-info foreign ~A" type)
  type)



;; raw structure-class
(defmethod get-type-info ((class structure-class) )
  (format t "~%get-type-info structure-class ~A" class)
  class)

	
;; raw class class
(defmethod get-type-info ((class standard-class)  )
  (format t "~%get-type-info class ~A" class)
  class)

(defmethod get-slots ((class class))
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
       (list (symbolicate prefix slotsym) slotsym)))
;;==============================================================================
;; GET-OLD-CLAUSE
;;
(defmethod get-old-clause ((class class) prefix binds instance body)
  (let* ((slots (get-slots class))
	 (fixed-binds (if binds
			  (fix-lisp-bindings prefix binds slots)
			  (default-bindings prefix slots))))
    `(with-slots ,fixed-binds ,instance
      ,@body)))

(defmethod get-old-clause ((cffitype cffi::foreign-type) prefix binds instance body)
  (let* ((slots (get-slots cffitype))
	 (fixed-binds (if binds
			  (fix-cffi-bindings prefix binds slots)
			  (default-bindings prefix slots))))
    `(with-foreign-slots (,fixed-binds ,instance ,cffitype) 
      ,@body)))

;;==============================================================================
;; GET-NEW-CLAUSE
;;
(defmethod get-new-clause ((class class) prefix binds instance body)
  (let* ((slots (get-slots class))
	 (fixed-binds (if binds
			  (fix-lisp-bindings prefix binds slots)
			  (default-bindings prefix slots))))
    `(let ((,instance (make-instance ,class)))
       (with-slots ,fixed-binds ,instance
	   ,@body))))

(defmethod get-new-clause ((cffitype cffi::foreign-type) prefix binds instance body)
  (let* ((slots (get-slots cffitype))
	 (fixed-binds (if binds
			  (fix-cffi-bindings prefix binds slots)
			  (default-bindings prefix slots))))
    `(let ((,instance (foreign-alloc ,cffitype)))
       (with-foreign-slots (,fixed-binds ,instance ,cffitype) 
	 ,@body))))

;;==============================================================================
;; GET-TEMP-CLAUSE
;;
(defmethod get-temp-clause ((class class) prefix binds instance body)
   (let* ((slots (get-slots class))
	 (fixed-binds (if binds
			  (fix-lisp-bindings prefix binds slots)
			  (default-bindings prefix slots))))
    `(let ((,instance (make-instance ,class)))
       (with-slots ,fixed-binds ,instance
	 ,@body))))

(defmethod get-temp-clause ((cffitype cffi::foreign-type) prefix binds instance body)
  (let* ((slots (get-slots cffitype))
	 (fixed-binds (if binds
			  (fix-cffi-bindings prefix binds slots)
			  (default-bindings prefix slots))))
    `(with-foreign-object (,instance ,cffitype)
       (with-foreign-slots (,fixed-binds ,instance ,cffitype) 
	 ,@body))))
;;===============================================================================

(defmacro with-one-old ((type inst &rest params) &body body)
   (if (keywordp type)
      `(with-one-new-cffi-simple (,type ,inst ,@params) ,@body)
      ;; presumably it is a slotted type
      (let ((class (get-type-info type)))
	(multiple-value-bind (prefix binds) (if (stringp (car params))
						(values (car params) (cdr params))
						(values "" params))
	  (let ((clause (get-old-clause class prefix binds inst body)))
	    `(progn
	       ,clause))))))

(defmacro with-one-new ((type inst &rest params) &body body)
  (if (keywordp type)
      `(with-one-new-cffi-simple (,type ,inst ,@params) ,@body)
      ;; presumably it is a slotted type
      (let ((class (get-type-info type)))
	(multiple-value-bind (prefix binds) (if (stringp (car params))
						(values (car params) (cdr params))
						(values "" params))
	  (let ((clause (get-new-clause class prefix binds inst body)))
	    `(progn
	       ,clause)))
	)))

(defmacro with-one-temp ((type inst &rest params) &body body)
  (if (keywordp type)
      `(with-one-temp-cffi-simple (,type ,inst ,@params) ,@body)
      ;; presumably it is a slotted type
      (let ((class (get-type-info type)))
	(multiple-value-bind (prefix binds) (if (stringp (car params))
						(values (car params) (cdr params))
						(values "" params))
	  (let ((clause (get-new-clause class prefix binds inst body)))
	    `(progn
	       ,clause))))    
      )
)
;===============================================================================
(defmacro with-one ((dispo &rest rest)  &body body)
  (let ((new-params `((,(car rest) ,(cadr rest) ,@ (cddr rest)) ,@body)))
    (if (keywordp dispo)
	(case dispo
	  ((:old :existing) `(with-one-old ,@new-params))
	  (:new `(with-one-new ,@new-params))
	  (:temp `(with-one-temp ,@new-params))
	  (t
	   (let ((withname (find-symbol-or-die
			    (catstring "WITH-" dispo) *package*
			    "~A is not a valid WITH- symbol")))
	     `(,withname ,@rest ,@body))))))
  ;;  (mapcar #'ban params)
 #|| (if (not params)
    ;  (let ((instance-type (type-of instance))))
      )
  ||#)
  
  #||

  (format t "~%~A : BANLIST ~A" (car descriptors) *banlist*)
  
  (ban instance)
  (when (banned? instance)
  (error "WITH: clause ~A specifies symbol ~A which is already in use"
  (car descriptors) instance))
  ||#
(defmacro with-many ((descriptor &rest descriptors) &body body)
  (if descriptors
      `(with-one (,(first descriptor) ,(second descriptor) ,
		   (third descriptor) ,@(cdddr  descriptor))
	 (with-many (,(car descriptors) ,@(cdr descriptors))
	   ,@body))
      `(with-one (,(first descriptor) ,(second descriptor) ,
		   (third descriptor) ,@(cdddr  descriptor))
	 ,@body)))

(defmacro with- (descriptor-or-descriptors &body body)
  (setf *banlist* nil)
  (let ((descriptors (listify descriptor-or-descriptors)))
    `(with-many (,(car descriptors) ,@(cdr descriptors)) 
       ,@body)))

