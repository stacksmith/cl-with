;;;; cl-with.lisp

(in-package #:with)

(defmacro assure-list2 (thing)
  (if (consp (car thing))
      thing
      (list thing)))

(defmacro with-this((&rest this) &body body)
  ;`(,@before (unwind-protect ,@body ,after ))
  `(,@this ,@body)
  )
(defmacro with-macro((&rest definition) &body body)
  ;`(,@before (unwind-protect ,@body ,after ))
  `(macrolet ((mac (body)
		,definition))
     (mac ,body)))

(defmacro with-var ((sym) &body body)
  `(let ((,sym ,@body))))

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

(defun clause-get-prefix-binds (params)
  (let ((p1 (caar params))
	(p2 (cadar params)))
    (let ((prefix (if (stringp p1) p1 ""))
	  (binds (if (stringp p1) p2 p1)))
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
  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots cffitype))
	   (fixed-binds (if binds
			    (fix-cffi-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(with-foreign-slots (,fixed-binds ,inst ,cffitype) 
	 ,@body))))

;;==============================================================================
;; GET-NEW-CLAUSE
;;
(defmethod get-new-clause (inst (class class) body &rest rest)
  (print rest)
  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots class))
	   (fixed-binds (if binds
			    (fix-lisp-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(let ((,inst (make-instance ,class)))
	 (with-slots ,fixed-binds ,inst
	   ,@body)))))

(defmethod get-new-clause (inst (cffitype cffi::foreign-type) body &rest rest)
  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots cffitype))
	   (fixed-binds (if binds
			    (fix-cffi-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(let ((,inst (foreign-alloc ,cffitype)))
	 (with-foreign-slots (,fixed-binds ,inst ,cffitype) 
	   ,@body)))))

(defmethod get-new-clause (inst (it list) body &rest rest)
  (print rest)
  `(let ((,inst (,@it
		 ,@(car rest) 
		 ,@body)))))
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
  (multiple-value-bind (prefix binds) (clause-get-prefix-binds rest)
    (let* ((slots (get-slots cffitype))
	   (fixed-binds (if binds
			    (fix-cffi-bindings prefix binds slots)
			    (default-bindings prefix slots))))
      `(with-foreign-object (,inst ,cffitype)
	 (with-foreign-slots (,fixed-binds ,inst ,cffitype) 
	   ,@body)))))
;;===============================================================================

(defmacro with-one-old ((inst type &rest params) &body body)
   (if (keywordp type)
      `(with-one-new-cffi-simple (,type ,inst ,@params) ,@body)
      ;; presumably it is a slotted type
      (let ((class (get-type-info type)))
	(let ((clause (get-old-clause inst class body params)))
	  `(progn
	     ,clause)))))

(defmacro with-one-new ((inst type &rest params) &body body)
  (if (keywordp type)
      `(with-one-new-cffi-simple (,type ,inst ,@params) ,@body)
      ;; presumably it is a slotted type
      (let ((class (get-type-info type)))
	(format t "[ ~%~A~%~A~%~A]~%" type (type-of type) class)
	(let ((clause (get-new-clause inst class body params)))
	  `(progn
	     ,clause)))))

(defmacro with-one-temp ((inst type &rest params) &body body)
  (if (keywordp type)
      `(with-one-temp-cffi-simple (,type ,inst ,@params) ,@body)
      ;; presumably it is a slotted type
      (let ((class (get-type-info type)))
	(let ((clause (get-new-clause inst class body params)))
	  `(progn
	     ,clause)))    
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
			    "~A is not a valid WITH- symbol" )))
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
  `(with-one (,(first descriptor) ,@(cdr descriptor))
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
    `(with-many (,(car descriptors) ,@(cdr descriptors)) 
       (progn
	 ,@body))))



(defstruct point x y)
(defun qqq (v)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type fixnum v))
  (multiple-value-bind (a b) v
    
    a)
