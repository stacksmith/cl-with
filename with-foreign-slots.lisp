(in-package :with)

(defmacro with-foreign-slots ((vars ptr type) &body body)
  "Create local symbol macros for each var in VARS to reference
foreign slots in PTR of TYPE. Similar to WITH-SLOTS.
Each var can be of the form: 
  name                       name bound to slot of same name              
  (:pointer name)            name bound to pointer to slot of same name
  (name slot-name)           name bound to slot-name
  (name :pointer slot-name)  name bound to pointer to slot-name"

  (let ((ptr-var (gensym "PTR")))
    `(let ((,ptr-var ,ptr))
       (symbol-macrolet
           ,(loop :for var :in vars
	       :collect
		 (if (listp var)
		     (let ((p1 (first var)) (p2 (second var)) (p3 (third var)))
		       (if (eq p1 :pointer)	
			   `(,p2 (foreign-slot-pointer ,ptr-var ',type ',p2))
			   (if (eq p2 :pointer)
			       `(,p1 (foreign-slot-pointer ,ptr-var ',type ',p3))
			       `(,p1 (foreign-slot-value ,ptr-var ',type ',p2)))))
		     `(,var (foreign-slot-value ,ptr-var ',type ',var))))
         ,@body))))
