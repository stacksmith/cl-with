# cl-avec

AVEC is a universal macro for dealing with Common Lisp slotted objects.  It is essentially a WITH-SLOTS macro on steroids:
* works with structs and classes, as well as foreign objects;
* automatically figures out and binds all slots by default;
* automatically rebinds package-local names regardless of where objects are defined;
* allows multiple instances to be processed;
* allows per-instance prefix to differentiate names;
	

## License

BSD 3-clause License

## Installation

Clone the repo into a visible directory, use (ql:quickload "CL-AVEC") or ASDF magic.

## A Taste of AVEC

AVEC attempts to minimize the required parameters and infer as much as possible from them.  This results in a somewhat quirky - although common-sense - syntax.  Some examples:

```
(in-package :graphics)
(defstruct point x y)

(in-package :cl-user)
(setf *gpoint* (graphics::make-point :x 1 :y 1))

(avec (*gpoint*)                  ;avec automatically bound x and y
  (values x y))                   ;in this package (beats graphics::x graphics::y)
  
(let (my-point)
  (avec (my-point :new 'graphics::point (:x 1 :y 1)) 
    (print x))
  (avec (my-point "PT-")                ;prefix specified so bind PT-X and PT-Y
    (print pt-x)
	(print pt-y)))
	
(avec ((*gpoint* "OLD-")                       ; OLD-X and OLD-Y refer to *GPOINT*
       (pt2 :temp point (:x 1 :y 2) "NEW-"))   ; NEW-X and NEW-Y refer to new PT2
  (setf old-x new-x
        old-y new-y))

(defcstruct cpoint
  (x :int )
  (y :int ))

;; pretend that offset is a foreign :int object...
;;
(avec ((*gpoint* "OLD-" 
           :REBIND ((vertical y)))                    ; customize an accessor name
       (cpt :NEW cpoint "C-")                         ; a CFFI cstruct
	   offset)                                        ; a CFFI int pointer
  (setf c-x (+ *offset old-x)                         ; simple CFFI types have
        c-y (+ *offset old-vertical)                  ; value accessor with * prefix
  cpt)
```

## Usage

`(avec descriptor body)` or `(avec (descriptors) body)` 

Each descriptor is a list in the form:

`(instance [disposition] [prefix bindings]`

Only `instance` is required.  If `bindings` are provided, the `prefix` must be also specified; `disposition` is entirely optional.
```
instance     A symbol, either already bound to an existing object,
             or to be bound to a newly created object (see 'disposition').

disposition  A keyword or a list specifying what the instance is, or how it is to
             be created and dealt with.
			 
prefix       A string to be appended to all slot accessors for this instance

bindings     A list of bindings in the format acceptable to 'with-slots' or 
             'with-foreign-slots', as appropriate
```

### Disposition

AVEC works with a variety of types, including instances of CLOS classes, structs, and a variety of foreign CFFI objects.  The `disposition` parameter provides type-specific instructions.

When omitted, AVEC assumes that `instance` is already bound to a struct, class or foreign object instance.  

Disposition may be one of the keywords listed below; in such a case it must be followed by an appropriate clause as described:

#### :NEW type construction-data

AVEC will construct a new object and sets `instance` to it using `setf`.  Instance must specify a place that is setfable; the newly-created object will outlive the scope of AVEC.

The type may be any valid CLOS class, struct or CFFI foreign type

Construction-data must contain data appropriate for the type.

#### :TEMP type construction-data

AVEC will construct a temporary object and bind `instance` to it using `let`.  Foreign CFFI objects will be destroyed after the execution of `body`.

#### :EXISTING type

States that `instance` is alread




			 


			 

			 
			 
