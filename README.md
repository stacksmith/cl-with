# WITH-

WITH- is a universal macro that:
* groups with- style macros (to avoid deep indentation)
* uniformly manages structs, classes and CFFI objects
* unifies the syntax binding single and multiple values
### WITH- aggregation
The most visible benefit: grouping any number of existing with- macros together avoids deep indentation and makes complicated expressions simpler:
```
(with-
    (:open-file ( ...))             ;(with-open-file
	...
    ('mypkg::a-smaile .))           ;(mypkg::with-a-smile ...
  ...))
```
### BINDING
It is easy to bind one or more variables in a with- statement
```
(with- (...                  
        (i 9)                   ;like (let ((i 9))...)
        ((j k) (values 1 2))    ;like (multiple-value-bind (j k)(values 1 2)...)
        ...)
   ...)
```
### CLASS, STRUCT and CFFI object integration
More importantly, WITH- unifies the syntax for dealing with structs, classes, and foreign CFFI objects, extracting and rebinding slot accessors (automatically or selectively).  The syntax establishes a clear distinction between existing object, temporary objects and newly-created object that are expected to outlive the statement.  

Syntactically, it resembles a binding initialized with an existing object (`:OLD`), or newly-created object (`:NEW` or `:TEMP`).  CFFI objects created as `:TEMP` are destroyed at the end of the scope.
```
(defstruct spoint x y)
(with- (pt :new 'spoint)
  (setf x 10 y 20)  ;with- automatically bound slot accessors
  pt)

(with- (p   :temp :int)                              ; like with-foreign-object
       (gpt :temp (:struct gtk:g-point) "P1-")       ; prefixed: p1-x and p1-y 
       (spt :old 'q:spoint "P2-")                    ; existing instance, p2-x etc.
       (ppp :new 'graphics:point (h hor)(v ver))     ; rename graphics::hor to h, etc.
  (setf p1-x p2-x              ;note that bindings are package-local!
        p1-y p2-y)             ;and prefixed to differentiate multiple objects
  (setf h (+ p1-x p2-x)
        v (- p1-x 3))
  ppp) 
```  
  
* works with structs and classes, as well as foreign objects;
* automatically figures out and binds all slots by default;
* automatically rebinds package-local names regardless of where objects are defined;
* allows multiple instances to be processed;
* allows per-instance prefix to differentiate names;
* works with existing :old objects
* allocates :new objects when requested
* allocates :temp objects, destroying CFFI objects at the end

## License
BSD 3-clause License
## Installation
Clone the repo into a visible directory and evaluate `(ql:quickload "CL-WITH")` or equivalent ASDF magic. 

In your project, add system `CL-WITH` to the dependency list of your .asd file, and package `WITH` to your `defpackage` `:USE` line.
`
This library requires uses (and internally depends on) CFFI and CLOSER-MOP.

# Usage

`(with- clause(s) body)` 

A with- statement may contain a single clause or a list of clauses.  Each clause is a list.  Each clause has access to bindings created by all previous clauses, and may create additional bindings.

## WITH- macro aggregation

A clause may start with a shortened, keyword version of any existing with- macro, with the 'with-' prefix removed and : prepended).  The rest of the list contains whatever the original macro expects for its parameters.

Example:

```
(with- (:open-file (s "test.txt" :direction :output))
  (print s "Hello"))
```

If the with-macro is in another package and is not imported, instead of using a keyword, use a quoted symbol with the package specification:
```
(with- ('another-package:foo ...)..) ; same as (another-package:with-foo ...
```

## BINDING

A clause may start with a symbol, in which case it acts much like a let form.  Any list elements that follow will be construed as the initialization form.

A list of symbols acts as a `multiple-value-bind` form.

```
(with- ((q "test.txt")
        (:open-file (s q))
        ((found status) (find-symbol "FOO")))
  (format t "~A ~A ~A " s found status))
```

The initializers for such bindings are regular Lisp code (that is, they are not WITH- clauses).  Any symbols bound in previous clauses are available to initializers.  Needless to say, any local scopes established inside initializers are not visible to the main body.  That is:
```
(with ((j 100)
       (q (let ((invisible (+ j 99)))) ;j is visible here
            invisible)))
  (print q) ;199 - this is fine
  (print invisible); *** ERROR  invisible is not in scope!
)
```
## STRUCT, CLASS or CFFI instance manipulation

A clause may also bind a slotted object or a CFFI foreign object:

`(instance :new|:temp|:old type [prefix] [bindings]`

### INSTANCE
```
instance     A symbol, bound to an existing object if :old, or to be
             bound to a newly created object if :new or :temp
```

### DISPOSITION
```
disposition  :new  to create a new object and bind to 'instance'
             :temp as above, but destroyed on exit if foreign
			 :old  to use an existing, bound 'instance'
```
:NEW clauses create a new object of type specified by `type` and create a lexical binding  to the symbol `instance`.  Since `instance` is lexical, you must return or assign the object prior to leaving the scope if you need it later.  This is especially important for foreign objects, since no automatic deallocation takes place for objects created with the `:NEW` clause.

:TEMP clauses likewise create a new object of `type` and bind it to `inst`, but telegraph the intention to never use such an object outside the established scope.  CFFI objects are automatically destroyed; Lisp objects are abandoned to be garbage collected later.  You must not return or assign such objects outside the established scope.

:OLD clauses operate on existing objects - objects already bound to `inst`.  For slotted objects, bindings are created as usual.

### TYPE
```
type         A quoted symbol signifying struct or class name
             A symbol whose symbol-value is a type
			 A keyword denoting a simple CFFI type such as :int
			 A list representing a cffi type such as (:STRUCT ...)
```
The `TYPE` parameter specifies the type of the object for this clause.  Note that Lisp class and struct types are quoted, while CFFI types such as (:struct foo) are not quoted.

For the sake of brevity, a variable containing a CFFI type may be used - as an unquoted symbol.  

### PREFIX
```
prefix       A string to be appended to all slot accessors for this instance
             Optional; default is ""
```
Prefix should generally be capitalized unless you truly intend to use lowercase symbols.

### BINDINGS
```
bindings     One or more bindings in the format acceptable to 'with-slots' or 
             'with-foreign-slots', as appropriate.  Optional; defaults to
			 automatic extraction and rebinding of all slots in local package
```
If no bindings are specified, package-local bindings with names identical to the `type`'s slot names will be generated.  If one or more bindings are specified, only those bindings will be generated.

Bindings are always specified as package-local symbols; `WITH-` automatically deals with package issues.  

The usual binding syntax for each slot binding is one of:
```
name                
(name slotname)
```
CFFI slotted objects also allow binding pointers to slots using one of:
```
(:pointer name)
(name :pointer slotname)
```

## Example
```
(with- (p   :temp :int)                              ; like with-foreign-object
       (gpt :temp (:struct gtk:g-point) "P1-")       ; prefixed: p1-x and p1-y 
       (spt :old 'q:spoint "P2-")                    ; existing instance, p2-x etc.
       (ppp :new 'graphics:point "" (h hor)(v ver))  ; rename graphics::hor to h, etc.
(setf p1-x p2-x              ;note that bindings are package-local!
        p1-y p2-y)             ;and prefixed to differentiate multiple objects
  (setf h (+ p1-x p2-x)
        v (- p1-x 3))
  ppp) ```



			 


			 

			 
			 
