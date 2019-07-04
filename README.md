# WITH-

WITH- is a Swiss-Army-Knife macro that attempts to make CL code more consice and regular.

The most obvious benefit it provides is the ability to group any number of existing with- macros together, avoiding deep indentation:
```
(with-
    (:open-file (s ...))    
    (:output-to-string .))
  ...))
```
WITH- unifies the syntax for structs, classes, as well as foreign CFFI objects -- automatically extracting and rebinding slot accessors.
```
(with- (:temp :int p)                                  ; like with-foreign-object
       (:temp (:struct gtk:g-point) gpt "P1-")         ;
       (:old 'q:spoint spt "P2-")                      ; use existing instance
       (:new 'graphics:point ppp "" (h hor)(v ver))    ; ppp gets a new instance
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

Clone the repo into a visible directory, use (ql:quickload "CL-AVEC") or ASDF magic.

This library requires CFFI and CLOSER-MOP

## Usage

`(with- descriptor body)` or `(with- (descriptor1 ...) body)` 

Each descriptor is a list.  It may start with a keyword version of any existing with- macro, with the 'with-' prefix removed.  WITH-OPEN-FILE must be written as  ```(with- (:open-file ...)```  In this case, the rest of the list contains whatever the original macro expects.

A descriptor may also be in the form of:

`(:new|:temp|:old type instance [prefix] [bindings]`

```
disposition  :new  to create a new object and bind to 'instance'
             :temp as above, but destroyed on exit if foreign
			 :old  to use an existing, bound 'instance'
			 
type         A quoted symbol signifying struct or class name
             A symbol whose symbol-value is an actual <TYPE-CLASS>
			 A keyword denoting a simple CFFI type such as :int
			 A list representing a cffi type such as (:STRUCT ...)
			 
instance     A symbol, bound to an existing object if :old, or to be
              bound to a newly created object if :new or :temp

prefix       A string to be appended to all slot accessors for this instance
             Optional; default is ""

bindings     A list of bindings in the format acceptable to 'with-slots' or 
             'with-foreign-slots', as appropriate.  Optional; defaults to
			 automatic extraction and rebinding of all slots
```



			 


			 

			 
			 
