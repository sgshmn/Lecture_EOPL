# EOPL in Haskell

 - [Essentials of Programming Languages](https://github.com/mwand/eopl3) 
 - Rewritten in [Haskell](https://www.haskell.org/)

### Chapter 1: Inductive Sets of Data

 - Recursively defined data types


### Chapter 2: Data Abstraction

 - Environment (representation)

 - Abstract syntax and its representation


### Chapter 3: Expressions

 - LET: A expression language with local bindings

 - PROC: A language with procedures (based on LET)

 - LETREC: A language with recursive procedures (based on PROC)


### Chapter 4: State

 - EXPLICIT-REFS: A language with explicit references (based on LETREC)

 - IMPLICIT-REFS: A language with implicit references (based on LETREC)

### Chapter 5: Continuation-passing interpreters

 - LETREC-CPS: Reimplement LETREC with a continuation-based interpreter (based on LETREC)

 - EXCEPTIONS: A language with exception handling (based on LETREC-CPS)

 - THREADS: A multi-threaded concurrent language (based on LETRE-CPS and IMPLICIT-REFS)

### Chapter 7: Types (INFERRED Not Available Yet)

 - CHECKED: A type-checked language (based on LETREC)
 - INFERRED: A language with type inference (based on CHECKED)

### Chapter 8: Modules (OPAQUE-TYPES and PROC-MODULES Not Available Yet)

 - SIMPLE-MODULES: A modular language (based on CHECKED)
 - OPAQUE-TYPES: A modular language with interfaces, i.e., module types (based on SIMPLE-MODULES)
 - PROC-MODULES: A modular language with parameterized modules (based on OPAQUE-TYPES)

### Chapter 9: Objects and Classes (TYPED-OO Not Available Yet)

 - CLASSES: An untyped object-oriented language (based on IMPLICIT-REFS)
 - TYPED-OO: A typed object-oriented language (based on CLASSES)

