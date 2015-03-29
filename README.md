FMT
===

**FMT** is an extensible text formatting facility for Common Lisp. It is
meant to do the same things Common Lisp **FORMAT** function does, but
instead of using a control-string for formatting directives, it uses
s-expressions.

Invocation
==========

with-fmt
--------

A macro that expands its body to formatting commands.

`destination` can be:

-   NIL: formatting is done on a new string that is returned as result
-   T: formatting is printed to `*standard-output*`
-   a stream: formatting is written to the stream

If no `destination` is given, then `*fmt-destination*` is the default
destination.

Body forms are either some lisp object, like strings, numbers,
characters, etc; or some formatting operation. A formatting operation
has the form of a list beggining with the operation keyword, like
`(:aesthetic message)`, `(:join "," list)`, etc. Forms appearing in body
are formatted one after the other.

Example:

    (with-fmt ()
       "Hello world"
       #\newline
       (:join "," (list 1 2 3)))

prints:

    Hello world
    1,2,3

fmt
---

This macro is almost exactly to `with-fmt`, but with a different syntax,
with makes it look very similar to Common Lisp `format` function.

Example:

    (fmt nil "Hello" \#space "world")
    (fmt t (:s (list 1 2 3)))

fmt\*
-----

Both `fmt` and `with-fmt` are macros and compile formatting directives
to code that writes on the destination stream at compile-time. `fmt*` is
a function, and interprets the formatting clauses given to it at
run-time. This can be useful if you need a function to pass around, or
you need extra flexibility in the formatting spec form.

Example:

    (fmt* nil "Hello" #\space "world")
    (fmt* nil (if t `(:d ,22) `(:f ,23.44)))

Note that some control flow operations (like `:do` and `:if` are not
available in interpreted mode).

Printer operations
==================

Aesthetic (:a, :aesthetic)
--------------------------

The aesthetic operation is the equivalent of Common Lisp [FORMAT's
\~A](http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cda.htm)
directive.

Example:

    (fmt nil (:a (list :foo :bar :baz)))

returns `"(FOO BAR BAZ)"`

Standard (:s, :std, :standard)
------------------------------

The standard operation is the equivalent of Common Lisp [FORMAT's
\~S](http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cdb.htm)
directive.

Example:

    (fmt nil (:s (list :foo :bar :baz)))

returns `"(:FOO :BAR :BAZ)"`

Special operations
==================

Escaping (:esc and :fmt)
------------------------

Use the `:esc` directive for disabling formatting in a particular place.

For instance:

    (fmt nil "Hello" #\space (:esc "beautiful" #\space) "world")

returns `"hello world"`

It's important to note that the code inside :esc is not removed
completly, it is executed, but its result is not formatted. You can see
that in the macroexpansion of the above code:

    (WITH-FMT-DESTINATION (#:STREAM925 NIL)
      (MACROLET ((:FMT (&REST CLAUSES)
                `(FMT ,'#:STREAM925 ,@CLAUSES)))
      (WRITE-STRING "Hello" #:STREAM925)
      (WRITE-CHAR #\  #:STREAM925)
      (PROGN "beautiful" #\ )
      (WRITE-STRING "world" #:STREAM925)))

This is useful in combination with the `:fmt` directive, that reenables
formatting inside escaped forms:

    (fmt nil 
         (:a "start")
     #\newline
     (:esc 
       (loop for x in (list 1 2 3)
       do (:fmt (:s x))))
     #\newline
     (:a "end"))

In the above example the output of the loop is not formatted as it is
enclosed in an `:esc`; but the `:fmt` operation inside the loops makes
sure each of the elements of the list is formatted.

Control flow operations
=======================

Conditional (:when and :if)
---------------------------

Conditional control flow can be controlled via `:when` and `:if`
operations.

`:when` is the simplest of the two and executes its body when the
condition given is true.

Syntax:

    (:when condition &body body)

Example:

    (let ((cond t))
        (fmt nil (:when cond "yes"))) ;=> "yes"

    (let ((cond nil))
     (fmt nil (:when cond "yes"))) ;=> ""

`:if` has an `else` branch.

Syntax:

    (:if condition &body body)

The `else` branch is indicated with the `:else` keyword.

Example:

    (let ((list (list 1 2 3)))
      (fmt nil (:if (not list)
                     "none"
             :else
             (:join "," list)))) ;=> "1,2,3"

     (let ((list (list)))
       (fmt nil (:if (not list)
              "none"
              :else
              (:join "," list)))) ;=> "none"

Note: `:if` is not implemented in interpreter mode, so it cannot be used
in `fmt*` function.

Iteration (:do)
---------------

To iterate a list formatting its elements, there's the `:do` operation.

Syntax:

    (:do (var list) &body body)

Example:

    (fmt nil (:do (item (list 1 2 3))
                  (:s item))) ;=> "123"

Note: `:do` is not implemented in interpreter mode, so it cannot be used
in `fmt*` function.

Repetition (:times)
-------------------

Repeat formatting N number of times

Syntax:

    (:times clause n)

Example:

    (fmt nil (:times #\newline 5))

More complex control flow
-------------------------

Just use lisp with `:esc` and `:fmt` for more complex control flow.

Example:

    (let ((list (list 1 2 3)))
      (fmt nil (:esc (if (not list)
                       (:fmt "No elements")
                       (loop for x in (butlast list)
                             do (:fmt (:a x) "; ")
                             finally (:fmt (:a (car (last list)))))))))           

Other operations
================

Join (:join)
------------

Joins the elements of its list argument using a separator.

Syntax:

    (:join separator list &optional format)

`separator` can be either be a character or a string. `list` is of
course the list of elements to join. `format`, if present, is a command
for formatting the list elements. If it is not present `:s` is used. `_`
is bound to the list element.

Example:

    (fmt nil (:join ", " (list "foo" "bar" "baz"))) ;=> "foo, bar, baz"
    (fmt nil (:join #\, (list "foo" "bar"))) ;=> "foo,bar"
    (fmt nil (:join (", " " and ")
                 (list "foo" "bar" "baz"))) ;=> "foo, bar and baz"
    (fmt nil (:join ", " (list "a" "b" "c") (:a _ :up))) ;=> "A, B, C"

Common Lisp format (:format)
----------------------------

It is possible to just invoke Common Lisp format function to write on
the current destination.

Syntax:

    (:format control-string &rest args)

Example:

    (let ((list (list "foo" "bar" "baz")))
     (fmt nil (:format "~{~A~^, ~}" list))) ;=> "foo, bar, baz"

    (let ((list (list :foo :bar :baz)))
     (fmt nil (:format "~{~S~^, ~}" list))) ;=> ":FOO, :BAR, :BAZ"

Filters
=======

Filters are particular operations or functions that modify the input
before it gets formatted.

aesthetic and standard operations support filters.

Filters appear at the end of the `:a` or `:s` operations:

    (:a arg &rest filters)
    (:s arg &rest filters)

Filters can be either a function reference or some previously defined
filter.

Example:

    (fmt nil (:a "foo" :upcase)) ;=> "FOO"
    (fmt nil (:s "foo" #'string-upcase)) ;=> "FOO"
    (fmt nil (:a "  foo  " (:trim #\ ) :up)) ;=> "FOO"

Some very common filters are `:upcase` or `:up`, `:downcase` or `:down`,
`:trim`, etc

Radix control
=============

Radix (:r, :radix)
------------------

Prints argument in radix. Equivalent to [Common Lisp FORMAT's
\~R](http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cba.htm)

Syntax:

    (:r n &optional (interpretation :cardinal))

`interpretation` can be `:cardinal`, `:ordinal`, `:roman` and
`:old-roman`.

Examples:

    (fmt nil (:r 4)) ;=> "four"
    (fmt nil (:r 4 :cardinal)) ;=> "four"
    (fmt nil (:r 4 :ordinal)) ;=> "fourth"
    (fmt nil (:r 4 2)) ;=> "100"
    (fmt nil (:r 4 :roman)) ;=> "IV"
    (fmt nil (:r 4 :old-roman)) ;=> "IIII"

Extending FMT
=============

Custom formatting operations definition
---------------------------------------

Custom formatting operations can be defined via
`define-format-operation` macro.

It has the following syntax:

    (define-format-operation operation-name
       (:keywords keyword-list)
       (:format (destination clause)
             &body body)
       (:compile (destination clause)
             &body body)
       (:documentation docstring))

Where:

-   `keyword-list` is a list of keywords with which the formatting
    operation can be invoked.
-   `format` is the function that is run at run-time for formatting with
    `fmt*` function. `destination` is the current formatting
    destination, and `clause` is the whole format clause.
-   `compile` is the code transformation triggered at compile-time by
    `fmt` and `with-fmt` macros. It is expected to return a piece of
    code.
-   `documentation` is the operation description string.

For example, we can define a time formatting operation

    (define-format-operation time
        (:keywords (:time))
        (:format (destination clause)
         (destructuring-bind (_ timestamp &optional (format local-time:+iso-8601-format+)) clause
             (declare (ignore _))
             (let ((local-time (etypecase timestamp
                     (integer
                      (local-time:universal-to-timestamp timestamp))
                     (local-time:timestamp
                      timestamp))))
               (local-time:format-timestring destination 
                             local-time
                             :format format))))
        (:compile (destination clause)
          (destructuring-bind (_ timestamp &optional (format 'local-time:+iso-8601-format+)) clause
              (declare (ignore _))
              (alexandria:with-unique-names (local-time)
                (alexandria:once-only (timestamp)
                `(let ((,local-time (etypecase ,timestamp
                          (integer
                           (local-time:universal-to-timestamp ,timestamp))
                          (local-time:timestamp
                           ,timestamp))))
               (local-time:format-timestring ,destination 
                             ,local-time
                             :format ,format))))))
        (:documentation "Time formatting"))

And then we can use the new operation like this:

    (fmt* nil `(:time ,(get-universal-time)))

That goes through the operation's `:format` code.

    (fmt nil (:time (get-universal-time)))

Which transforms code using the operation's `:compile` code.

Custom filters definition
-------------------------

Filters are defined via `define-format-filter` macro, very similarly to
format operations.

Syntax:

    (define-format-filter filter-name
       (:keywords keyword-list)
       (:apply (arg)
             &body body)
       (:compile (arg)
             &body body)
       (:documentation docstring))

Where:

-   `keyword-list` is a list of keywords with which the filter can be
    applied.
-   `apply` is the function that is run at run-time for formatting with
    `fmt*` function. `arg` is the argument to which apply the filter.
-   `compile` is the code transformation triggered at compile-time by
    `fmt` and `with-fmt` macros. It is expected to return a piece of
    code.
-   `documentation` is the operation description string.

For example, the `:trim` filter is defined like this:

    (define-format-filter trim
       (:keywords (:trim))
       (:apply (arg &rest chars)
           (string-trim (or chars (list #\ )) arg))
       (:compile (arg &rest chars)
         (let ((chars-bag (or chars (list #\ ))))
           `(string-trim ',chars-bag ,arg)))
       (:documentation "String trim filter"))

Filters can be used in `:a` and `:s` operations afterwards:

    (fmt nil (:a "  hello  " :trim)) ;=> "hello"
    (fmt nil (:a "//hello" (:trim #\/))) ;=> "hello"