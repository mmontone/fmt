.. FMT documentation master file, created by
   sphinx-quickstart on Sat Mar 28 11:07:32 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

.. cl:package:: fmt

Welcome to FMT's documentation!
===============================

..
   Contents:

   .. toctree::
      :maxdepth: 2

Overview
========

**FMT** is an extensible text formatting facility for Common Lisp. It is meant to do the same things Common Lisp **FORMAT** function does, but instead of using a control-string for formatting directives, it uses s-expressions.

Invocation
==========

with-fmt
--------

.. cl:macro:: with-fmt

A macro that expands its body to formatting commands. 

``destination`` can be:

* NIL: formatting is done on a new string that is returned as result
* T: formatting is printed to ``*standard-output*``
* a stream: formatting is written to the stream

If no ``destination`` is given, then ``*fmt-destination*`` is the default destination.

Body forms are either some lisp object, like strings, numbers, characters, etc; or some formatting operation. A formatting operation has the form of a list beggining with the operation keyword, like ``(:aesthetic message)``, ``(:join "," list)``, etc. Forms appearing in body are formatted one after the other.

Example:

.. code-block:: common-lisp

   (with-fmt ()
      "Hello world"
      #\newline
      (:join "," (list 1 2 3)))

prints::

   Hello world
   1,2,3

fmt
---

.. cl:macro:: fmt

This macro is almost exactly to ``with-fmt``, but with a different syntax, with makes it look very similar to Common Lisp ``format`` function.

Example:

.. code-block:: common-lisp

   (fmt nil "Hello" \#space "world")
   (fmt t (:s (list 1 2 3)))

fmt*
----

.. cl:function:: fmt*

Both ``fmt`` and ``with-fmt`` are macros and compile formatting directives to code that writes on the destination stream at compile-time. ``fmt*`` is a function, and interprets the formatting clauses given to it at run-time. This can be useful if you need a function to pass around, or you need extra flexibility in the formatting spec form.

Example:

.. code-block:: common-lisp

     (fmt* nil "Hello" #\space "world")
     (fmt* nil (if t `(:d ,22) `(:f ,23.44)))

Note that some control flow operations (like ``:do`` and ``:if`` are not available in interpreted mode).

Printer operations
==================

Aesthetic (:a, :aesthetic)
--------------------------

The aesthetic operation is the equivalent of Common Lisp `FORMAT's ~A <http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cda.htm>`_ directive.

Example:

.. code-block:: common-lisp

     (fmt nil (:a (list :foo :bar :baz)))

returns ``"(FOO BAR BAZ)"``

Standard (:s, :std, :standard)
------------------------------

The standard operation is the equivalent of Common Lisp `FORMAT's ~S <http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cdb.htm>`_ directive.

Example:

.. code-block:: common-lisp

   (fmt nil (:s (list :foo :bar :baz)))

returns ``"(:FOO :BAR :BAZ)"``


Special operations:
===================

Escaping (:esc and :fmt)
------------------------

Use the ``:esc`` directive for disabling formatting in a particular place.

For instance:

.. code-block:: common-lisp

     (fmt nil "Hello" #\space (:esc "beautiful" #\space) "world")

returns ``"hello world"``

It's important to note that the code inside :esc is not removed completly, it is executed, but its result is not formatted. You can see that in the macroexpansion of the above code:

.. code-block:: common-lisp
   
     (WITH-FMT-DESTINATION (#:STREAM925 NIL)
       (MACROLET ((:FMT (&REST CLAUSES)
                 `(FMT ,'#:STREAM925 ,@CLAUSES)))
       (WRITE-STRING "Hello" #:STREAM925)
       (WRITE-CHAR #\  #:STREAM925)
       (PROGN "beautiful" #\ )
       (WRITE-STRING "world" #:STREAM925)))

This is useful in combination with the ``:fmt`` directive, that reenables formatting inside escaped forms:

.. code-block:: common-lisp

     (fmt nil 
          (:a "start")
	  #\newline
	  (:esc 
	    (loop for x in (list 1 2 3)
	 	do (:fmt (:s x))))
	  #\newline
	  (:a "end"))

In the above example the output of the loop is not formatted as it is enclosed in an ``:esc``; but the ``:fmt`` operation inside the loops makes sure each of the elements of the list is formatted.

Control flow operations
=======================

Conditional (:when and :if)
---------------------------

Conditional control flow can be controlled via ``:when`` and ``:if`` operations.

``:when`` is the simplest of the two and executes its body when the condition given is true.

Syntax::
  
  (:when <condition> &body body)

Example:

.. code-block:: common-lisp

   (let ((cond t))
       (fmt nil (:when cond "yes"))) ;=> "yes"

   (let ((cond nil))
    (fmt nil (:when cond "yes"))) ;=> ""

``:if`` has an ``else`` branch.

Syntax::

  (:if <condition> &body body)

The ``else`` branch is indicated with the ``:else`` keyword. 

Example:

.. code-block:: common-lisp

   (let ((list (list 1 2 3)))
     (is (equalp (fmt nil (:if (not list)
 	 		        "none"
			        :else
			        (:join "," list)))
		"1,2,3"))) ;=> "1,2,3"

    (let ((list (list)))
      (is (equalp (fmt nil (:if (not list)
			        "none"
			        :else
			        (:join "," list)))
		  "none"))) ;=> "none"

Note: ``:if`` is not implemented in interpreter mode, so it cannot be used in ``fmt*`` function.


Iteration (:do)
---------------

To iterate a list formatting its elements, there's the ``:do`` operation.

Syntax::
  
  (:do (<var> <list>) &body body)

Example:

.. code-block:: common-lisp

    (fmt nil (:do (item (list 1 2 3))
                  (:s item))) ;=> "123"

Note: ``:do`` is not implemented in interpreter mode, so it cannot be used in ``fmt*`` function.

Filters
=======

Radix control
=============

Radix (:r, :radix)
------------------

Prints argument in radix. Equivalent to `Common Lisp FORMAT's ~R <http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cba.htm>`_

Syntax::
  (:r <n> &optional (<interpretation> :cardinal))

``<interpretation>`` can be ``:cardinal``, ``:ordinal``, ``:roman`` and ``:old-roman``.

Examples:

.. code-block:: common-lisp
   
   (fmt nil (:r 4)) ;=> "four"
   (fmt nil (:r 4 :cardinal)) ;=> "four"
   (fmt nil (:r 4 :ordinal)) ;=> "fourth"
   (fmt nil (:r 4 2)) ;=> "100"
   (fmt nil (:r 4 :roman)) ;=> "IV"
   (fmt nil (:r 4 :old-roman)) ;=> "IIII"

Extending FMT
=============

Custom operations definition
----------------------------

Custom filters definition
-------------------------

Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
