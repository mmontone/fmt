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

Invoking
========

with-fmt
--------

.. cl:macro:: with-fmt

A macro that expands its body to formatting commands. 

``destination`` can be:

* NIL: formatting is done on a new string that is returned as result
* T: formatting is printed to ``*standard-output*``
* a stream: formatting is written to the stream

If no ``destination`` is given, then ``*fmt-destination*`` is the default destination.

Example:

.. code-block:: common-lisp

   (with-fmt ()
      "Hello world"
      #\newline
      (:join "," (list 1 2 3)))

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

Note that some control flow operations (like ``:do`` and ``:if`` are not available in interpretive mode).

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

Escaping (:esc)
---------------

Use the ``:esc`` directive for disabling formatting in a particular place.

For instance:

.. code-block:: common-lisp

     (fmt nil "Hello" #\space (:esc "beautiful" #\space) "world")

returns ``"hello world"``

It's important to note that the code inside :esc is not removed completly, it is executed, but its result is not formatted. You can see that in the macroexpansion of the above code:

.. code-block:: common-lisp
   
     (WITH-FMT-DESTINATION (#:STREAM925 NIL)
       (MACROLET ((EMB (&REST CLAUSES)
                 `(FMT ,'#:STREAM925 ,@CLAUSES)))
       (WRITE-STRING "Hello" #:STREAM925)
       (WRITE-CHAR #\  #:STREAM925)
       (PROGN "beautiful" #\ )
       (WRITE-STRING "world" #:STREAM925)))

This is useful in combination with the ``emb`` directive:

.. code-block:: common-lisp

     (fmt nil 
          (:a "start")
	  #\newline
	  (:esc 
	    (loop for x in (list 1 2 3)
	 	do (emb (:s x))))
	  #\newline
	  (:a "end"))

In the above example the output of the loop is not formatted as it is enclosed in an ``:esc``; but the ``emb`` operation inside the loops makes sure each of the elements of the list is formatted.


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
