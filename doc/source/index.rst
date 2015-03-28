.. FMT documentation master file, created by
   sphinx-quickstart on Sat Mar 28 11:07:32 2015.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to FMT's documentation!
===============================

Contents:

.. toctree::
   :maxdepth: 2

Overview
========

**FMT** is an extensible text formatting facility for Common Lisp. It is meant to do the same things Common Lisp **FORMAT** function does, but instead of using a control-string for formatting directives, it uses s-expressions.

Invoking
========

.. cl:package:: fmt

.. cl:macro:: fmt

.. cl:function:: fmt*

.. cl:macro::with-fmt

Example:

.. code-bock:: common-lisp

   (fmt nil "Hello" #\space "world")

Printer operations
==================

Aesthetic (:a)
--------------

The aesthetic operation is the equivalent of Common Lisp `FORMAT's ~A <http://www.lispworks.com/documentation/lw50/CLHS/Body/22_cda.htm>`_ directive.

Example:

.. code-block:: common-lisp

   (fmt nil (:a (list :foo :bar :baz)))


Special operations:
===================

Escaping (:esc)
---------------

Use the `:esc` directive for disabling formatting in a particular place.

For instance:

.. code-bock:: common-lisp

   (fmt nil "Hello" #\space (:esc "beautiful" #\space) "world")

prints `"hello world"`

It's important to note that the code inside :esc is not removed completly, it is executed, but its result is not formatted. You can see that in the macroexpansion of the above code:

.. code-block:: common-lisp
   
   (WITH-FMT-DESTINATION (#:STREAM925 NIL)
    (MACROLET ((EMB (&REST CLAUSES)
               `(FMT ,'#:STREAM925 ,@CLAUSES)))
    (WRITE-STRING "Hello" #:STREAM925)
    (WRITE-CHAR #\  #:STREAM925)
    (PROGN "beautiful" #\ )
    (WRITE-STRING "world" #:STREAM925)))

This is useful in combination with the `emb` directive:

.. code-block:: common-lisp

   (fmt nil 
        (:a "start")
	#\newline
	(:esc 
	   (loop for x in (list 1 2 3)
		do (emb (:s x))))
	#\newline
	(:a "end"))

In the above example the output of the loop is not formatted as it is enclosed in an `:esc`; but the `emb` operation inside the loops makes sure each of the elements of the list is formatted.


Indices and tables
==================

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`

