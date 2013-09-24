This document describes the coding standards. The standards have
evolved with the project but we have not systematically gone back to
fix the code so that it would pass newer versions of the standard. So
old code exists with some mistakes in them.

Also some may wonder why worry about spaces or indentation, etc. There
are two primary reasons:

#. Favoring diffs that show only relevant differences. A difference in
   trailing whitespace is (almost always) irrelevant. A difference in
   indentation that does not change the algorithm is also irrelevant.

#. It makes it easier for contributors to understand each other: No
   one wants to read this::

    function
    () { var i=0
      ;
       var q    =    15;
        for (; i < q; ++ i) { console.log(i); }

General
=======

As a baseline, all code must:

#. be consistently indented,

#. not contain trailing whitespace (as defined by git),

#. use only spaces for indentation.

Documentation
=============

All documenation must be:

#. spell-checked, including embedded API documentation (e.g. jsdoc, sphinx).

#. not contain dangling links.

JavaScript
==========

All JavaScript files must:

#. turn on strict mode (``'use strict;'``),

#. have a base indent of 4 spaces,

#. be documented using the latest release of jsdoc 3 (see wed's lib/wed/tree_updater.js as a model of what is expected),

#. pass a jshint check without warning or error.

#. as a general rule, JavaScript executed in the browser must not display anything to the JavaScript console. (Sometimes 3rd party libraries do display diagnostic messages to the console. Turn these off only to the extent that they can be turned off by calling the public API differently. If there is no way to turn them off through the API then there's nothing you can do.)

.. note:: There are some rare instances where it is allowable to have
          some sort of developer mode be turned on and use console.log
          to output diagnosis. This is to be used sparingly.

Python
======

All Python files must be:

#. PEP8-compliant (pass a ``pep8`` check).

#. pass through ``pylint`` without error or warning.

.. note:: This means using ``# pylint: disable=`` to turn off pylint
          when it is too eager. This might also mean using a
          ``pylintrc`` file at the top of the Python code tree to turn
          recurring false-positives.

#. documented using sphinx.

.. note:: There may be cases where an absence of documentation is
          tolerated.
