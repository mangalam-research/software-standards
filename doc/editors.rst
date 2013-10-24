This document describes how to set up your editor to *help* follow the
standards described here.

Only the editors that are in common use by people working on Mangalam
projects are described.

With some exceptions, only the languages currently used in Mangalam
projects are described.

=====
Emacs
=====

General Changes
===============

All Emacs configurations require this appearing at the start of your
``.emacs`` file::

    (require 'package)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    ;;(add-to-list 'package-archives
    ;;             '("melpa" . "http://melpa.milkbox.net/packages/"))
    (package-initialize)

This has to be before the customization variables because packages
installed by the packaging system must be known to emacs before their
variables are customized.

Melpa might be needed for some packages. However, it is not
recommended to keep it in the list of package-archives at all
times. The problem is that Melpa builds packages from what is on the
``master`` branch of github repositories. For many packages, what
happens to be current in ``master`` is a *development* version, which
may break.

Set ``indent-tabs-mode`` to ``nil``, which can be done with::

    (setq-default indent-tabs-mode nil)

This makes it so that tabs are never used for indentation.

Set a ``before-save-hook`` that runs
``delete-trailing-whitespace``. This makes it so that git won't ever
complain about trailing whitespace.

JavaScript
----------

Install flymake-jshint::

M-x package-install [ENTER] flymake-jshint

Install js2-mode::

M-x package-install [ENTER] js2-mode

.. warning:: Make sure you install js2-mode 20130913 or later. This
             may require that you add the Melpa archive just to add
             js2-mode and then remove this archive (see above).

Load `<../emacs/js2-mode-init.el>`_ this will:

#. Make js2-mode your main mode for editing JavaScript.

#. Make js2-mode look recursively for a ``.pylintrc`` from which to
   find which globals should be considered to be defined.

#. Make js2-mode use a different idendation logic so that AMD-style
   code indents like this::

       define(function (...) {

       var foo = 'a';

       });

   The default indentation would add indent in front of ``var
   foo``. The indentation is also changed so that code indents like
   this::

       blah(a, b,
            function () {

       });

   The default would be something like::

       blah(a, b,
            function () {

           });


Python
------

If you run emacs 24.2.1 or older, get `this python mode code
<http://repo.or.cz/w/emacs.git/blob_plain/master:/lisp/progmodes/python.el>`_
and add it to ``load-path``.

Indentation
===========

C
-

For C and C-derived modes, set the customization variable
``c-basic-offset`` to 4. This could be added to your customized
variables in your .emacs::

    '(c-basic-offset 4)


JavaScript
----------

See C_.


Style
=====

C
-

The default style is ``"bsd"``. Set ``c-default-style`` so that "bsd"
is used::

    '(c-default-style (quote ((java-mode . "bsd") (other . "bsd"))))

JavaScript
----------

See C_.


Highlighting
============

JavaScript
----------

This is optional but using the following configuration should help
making jsdoc documentation look better::

    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(js2-jsdoc-html-tag-delimiter ((t nil)))
     '(js2-jsdoc-html-tag-name ((t (:foreground "blue"))))
     '(js2-jsdoc-value ((t (:foreground "gold4")))))


MMM Mode
========

.. warning:: DO NOT USE THIS YET. THERE'S A PROBLEM WITH mmm-mode.

We use MMM mode to enable rst-mode in docstrings in buffers that
contain python code. mmm-mode is available on Melpa. Because Melpa is
distributes bleeding-edge packages, we recommend::

#. Adding Melpa to your list of archives.

#. Installing mmm-mode.

#. Removing Melpa from your list of archives. Otherwise, you'll get
   notifications to upgrade stable packages to unstable ones.

The customization variable ``mmm-global-mode``
should be set to ``maybe`` so that mmm-mode analyzes each
buffer. Adding the following to your invocation of
``custom-set-variables`` would do it::

    '(mmm-global-mode (quote maybe) nil (mmm-mode))

Make your ``.emacs`` load the `<../emacs/mmm-rst-python.el>`_ file.


Optional Changes
================

If you keep a lot of buffers open and use flymake, you may find that
flymake operates slowly. There is an `experimental flymake
<https://github.com/illusori/emacs-flymake.git>`__ that fixes this
problem.

We recommend using ``magit`` to manage git in Emacs.
