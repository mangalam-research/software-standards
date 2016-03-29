At the moment Mangalam projects only use git and github for version
control.

Git
===

.. _gitflow:

#. (Using gitflow is optional if you contribute rarely. It is
   mandatory if you contribute frequently.)  Install the AVH edition
   of `gitflow <https://github.com/petervanderdoes/gitflow>`__. We use
   this version because the original gitflow appears to be
   unmaintained (as of 2013-11-24). The AVH edition contains numerous
   fixes, the most important one being that ``git describe`` can be
   used again to get meaningful build ids.

#. We recommend that you globally turn on ``rerere.enabled``::

        git config --global rerere.enabled true

#. Fork the project from the mangalam-research account on github into
   your own github account.

#. Clone your fork to your machine.

#. Set your remotes so that:

    - ``origin`` points to your fork on github.

    - ``upstream`` points to the Mangalam repository.

#. Set up gitflow with the following configuration parameters::

       [gitflow "branch"]
       master = master
       develop = develop
       [gitflow "prefix"]
       feature = feature/
       release = release/
       hotfix = hotfix/
       support = support/
       versiontag = v

   Whether or not you use gitflow, you must still work in a way
   compatible with the configuration described above.

#. You almost always want to work (i.e. create branches) from the
   ``develop`` branch. The ``master`` branch only contains a
   succession of stable releases.

   .. note:: There are few rare exceptions to this rule. For instance,
             generating the documentation for wed must (with very rare
             exceptions; yes, there are exceptions to the exceptions)
             be done from ``master`` to get a correct version number
             in the generated documentation.

Working on Features
===================

Try to plan your work so that you work on one well delimited unit of
functionality at a time. Create a feature branch for it::

    $ git flow feature start [name of feature]

Please use a reasonably descriptive name because other folks will
probably see this name at some point (e.g. ``schema-documentation``,
``cut-copy-and-paste``). This will create a branch named
``feature/[name of feature]``.

Use the branch to develop your work and commit there.

When you are ready to submit for inclusion into the Mangalam
repository::

    $ git flow feature publish

This will publish your branch to ``origin`` (which should be your own
fork on github). Then you can pull in a pull request for the feature.

Github
======

#. Send in a pull request when you have something to contribute. Such
   requests should almost always ask to merge onto ``develop``.

Keeping Your Code in Sync
-------------------------

From time to time you should pull from the mangalam repositories to
keep your code in sync with the master version. The way to do this is
as follows::

    $ git pull upstream develop

    $ git push origin develop

You can do this with ``master`` too if you want or need. It is also
possible to do this::

    $ git pull upstream :

    $ git push origin :

Note the colon at the end of both commands. It asks git to transfer
data between identically named branches. So ``master`` and ``develop``
will both be updated automatically, and so are any branches that have
the same name on ``upstream``, locally and on ``origin``.

..  LocalWords:  github gitflow AVH rerere config mangalam hotfix
..  LocalWords:  versiontag
