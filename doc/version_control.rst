At the moment Managlam projects only use git and github for version
control.

Git
===

.. _gitflow:

#. (Using gitflow is optional if you are not part of the core
   developer team. It is mandatory if you are part of this team.)
   Install the AVH edition of `gitflow
   <https://github.com/petervanderdoes/gitflow>`__. We use this version
   because the original gitflow appears to be unmaintained (as of
   2013-10-28). The AVH edition contains numerous fixes, the most
   important one being that ``git describe`` can be used again to get
   meaningful build ids.

#. We recommend that you globally turn on ``rerere.enabled``::

        git config --global rerere.enabled true

Github
======

#. Fork the project from the managalam-research account on github into your own github account.

#. (See gitflow_ to determine whether this is optional for you.) Set
   up gitflow with the following configuration parameters::

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

#. Send in a pull request when you have something to contribute. Such
   requests should almost always ask to merge onto ``develop``.
