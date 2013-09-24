At the moment Managlam projects only use git and github.

Github
======

#. Fork the project from the managalam-research account on github into your own github account.

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

   If you cannot use gitflow, you must still work in a way compatible
   with the configuration described above.

#. You almost always want to work from the ``develop`` branch. The
   ``master`` branch only contains a succession of stable releases.

#. Send in a pull request when you have something to contribute. Such
   requests should almost always ask to merge onto ``develop``.
