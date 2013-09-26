Node changes at a fast pace, so you might to install a newer one
directly from the Node.js site. The following instructions are a
suggestion. You can install node and configure npm any way you want.

Installing Node
===============

Binary
------

#. Uninstall your previous version of node.

#. Get the latest binary from http://nodejs.org/download/

#. Symlink the two files in ``bin/`` to a location that is in your
   ``PATH``. (Louis symlinks to ``~/local/bin/``.)

Source
------

To be written.

NPM Configuration
=================

The npm configuration `described here
<http://tnovelli.net/blog/blog.2011-08-27.node-npm-user-install.html>`_
is recommended. You obviously do not need to follow the node
installation instruction there since you should have already installed
it.

Louis uses a configuration that differs from the page above in the
following way:

#. It uses ``local`` rather than ``.local``.

#. No ``~/.node_modules`` symlink. (Does not seem to impact npm's
   ability to find modules.)
