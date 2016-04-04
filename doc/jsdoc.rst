JSDoc 3 is documented at http://usejsdoc.org/

We turn on Markdown processing with the following in the ``jsdoc.conf.json``::

    "plugins": ["plugins/markdown"],
    "markdown": {
        "gfm": true,
        "tables": true
    },

With this turned on, all the doclets that jsdoc processes can contain
markup in the Markdown format.

.. note:: Mangalam's projects use both Markdown and reStructuredText
          in different contexts. Sometimes the way to markup a piece
          of text with Markdown and reStructuredText can differ quite
          a bit (e.g. hyperlinks). However, there are cases where it
          is possible to use the same markup for both. One such
          prominent case is how to mark inline code. In
          reStructuredText, you have to use two backticks to start and
          end the code. Using a single backtick will **not** produce
          the desired result. Markdown on the other hand will be happy
          with a single backtick *and* a double backtick.

          For the formal documentation bundled with the code, we've
          settled on using the double backtick both when using
          reStructuredText *and* Markdown.
