(require 'newcomment)
(require 'ispell)

(defun clean-local-words ()
  "Removes from LocalWords lists those words that are not present
in the buffer (elsewhere than in LocalWords). Also removes
duplicate words from LocalWords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (atomic-change-group
      (let* (eol ;; EOL position on the current line.
             (words (make-hash-table :test 'equal)) ;; Words gathered.
             final ;; Final list of words.
            )
        (while (search-forward ispell-words-keyword nil t)
          ;; Only do it if we are in a comment.
          (when (save-excursion (comment-beginning))
            (setq eol (point-at-eol))
            (while (re-search-forward " *\\([^ ]+\\)" eol t)
              (puthash (match-string-no-properties 1) t words)
            )
            (delete-region (point-at-bol) (min (1+ eol) (point-max)))
          )
        )
        ;; Check whether the words are actually in the file.
        (maphash (lambda (key value)
                   (goto-char (point-min))
                   (when (re-search-forward
                          (concat "\\b" (regexp-quote key) "\\b") nil t)
                     (setq final (cons key final))
                   )
                 )
                 words
        )
        ;; Append the remaining words to the buffer.
        (goto-char (point-max))
        (mapc 'ispell-add-per-file-word-list final)
      )
    )
  )
)
