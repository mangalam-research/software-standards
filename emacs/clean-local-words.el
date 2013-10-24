(defun clean-local-words ()
  "Removes from LocalWords lists those words that are not present
in the buffer (elsewhere than in LocalWords). Also removes
duplicate words from LocalWords."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let* (lines ;; Lines where we got a match.
           eol ;; EOL position of the current line.
           (words (make-hash-table :test 'equal)) ;; Words gathered.
           final ;; Final list of words.
          )
      (while (search-forward ispell-words-keyword nil t)
        ;; Only do it if we are in a comment.
        (when (save-excursion (comment-beginning))
          (setq lines (cons (line-number-at-pos) lines))
          (setq eol (point-at-eol))
          (while (re-search-forward " *\\([^ ]+\\)" eol t)
            (puthash (match-string-no-properties 1) t words)
          )
        )
      )
      ;; Check whether the words are actually in the file.
      (goto-char (point-min))
      (maphash (lambda (key value)
                 (save-excursion
                   (when (and (re-search-forward
                               (concat "\\b" (regexp-quote key) "\\b") nil t)
                              ;; Don't count hits in our lines.
                              (not (member (line-number-at-pos) lines))
                         )
                     (setq final (cons key final))
                   )
                 )
               )
               words
      )
      (atomic-change-group
        ;; Delete the lines from which we got our words
        (let ((kill-whole-line t))
          (mapc (lambda (line)
                  (goto-line line)
                  (kill-line)
                )
                ;; The lines are already in reverse order, so we won't mess up
                ;; line numbers as we delete.
                lines
          )
          ;; Append the remaining words to the buffer.
          (goto-char (point-max))
          (mapc (lambda (word)
                  (ispell-add-per-file-word-list word)
                )
                final
          )
        )

      )
    )
  )
)
