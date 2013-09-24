(require 'python)

(defun rst-python-docstrings-find-front (bound)
  (when (not bound)
    (setq bound (point-max))
  )
  (message "Start %s" (point))
  (loop
   while (< (point) bound)
   do (progn
        (message "Search at %s" (point))
        (when (re-search-forward "\\(\"\"\"\\|\'\'\'\\)" bound 'limit)
        (let* ((start (match-beginning 0)))
          (save-excursion
            (goto-char start)
            (save-match-data
              (python-nav-beginning-of-statement)
            )
            (when (and (= (point) start))
              (return (match-end 0))
            )
          )
        )
      )
      )
  )
)

(defun rst-python-docstrings-find-back (bound)
  (when (not bound)
    (setq bound (point-max))
  )
  (loop
   while (< (point) bound)
   do (when (re-search-forward "\\(\"\"\"\\|\'\'\'\\)" bound t)
        (let* ((delim (match-string 0)))
          (save-excursion
            (save-match-data (python-nav-beginning-of-statement))
            (when (looking-at-p delim)
              (return (match-end 0))
            )
          )
        )
      )
  )
)

(require 'mmm-mode)

(add-to-list 'mmm-save-local-variables 'adaptive-fill-regexp)
(add-to-list 'mmm-save-local-variables 'fill-paragraph-function)

(mmm-add-classes
 '((rst-python-docstrings
    :submode rst-mode
    :face mmm-comment-submode-face
    :front rst-python-docstrings-find-front
    :back rst-python-docstrings-find-back
    :save-matches 1
    :insert ((?d embdocstring nil @ "\"\"\"" @ _ @ "\"\"\"" @))
    :delimiter-mode nil
   )
 )
)
;;;(assq-delete-all 'rst-python-docstrings mmm-classes-alist)

(mmm-add-mode-ext-class 'python-mode nil 'rst-python-docstrings)
