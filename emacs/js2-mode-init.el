(require 'json)
(require 'flymake-jshint)
(require 'js2-mode)
(defun ldd-js2-parse-jshintrc ()
  "This looks recursively up for a .jshintrc and extracts the
globals from it to add them to js2-additional-externs."
  (let* ((jshintrc (find-file-recursively-up "^\\.jshintrc$"))
         (json (and jshintrc
                    (json-read-file jshintrc)))
         (globals (and json
                       (cdr (assq 'globals json))))
        )
    (when globals
      (setq js2-additional-externs
            (append
             (mapcar (lambda (pair)
                         (symbol-name (car pair))
                     )
                     globals
             )
             js2-additional-externs
            )
      )
      (js2-reparse t)
    )
  )
)

;; Holds the old value of js2-proper-indentation
(fset 'ldd-old-js2-proper-indentation
      (symbol-function 'js2-proper-indentation))

(defun js2-proper-indentation (parse-status)
  "Return the proper indentation for the current line.

This has been modified to support some of the coding standards
used for Mangalam projects.

- A top level `define(` won't increase indentation.

- `function () {` that appear as parameters set their indentation
  so that they do not make the closing `})` appear overindented.
"
  (or
   (when (nth 4 parse-status)
     ;; If we are in a comment, just invoke the default function.
     (ldd-old-js2-proper-indentation parse-status))
   (js2-ctrl-statement-indentation)
   (save-excursion
     (back-to-indentation)
     (let* ((closing-curly-bracket (looking-at "}"))
            (continued-expr-p (js2-continued-expression-p))
            (bracket (nth 1 parse-status)))
       (when bracket
         (goto-char bracket)
         ;; A { preceded by a closing paren, maybe a function start?
         (when (and (looking-at "{[ \t]*\\(/[/*]\\|$\\)")
                    (save-excursion (skip-chars-backward " \t)")
                                    (looking-at ")")))
           (backward-list)
           (backward-word)
           (when (looking-at "function[ \t]*\\((\\|/[/*]\\|$\\)")
             ;; If we're here, we are looking at an anonymous function
             (let* ((status-at-function (syntax-ppss (point)))
                    (function-wrapping-bracket (nth 1 status-at-function)))
               (when (and function-wrapping-bracket
                          (progn
                            (goto-char function-wrapping-bracket)
                            (looking-at "(")))
                 ;; If we're here, our function is inside a (...) list.
                 (back-to-indentation)
                 (cond
                  ;; General anonymous function, cases...

                  ;; We're at the closing bracket of the function,
                  ;; just take the column.
                  (closing-curly-bracket (current-column))

                  (continued-expr-p
                   (+ (current-column) (* 2 js2-basic-offset)))

                  ;; When "define(" at beginning of line, assume
                  ;; an AMD definition and act accordingly.
                  ((and (eq (current-column) 0)
                        (looking-at "define[ \t]*("))
                   (current-column))

                  ;; Otherwise, add an offset.
                  (t (+ (current-column) js2-basic-offset))))))))))
   ;; Customizations did not apply, call the default function.
   (ldd-old-js2-proper-indentation parse-status)))



(add-hook 'js2-mode-hook (lambda () (flymake-mode t)))
(add-hook 'js2-init-hook 'ldd-js2-parse-jshintrc)
(add-hook 'js2-init-hook
          (lambda ()
            ;; This adds support for jsdoc3-style links.
            (setq js2-jsdoc-link-tag-regexp
                  (let* ((spaces
                          "\\(?:\\s-\\|[\n\r]\\)+\\(?:\\s-\\|[*\n\r]\\)*"))
                    (concat
                     ;; start of link
                     "{\\(@\\(?:link\\|code\\)\\)"
                     ;; spaces and *
                     spaces
                     ;; first parameter
                     "\\(.+?\\)"
                     ;; second parameter
                     "\\(?:" spaces "\\(.+?\\)\\)?"
                     ;; terminating character
                     "}")
                  )
            )
          )
)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
