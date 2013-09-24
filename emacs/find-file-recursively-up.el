(defun find-file-recursively-up (regexp)
  "Searches for regexp in the current directory and recursively in parents
   until it finds the file or fails to find it."
  (let* ((find-file-r (lambda (path)
                        (or
                         ;; search here
                         (directory-files path t regexp)
                         ;; not found, so move up
                         (let* ((parent (file-name-directory
                                         (directory-file-name path))))
                           (and parent
                                ;; Eventually path is "/", at which
                                ;; point getting its parent is also
                                ;; "/". So w/o this test, we'd recurse
                                ;; forever.
                                (not (string= parent path))
                                (funcall find-file-r parent))
                         )
                        )
                      )
         )
         (start (if (or (null buffer-file-name)
                        (file-directory-p buffer-file-name))
                    buffer-file-name
                  (file-name-directory buffer-file-name))
         )
        )
    (when start
      (funcall find-file-r start))
  )
)
