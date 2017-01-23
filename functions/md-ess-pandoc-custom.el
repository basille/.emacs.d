;; Function md-ess-pandoc-custom to use knitr::pandoc with options in
;; .pandoc/config.pandoc
(defun md-ess-pandoc-custom ()
  "Run Pandoc on the associated .md file."
  (interactive)
  (let* ((rmd-buf (current-buffer)))
    (save-excursion
      (let* ((sprocess (ess-get-process ess-current-process-name))
             (sbuffer (process-buffer sprocess))
             (md-filename (file-name-sans-extension (buffer-file-name)))
             (buf-coding (symbol-name buffer-file-coding-system))
             (pandoc-cmd
	      (format "require(knitr); pandoc(\"%s.md\", format =
	      \"html\", config =
	      \"/home/mathieu/.pandoc/config.pandoc\", encoding =
	      \"UTF-8\")" md-filename)))
	(message "Running pandoc on %s.md" md-filename)
        (ess-execute pandoc-cmd 'buffer nil nil)
        (switch-to-buffer rmd-buf)
        (ess-show-buffer (buffer-name sbuffer) nil)))))
