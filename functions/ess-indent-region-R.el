(defun ess-indent-region-as-R-function (beg end)
  (let ((string (replace-regexp-in-string
		 "\"" "\\\\\\&"
		 (replace-regexp-in-string ;; how to avoid this double matching?
		  "\\\\\"" "\\\\\\&" (buffer-substring-no-properties beg end))))
	(buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command (format "local({oo<-options(keep.source=FALSE);
cat('\n',paste(deparse(parse(text=\"%s\")[[1L]]),collapse='\n'),'\n',sep='')
options(oo)})\n"  string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      ;; (skip-chars-backward "\n")
      (let ((end (point)))
	(goto-char (point-min))
	(goto-char (1+ (point-at-eol)))
	(setq string (buffer-substring-no-properties (point) end))
	))
  (delete-region beg end)
  (insert string)
  ))

;; (add-hook 'ess-mode-hook '(lambda () (set (make-local-variable 'indent-region-function)
;; 					  'ess-indent-region-as-R-function)))


;; From Walmes Zeviani
;; https://github.com/basille/.emacs.d/issues/1
(defun ess-indent-region-with-formatR-function (beg end)
  "Format region of code R using formatR::tidy_source()."
  (interactive "r")
  (let ((string
         (replace-regexp-in-string
          "\"" "\\\\\\&"
          (replace-regexp-in-string ;; how to avoid this double matching?
           "\\\\\"" "\\\\\\&"
           (buffer-substring-no-properties beg end))))
	(buf (get-buffer-create "*ess-command-output*")))
    (ess-force-buffer-current "Process to load into:")
    (ess-command
     (format
      "local({
          formatR::tidy_source(text=\"\n%s\",
                               arrow=TRUE, width.cutoff=60) })\n"
      string) buf)
    (with-current-buffer buf
      (goto-char (point-max))
      ;; (skip-chars-backward "\n")
      (let ((end (point)))
	(goto-char (point-min))
	(goto-char (1+ (point-at-eol)))
	(setq string (buffer-substring-no-properties (point) end))
	))
    (delete-region beg end)
    (insert string)
    (delete-backward-char 2)
    ))

(add-hook 'ess-mode-hook '(lambda () (set (make-local-variable 'indent-region-function)
					  'ess-indent-region-with-formatR-function)))
