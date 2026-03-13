(defun mailto-replace-mailto-with-angle-brackets ()
  "Replace [email](mailto:email) with <email> in the region or buffer.
Shows the number of replacements made."
  (interactive)
  (let ((count 0)
        (bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (cons (point-min) (point-max)))))
    (save-excursion
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (goto-char (point-min))
        (while (re-search-forward "\\[\\([^]]+\\)\\](mailto:\\1)" nil t)
          (replace-match (concat "<" (match-string 1) ">"))
          (setq count (1+ count)))))
    (message "Replaced %d occurrences." count)))

(defun mailto-replace-angle-brackets-with-mailto ()
  "Replace <email> with [email](mailto:email) in the region or buffer, if <email> is valid.
Shows the number of replacements made."
  (interactive)
  (let ((count 0)
        (bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (cons (point-min) (point-max)))))
    (save-excursion
      (save-restriction
        (narrow-to-region (car bounds) (cdr bounds))
        (goto-char (point-min))
        (while (re-search-forward "<\\([^>]+\\)>" nil t)
          (let ((email (match-string 1)))
            (when (string-match-p
                   "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
                   email)
              (replace-match (concat "[" email "](mailto:" email "]"))
              (setq count (1+ count)))))))
    (message "Replaced %d occurrences." count)))
