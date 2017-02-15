(defun sql-send-region-or-paragrap ()
    "Evaluate region if there is an active one, otherwise the current paragraph."
  (interactive)
  ;; (interactive "P")
  (if (and transient-mark-mode mark-active ;; xemacs doesn't have use-region-p
           (> (region-end) (region-beginning)))
      (sql-send-region (region-beginning) (region-end))
    (sql-send-paragraph)))
