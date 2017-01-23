(defun md-fenced-r-code-block ()
  "Adds a fenced block for R code in Markdown"
  (interactive)
  (insert "\n```{r}\n\n```\n")
  (previous-line)
  (previous-line))

(defun md-inline-r-code ()
  "Insert inline R code in Markdown"
  (interactive)
  (insert "`r `")
  (backward-char))
