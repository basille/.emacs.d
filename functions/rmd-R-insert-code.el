(defun rmd-R-fenced-code-block ()
  "Adds a fenced block for R code in Markdown"
  (interactive)
  (insert "\n```{r}\n\n```\n")
  (previous-line)
  (previous-line))

(defun rmd-R-inline-code ()
  "Insert inline R code in Markdown"
  (interactive)
  (insert "`r `")
  (backward-char))
