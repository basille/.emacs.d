;; To-do library for Markdown
;; Author: Mathieu Basille

;; Function md-todo to insert **TODO** at beginning of line.
(defun md-todo ()
  "Insert **TODO** at beginning of line in Markdown."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (forward-word 1)
    (backward-word 1)
    (insert "**TODO** ")))

;; Function md-todo-done to replace **TODO** in current line by **DONE**.
(defun md-todo-done ()
  "Switch from TODO to DONE."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (while (search-forward "**TODO**" (line-end-position) 0)
        (replace-match "**DONE**" 1))))

;; Function md-todo-list to display a TODO list with 'rgrep'
(defun md-todo-list ()
  "Opens a top window with a list of all TODO items in the Notes folder."
  (interactive)
  (grep-compute-defaults)
  (rgrep "\\*\\*TODO\\*\\*" "*.md" "~/Public/Notes/./")
  (other-window 1))
