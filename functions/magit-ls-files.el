;; Execute 'ls-files' through Git
;; 
;; http://stackoverflow.com/a/25227438
(defun magit-ls-files ()
  "List tracked files of current repository."
  (interactive)
  (if (derived-mode-p 'magit-mode)
      (magit-git-command "ls-files" default-directory)
    (message "Not in a Magit buffer.")))
