(defun shell-xterm ()
  "Runs a shell with clearing capabilities."
  (interactive)
  (shell)
  (insert "TERM=xterm")
  (comint-send-input nil t))
