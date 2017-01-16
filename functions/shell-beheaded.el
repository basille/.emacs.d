;; Function shell-beheaded to open a Shell on Beheaded with screen
;; capability
(defun shell-beheaded ()
  "Connect to or visit a remote shell on Beheaded."
  (interactive)
  ;; Check if buffer already exists
  (if (not (get-buffer "*shell-beheaded*"))
      (progn
	;; Split window if necessary
        (split-window-sensibly (selected-window))
	;; Switch to other window
        (other-window 1)
	;; Run a shell
	(shell)
	;; Rename it as *shell-beheaded*
	(rename-buffer "*shell-beheaded*")
	;; String to connect to Beheaded via SSH + screen capability
	(insert "ssh Beheaded\nTERM=xterm")
	;; Evaluate the string
	(comint-send-input))
    ;; Switch to *shell-beheaded*
    (switch-to-buffer-other-window "*shell-beheaded*")))
