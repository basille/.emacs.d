;; To-do library for Markdown
;; Author: Mathieu Basille & ChatGPT

(require 'cl-lib)

(defcustom md-todo-states '("**TODO**" "**DONE**" "**CANCEL**" "")
  "List of states cycled by `md-cycle-todo'."
  :type '(repeat string)
  :group 'markdown)

(defun md-todo-cycle ()
  "Cycle TODO states at the start of a Markdown list line.
Cursor follows the text if a keyword is added or removed."
  (interactive)
  (let* ((pos (point))
         (line-beg (line-beginning-position)))
    (save-excursion
      (goto-char line-beg)
      (skip-chars-forward " \t")
      ;; Skip list marker if present
      (when (looking-at "\\([*+-]\\|[0-9]+[.)]\\)\\s-+")
        (goto-char (match-end 0)))
      ;; Compute cursor offset from current point
      (let ((offset (- pos (point))))
        ;; Check for existing keyword
        (let ((match (cl-find-if (lambda (s) (looking-at (regexp-quote s)))
                                 md-todo-states)))
          (let ((next (if match
                          (nth (mod (1+ (cl-position match md-todo-states :test #'string=))
                                     (length md-todo-states))
                               md-todo-states)
                        (car md-todo-states))))
            ;; Remove existing keyword + following space
            (when match
              (delete-region (match-beginning 0) (match-end 0))
              (when (looking-at " ") (delete-char 1)))
            ;; Insert next state if non-empty
            (unless (string= next "")
              (insert next)
              (unless (or (eolp) (looking-at " "))
                (insert " "))))
          ;; Restore cursor relative to text
          (goto-char (+ (point) offset)))))))

(defun md-todo-list ()
  "Opens a top window with a list of all TODO items in the Notes folder."
  (interactive)
  (grep-compute-defaults)
  (rgrep "\\*\\*TODO\\*\\*" "*.md" "~/Public/Notes/./")
  (other-window 1))
