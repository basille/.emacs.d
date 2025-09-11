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

(defun md--shorten-filename (filename max-len)
  "Return FILENAME shortened to MAX-LEN characters, ending with … if too long."
  (if (> (length filename) max-len)
      (concat (substring filename 0 (1- max-len)) "…")
    filename))

(defvar md-todo-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'md-todo-dashboard-open)
    (define-key map (kbd "Q") 'quit-window)
    (define-key map (kbd "n") 'md-todo-dashboard-next-section)
    (define-key map (kbd "p") 'md-todo-dashboard-previous-section)
    map)
  "Keymap for `md-todo-dashboard-mode`.")

(define-derived-mode md-todo-dashboard-mode special-mode "TODO-Dashboard"
  "Major mode for the Markdown TODO dashboard."
  (setq truncate-lines t))

(defun md-todo-dashboard-open ()
  "Open the TODO item at point."
  (interactive)
  (let ((file (get-text-property (point) 'file))
        (line (get-text-property (point) 'line)))
    (when file
      (find-file file)
      (goto-line line))))

(defun md-todo-dashboard-next-section ()
  "Jump to the next section header (wrapping if needed)."
  (interactive)
  (forward-line 1) ;; skip current line so we always move forward
  (if (re-search-forward "^.*items left in .+ list:" nil t)
      (beginning-of-line)
    ;; Wrap to top
    (goto-char (point-min))
    (when (re-search-forward "^.*items left in .+ list:" nil t)
      (beginning-of-line))))

(defun md-todo-dashboard-previous-section ()
  "Jump to the previous section header (wrapping if needed)."
  (interactive)
  (forward-line -1) ;; skip current line so we always move backward
  (if (re-search-backward "^.*items left in .+ list:" nil t)
      (beginning-of-line)
    ;; Wrap to bottom
    (goto-char (point-max))
    (when (re-search-backward "^.*items left in .+ list:" nil t)
      (beginning-of-line))))

(defun md--collect-todos (notes-dir)
  "Return an alist of categorized TODO items from NOTES-DIR."
  (let ((todo-types '(("TODO"  . todo)
                      ("SOON"  . soon)
                      ("LATER" . later)))
        (results '((todo . ()) (soon . ()) (later . ()))))
    (dolist (file (directory-files-recursively notes-dir "\\.md\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((linenum 1))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (dolist (tt todo-types)
                (when (string-match (format "\\*\\*%s\\*\\* *\\(.*\\)" (car tt)) line)
                  (push (list :file file
                              :line linenum
                              :text (match-string 1 line))
                        (cdr (assoc (cdr tt) results))))))
            (forward-line 1)
            (setq linenum (1+ linenum))))))
    ;; Reverse lists to preserve original file order
    (dolist (key '(todo soon later))
      (setf (alist-get key results) (nreverse (alist-get key results))))
    results))

(defun md--insert-section (items title notes-dir max-col-width)
  "Insert a section with ITEMS, TITLE string, relative to NOTES-DIR.
Shorten file names to MAX-COL-WIDTH."
  (let ((header-start (point)))
    (insert (format "%d items left in the %s list:\n\n"
                    (length items) title))
    ;; Bold the section type (to-do / soon / later)
    (save-excursion
      (goto-char header-start)
      (when (re-search-forward (regexp-quote title) (line-end-position) t)
        (add-text-properties (match-beginning 0) (match-end 0)
                             '(face bold)))))
  ;; Insert items
  (dolist (item items)
    (let* ((file (plist-get item :file))
           (line (plist-get item :line))
           (text (plist-get item :text))
           (relfile (file-relative-name file notes-dir))
           (shortname (md--shorten-filename relfile max-col-width))
           (start (point)))
      ;; File name (link)
      (insert (format (format "%%-%ds" max-col-width) shortname))
      (add-text-properties start (+ start (length shortname))
                           `(file ,file line ,line mouse-face highlight
                                  help-echo ,file
                                  face link))
      ;; TODO text
      (insert "  ")
      (let ((text-start (point)))
        (insert text "\n")
        (add-text-properties text-start (point)
                             '(face bold)))))
  (insert "\n"))

(defun md-todo-dashboard ()
  "Display a categorized TODO dashboard for all Markdown notes in ~/Public/Notes."
  (interactive)
  (let* ((buffer (get-buffer-create "*TODO Dashboard*"))
         (notes-dir "~/Public/Notes/")
         (todo-data (md--collect-todos notes-dir))
         (max-col-width 35))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (md-todo-dashboard-mode)
        (setq truncate-lines t)

        ;; Insert sections
        (md--insert-section (alist-get 'todo todo-data) "to-do" notes-dir max-col-width)
        (md--insert-section (alist-get 'soon todo-data) "soon" notes-dir max-col-width)
        (md--insert-section (alist-get 'later todo-data) "later" notes-dir max-col-width)))

    ;; Display buffer at top
    (display-buffer-in-side-window buffer
                                   '((side . top) (slot . 0) (window-height . 0.3)))
    (select-window (get-buffer-window buffer))
    (goto-char (point-min))))



