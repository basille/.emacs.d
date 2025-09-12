;; To-do library for Markdown
;; Author: Mathieu Basille & ChatGPT

(require 'cl-lib)

(defcustom md-todo-states '("**TODO**" "**DONE**" "**SOON**" "**LATER**" "**CANCEL**" "")
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

;;; md-todo-dashboard.el --- Markdown TODO Dashboard

;;; md-todo-dashboard.el --- Markdown TODO Dashboard

(defun md--shorten-filename (filename max-len)
  "Return FILENAME shortened to MAX-LEN characters, ending with … if too long."
  (if (> (length filename) max-len)
      (concat (substring filename 0 (1- max-len)) "…")
    filename))

(defvar md-todo-dashboard-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'md-todo-dashboard-visit)
    map)
  "Keymap used for clickable filename text in the TODO dashboard.")

(defvar md-todo-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'md-todo-dashboard-open)
    (define-key map (kbd "Q") 'quit-window)
    (define-key map (kbd "n") 'md-todo-dashboard-next-section)
    (define-key map (kbd "p") 'md-todo-dashboard-previous-section)
    (define-key map (kbd "TAB") 'md-todo-dashboard-toggle-section)
    (define-key map (kbd "r") 'md-todo-dashboard-refresh)
    map)
  "Keymap for `md-todo-dashboard-mode'.")

(define-derived-mode md-todo-dashboard-mode special-mode "TODO-Dashboard"
  "Major mode for the Markdown TODO dashboard."
  (setq truncate-lines t))

(defun md-todo-dashboard-open ()
  "Open the TODO item at point (RET)."
  (interactive)
  (let ((file (get-text-property (point) 'file))
        (line (get-text-property (point) 'line)))
    (when file
      (find-file file)
      (goto-char (point-min))
      (forward-line (1- (or line 1))))))

(defun md-todo-dashboard-visit (event)
  "Open the TODO clicked with the mouse (mouse-1)."
  (interactive "e")
  (let* ((posn (event-start event))
         (win (posn-window posn))
         (buf (window-buffer win))
         (pt  (posn-point posn)))
    (when (and pt buf)
      (with-current-buffer buf
        (goto-char pt)
        (let ((file (get-text-property pt 'file))
              (line (get-text-property pt 'line)))
          (when file
            (find-file file)
            (goto-char (point-min))
            (forward-line (1- (or line 1)))))))))

(defun md-todo-dashboard-next-section ()
  "Jump to the next section header (wraps)."
  (interactive)
  (forward-line 1)
  (if (re-search-forward "^# .*items left in .+ list" nil t)
      (beginning-of-line)
    (goto-char (point-min))
    (when (re-search-forward "^# .*items left in .+ list" nil t)
      (beginning-of-line))))

(defun md-todo-dashboard-previous-section ()
  "Jump to the previous section header (wraps)."
  (interactive)
  (forward-line -1)
  (if (re-search-backward "^# .*items left in .+ list" nil t)
      (beginning-of-line)
    (goto-char (point-max))
    (when (re-search-backward "^# .*items left in .+ list" nil t)
      (beginning-of-line))))

(defun md--collect-todos (notes-dir)
  "Return an alist of categorized TODO items from NOTES-DIR.
Lists keyed by symbols 'todo, 'soon, 'later. Each item is a plist (:file FILE :line LINE :text TEXT)."
  (let* ((todo-types '(("TODO"  . todo)
                       ("SOON"  . soon)
                       ("LATER" . later)))
         (results (list (cons 'todo  nil)
                        (cons 'soon  nil)
                        (cons 'later nil))))
    (dolist (file (directory-files-recursively notes-dir "\\.md\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((linenum 1))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
              (dolist (tt todo-types)
                (when (string-match (format "\\*\\*%s\\*\\* *\\(.*\\)" (car tt)) line)
                  (push (list :file file :line linenum :text (match-string 1 line))
                        (alist-get (cdr tt) results)))))
            (forward-line 1)
            (setq linenum (1+ linenum))))))
    ;; reverse each list to preserve order
    (dolist (k '(todo soon later))
      (setf (alist-get k results) (nreverse (alist-get k results))))
    results))

(defun md--insert-section (items title notes-dir max-col-width)
  "Insert a section for ITEMS with TITLE; use NOTES-DIR and MAX-COL-WIDTH."
  (let ((header-start (point)))
    ;; Insert header line with trailing colon
    (insert (format "# %d items left in the %s list:" (length items) title))
    (let ((header-end (point)))
      ;; Markdown header face
      (add-text-properties header-start header-end
                           '(section-header t md-folded nil face markdown-header-face-1))
      ;; Bold only the section word
      (save-excursion
        (goto-char header-start)
        (when (re-search-forward "\\(to\\-do\\|soon\\|later\\)" header-end t)
          (add-text-properties (match-beginning 0) (match-end 0)
                               '(face (:inherit (bold markdown-header-face-1))))))))
  ;; Newline (plain)
  (insert "\n\n")
  ;; Insert items
  (dolist (item items)
    (let* ((file (plist-get item :file))
           (line (plist-get item :line))
           (text (plist-get item :text))
           (relfile (file-relative-name file notes-dir))
           (shortname (md--shorten-filename relfile max-col-width))
           (start (point)))
      (insert (format (format "%%-%ds" max-col-width) shortname))
      (add-text-properties start (+ start (length shortname))
                           `(file ,file line ,line mouse-face highlight
                                  help-echo ,file
                                  face link
                                  keymap ,md-todo-dashboard-link-map))
      (insert " ")
      (let ((text-start (point)))
        (insert text "\n")
        (add-text-properties text-start (point) '(face bold)))))
  ;; Ensure single blank line after section
  (unless (looking-back "\n\n" nil)
    (insert "\n")))

(defun md-todo-dashboard-toggle-section ()
  "Toggle visibility of section at point."
  (interactive)
  (when (get-text-property (point) 'section-header)
    (save-excursion
      ;; Determine section bounds
      (let* ((header-start (line-beginning-position))
             (header-end (line-end-position))
             (content-start (progn (forward-line 1) (point)))
             (section-end (or (and (re-search-forward "^# .*items left in .+ list" nil t)
                                   (line-beginning-position))
                              (point-max)))
             (inhibit-read-only t)
             (folded (get-text-property header-start 'md-folded)))
        ;; Keep blank line visible at end of section
        (save-excursion
          (goto-char section-end)
          (forward-line -1)
          (when (looking-at-p "^$")
            (setq section-end (point))))
        ;; Toggle fold/unfold
        (if folded
            ;; Expand
            (remove-overlays content-start section-end 'md-section t)
          ;; Collapse
          (let ((ov (make-overlay content-start section-end)))
            (overlay-put ov 'invisible t)
            (overlay-put ov 'md-section t)
            (overlay-put ov 'isearch-open-invisible
                         (lambda (_ov) (delete-overlay _ov)))))
        ;; Rebuild header line string with correct trailing character
        (let* ((line-text (buffer-substring-no-properties header-start header-end))
               ;; Extract section word (to-do/soon/later)
               (case-word (if (string-match "\\(to\\-do\\|soon\\|later\\)" line-text)
                              (match-string 1 line-text)
                            ""))
               ;; Count number of items from the header (optional)
               ;; Preserve folding state: use … if collapsed, : if expanded
               (trailing-char (if folded "…" ":"))
               ;; Rebuild header
               (new-header (replace-regexp-in-string "[:…]$" trailing-char line-text)))
          ;; Replace old header line
          (goto-char header-start)
          (delete-region header-start header-end)
          (insert new-header)
          ;; Apply Markdown header face to entire header line
          (put-text-property header-start (point) 'face 'markdown-header-face-1)
          ;; Bold the section word
          (when (and case-word (not (string-empty-p case-word)))
            (let ((start (+ header-start (string-match (regexp-quote case-word) new-header)))
                  (end   (+ header-start (string-match (regexp-quote case-word) new-header) (length case-word))))
              (put-text-property start end 'face '(:inherit (bold markdown-header-face-1)))))
          ;; Store md-folded property
          (put-text-property header-start (point) 'md-folded (not folded))
          ;; Keep section-header property
          (put-text-property header-start (point) 'section-header t))))))

(defun md-todo-dashboard-refresh ()
  "Refresh the TODO dashboard buffer."
  (interactive)
  (when (eq major-mode 'md-todo-dashboard-mode)
    (let ((inhibit-read-only t)
          (notes-dir "~/Public/Notes/")
          (max-col-width 35))
      (erase-buffer)
      (let ((todo-data (md--collect-todos notes-dir)))
        (md--insert-section (alist-get 'todo todo-data) "to-do" notes-dir max-col-width)
        (md--insert-section (alist-get 'soon todo-data) "soon" notes-dir max-col-width)
        (md--insert-section (alist-get 'later todo-data) "later" notes-dir max-col-width))
      (goto-char (point-min)))))

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
        (md--insert-section (alist-get 'todo todo-data) "to-do" notes-dir max-col-width)
        (md--insert-section (alist-get 'soon todo-data) "soon" notes-dir max-col-width)
        (md--insert-section (alist-get 'later todo-data) "later" notes-dir max-col-width)))
    (display-buffer-in-side-window buffer '((side . top) (slot . 0) (window-height . 0.3)))
    (select-window (get-buffer-window buffer))
    (goto-char (point-min))))
