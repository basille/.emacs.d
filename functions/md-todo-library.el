;; To-do library for Markdown
;; Author: Mathieu Basille & ChatGPT

(require 'cl-lib)

;; ------------------------------
;; 1. Note folder, tags and colors
;; ------------------------------

(defvar md-todo-folder "~/Public/Notes/"
  "Folder of the Markdown notes.")

(defvar md-todo-tags '("TODO" "DONE" "SOON" "LATER" "PERSO" "CANCEL" "")
  "List of tags cycled by `md-todo-cycle`. Includes empty string for removing a tag.")

(defvar md-todo-colors
  '(("TODO"   . (:bg "#e01b24" :fg "white"))
    ("DONE"   . (:bg "#000000" :fg "white"))
    ("SOON"   . (:bg "#ff7800" :fg "white"))
    ("LATER"  . (:bg "#33d17a" :fg "white"))
    ("PERSO"  . (:bg "#3584e4" :fg "white"))
    ("CANCEL" . (:bg "#9a9996" :fg "white")))
  "Mapping of tags to background and foreground colors.")

;; ------------------------------
;; 2. Flyspell ignores tags
;; ------------------------------
(defun md-todo-flyspell-ignore ()
  "Return t if the word at point should be ignored by Flyspell."
  (let ((word (thing-at-point 'word t)))
    (and word (not (member word (butlast md-todo-tags))))))

(defun md-todo-setup-flyspell ()
  "Configure Flyspell to ignore TODO tags."
  (setq-local flyspell-generic-check-word-predicate #'md-todo-flyspell-ignore)
  ;; Only underline typos, no background
  (face-spec-reset-face 'flyspell-incorrect)
  (set-face-attribute 'flyspell-incorrect nil
                      :underline '(:color "red" :style wave)
                      :background nil)
  (face-spec-reset-face 'flyspell-duplicate)
  (set-face-attribute 'flyspell-duplicate nil
                      :underline '(:color "orange" :style wave)
                      :background nil))

(add-hook 'markdown-mode-hook #'md-todo-setup-flyspell)
(add-hook 'poly-markdown-mode-hook #'md-todo-setup-flyspell)

;; ------------------------------
;; 3. Cycle TODO tags
;; ------------------------------
(defun md-todo-cycle ()
  "Cycle tags at the start of a Markdown list line.
Cursor follows the text relative to content:
- At start → after tag
- Middle → same character relative to text
- End → stays at end
Handles cycling to empty tag correctly."
  (interactive)
  (let* ((line-beg (line-beginning-position))
         (orig-pos (point))
         content-start content-line text-offset match next)
    ;; Move to content after whitespace/list marker
    (goto-char line-beg)
    (skip-chars-forward " \t")
    (when (looking-at "\\([*+-]\\|[0-9]+[.)]\\|#+\\)\\s-+")
      (goto-char (match-end 0)))
    (setq content-start (point))
    ;; Current content text (without any tag)
    (setq content-line (buffer-substring-no-properties content-start (line-end-position)))
    ;; Detect existing tag at start of line
    (setq match (cl-find-if (lambda (s) (string-prefix-p s content-line)) md-todo-tags))
    ;; Remove existing tag from content-line for offset calculation
    (when match
      (setq content-line (string-trim-left (substring content-line (length match)))))
    ;; Compute cursor offset relative to content-line
    (setq text-offset (max 0 (- orig-pos content-start)))
    (when (and match (>= text-offset (length match)))
      ;; Adjust for removed tag (subtract 1 only if next tag is not the first one)
      (unless (string= (car md-todo-tags) (nth (mod (1+ (cl-position match md-todo-tags :test #'string=))
                                                   (length md-todo-tags))
                                               md-todo-tags))
        (setq text-offset (- text-offset (length match) 1))))
    (when (< text-offset 0) (setq text-offset 0))
    (when (> text-offset (length content-line)) (setq text-offset (length content-line)))
    ;; Compute next tag
    (setq next (if match
                   (nth (mod (1+ (cl-position match md-todo-tags :test #'string=))
                              (length md-todo-tags))
                        md-todo-tags)
                 (car md-todo-tags)))
    ;; Replace line content
    (delete-region content-start (line-end-position))
    (goto-char content-start)
    (if (string= next "")
        (insert content-line)
      (insert next " " content-line))
    ;; Restore cursor relative to text
    (goto-char
     (cond
      ((<= orig-pos content-start)
       ;; cursor at or before content → after tag
       (if (string= next "") content-start (+ content-start (length next) 1)))
      ((>= orig-pos (line-end-position))
       ;; cursor at end → stay at end of line
       (line-end-position))
      (t
       ;; cursor in middle → maintain offset in text
       (+ content-start (if (string= next "") 0 (+ (length next) 1)) text-offset))))
    ;; Apply overlays
    (save-excursion
      (let ((beg (line-beginning-position))
            (end (line-end-position)))
        (remove-overlays beg end 'md-todo-overlay t)
        (goto-char beg)
        ;; Only uppercase tags, case-sensitive
        (let ((uppercase-tags (cl-remove-if-not (lambda (tag) (string= tag (upcase tag)))
                                                (butlast md-todo-tags))))
          (let ((case-fold-search nil))  ;; ensure case-sensitive search
            (while (re-search-forward (regexp-opt uppercase-tags) end t)
              (let* ((tag (match-string 0))
                     (colors (cdr (assoc tag md-todo-colors)))
                     (bg (plist-get colors :bg))
                     (fg (plist-get colors :fg))
                     (ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'md-todo-overlay t)
                (overlay-put ov 'face `(:background ,bg :foreground ,fg :weight bold))))))))))

;; ------------------------------
;; 4. Apply overlays on buffer load
;; ------------------------------
(defun md-todo-apply-overlay-highlighting ()
  "Apply colored overlays to all uppercase tags in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward (regexp-opt (butlast md-todo-tags) 'words) nil t)
      (let ((tag (match-string 0)))
        (when (string= tag (upcase tag))
          (let* ((colors (cdr (assoc tag md-todo-colors)))
                 (bg (plist-get colors :bg))
                 (fg (plist-get colors :fg))
                 (ov (make-overlay (match-beginning 0) (match-end 0))))
            (overlay-put ov 'md-todo-overlay t)
            (overlay-put ov 'face `(:background ,bg :foreground ,fg :weight bold))))))))

(add-hook 'markdown-mode-hook #'md-todo-apply-overlay-highlighting)
(add-hook 'poly-markdown-mode-hook #'md-todo-apply-overlay-highlighting)


;; ------------------------------
;; 5. Dashboard (updated for new tags)
;; ------------------------------
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
Lists keyed by symbols 'todo, 'soon, 'later. Each item is a plist (:file FILE :line LINE :text TEXT).
Only matches upper-case tags at the beginning of a line (after optional Markdown list markers)."
  (let* ((todo-types '(("TODO"  . todo)
                       ("SOON"  . soon)
                       ("LATER" . later)
                       ("PERSO" . perso)))
         (results (list (cons 'todo  nil)
                        (cons 'soon  nil)
                        (cons 'later nil)
                        (cons 'perso nil))))
    (dolist (file (directory-files-recursively notes-dir "\\.md\\'"))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (let ((linenum 1))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties (line-beginning-position)
                                                        (line-end-position))))
              ;; Case-sensitive match
              (let ((case-fold-search nil))
                (dolist (tt todo-types)
                  ;; Only match at beginning of line after optional list marker
                  ;; List marker can be "-", "*", "+", or numbered "1." etc.
                  (when (string-match (format "^\\s-*\\(?:[*+-]\\|[0-9]+[.)]\\|#+\\)?\\s-*%s\\s-+\\(.*\\)"
                                            (car tt))
                                    line)
                    (push (list :file file :line linenum :text (match-string 1 line))
                          (alist-get (cdr tt) results))))))
            (forward-line 1)
            (setq linenum (1+ linenum))))))
    ;; reverse each list to preserve order
    (dolist (k '(todo soon later))
      (setf (alist-get k results) (nreverse (alist-get k results))))
    results))

(defun md--insert-section (items tag-name notes-dir max-col-width)
  "Insert a section for ITEMS with TAG-NAME; use NOTES-DIR and MAX-COL-WIDTH."
  (let ((header-start (point))
        (tag (upcase tag-name)))
    ;; Insert header line with trailing colon
    (insert (format "# %d items left in the %s list:" (length items) tag))
    (let ((header-end (point)))
      ;; Markdown header face
      (add-text-properties header-start header-end
                           '(section-header t md-folded nil face markdown-header-face-1))
      ;; Highlight the tag word like a TODO tag
      (let ((colors (cdr (assoc tag md-todo-colors))))
        (when colors
          (save-excursion
            (goto-char header-start)
            (when (re-search-forward (regexp-quote tag) header-end t)
              (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                (overlay-put ov 'face `(:background ,(plist-get colors :bg)
                                                   :foreground ,(plist-get colors :fg)
                                                   :weight bold))
                (overlay-put ov 'md-todo-overlay t))))))))
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
          (notes-dir md-todo-folder)
          (max-col-width 35))
      (erase-buffer)
      (let ((todo-data (md--collect-todos notes-dir)))
        (md--insert-section (alist-get 'todo todo-data) "TODO" notes-dir max-col-width)
        (md--insert-section (alist-get 'soon todo-data) "SOON" notes-dir max-col-width)
        (md--insert-section (alist-get 'later todo-data) "LATER" notes-dir max-col-width)
        (md--insert-section (alist-get 'perso todo-data) "PERSO" notes-dir max-col-width))
      (goto-char (point-min)))))

(defun md-todo-dashboard ()
  "Display a categorized TODO dashboard for all Markdown notes in ~/Public/Notes."
  (interactive)
  (let* ((buffer (get-buffer-create "*TODO Dashboard*"))
         (notes-dir md-todo-folder)
         (todo-data (md--collect-todos notes-dir))
         (max-col-width 35))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (md-todo-dashboard-mode)
        (setq truncate-lines t)
        (md--insert-section (alist-get 'todo todo-data) "TODO" notes-dir max-col-width)
        (md--insert-section (alist-get 'soon todo-data) "SOON" notes-dir max-col-width)
        (md--insert-section (alist-get 'later todo-data) "LATER" notes-dir max-col-width)
        (md--insert-section (alist-get 'perso todo-data) "PERSO" notes-dir max-col-width)))
    (display-buffer-in-side-window buffer '((side . top) (slot . 0) (window-height . 0.3)))
    (select-window (get-buffer-window buffer))
    (goto-char (point-min))))
