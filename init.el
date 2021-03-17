(setq package-archives 
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        )
      package-archive-priorities
      '(("org"          . 20)
        ("melpa"        . 10)
        ("gnu"          . 5)
        ("melpa-stable" . 0)
        ))

(package-initialize)
(setq package-enable-at-startup nil)

(setq package-list '(
                     2048-game
                     anzu
                     apropospriate-theme
                     auctex
                     auctex-latexmk
                     bonjourmadame
                     buffer-move
                     centered-window
                     circadian
                     ;; col-highlight    ; EmacsWiki not on MELPA anymore
                     company
                     company-auctex
                     company-reftex
                     company-math
                     counsel
                     counsel-world-clock
                     color-theme-sanityinc-tomorrow 
                     dimmer
                     dired-narrow
                     dired-quick-sort
                     dired-single
                     ebib
                     eshell-git-prompt
                     ess
                     expand-region
                     flycheck
                     flx
                     forge
                     format-sql
                     highlight 
                     highlight-indent-guides
                     ivy-bibtex
                     ivy-prescient
                     ivy-rich
                     magit
                     magit-gitflow
                     markdown-mode
                     markdown-toc
                     move-text
                     multiple-cursors
                     neotree
                     nord-theme
                     org-bullets
                     org-plus-contrib
                     pandoc-mode
                     pdf-tools
                     polymode
                     poly-R
                     poly-markdown
                     poly-noweb
                     poly-org
                     powerthesaurus
                     smex
                     sql-indent
                     sqlup-mode
                     string-inflection
                     sudden-death
                     tango-plus-theme
                     toc-org
                     web-mode
                     writeroom-mode
                     xkcd
                     yaml-mode
                     ))
;; Markdown-mode from MELPA, and not MELPA stable
;; (setq package-pinned-packages
;;       '((markdown-mode         . "melpa")
;;         (ivy-bibtex            . "melpa")))

(unless package-archive-contents
  (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(setq ad-redefinition-action 'accept)

(add-to-list 'load-path (concat user-emacs-directory "functions"))

(setq inhibit-startup-screen t)

(global-set-key [(ctrl shift f1)] 'menu-bar-mode)

(setq my-cache-dir (concat user-emacs-directory "cache/"))

(setq nord-comment-brightness 20)
;; (load-theme 'nord t)
;; Montpellier
(setq calendar-location-name "Montpellier, FR")
(setq calendar-latitude 43.6108)
(setq calendar-longitude 3.8767)
(setq circadian-themes '((:sunrise . tango-plus)
                         (:sunset  . nord)))
(add-hook 'circadian-before-load-theme-hook
          #'(lambda (theme)
              (setq custom-face-attributes '())))
(circadian-setup)

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b"))

;; (setq split-height-threshold 20)
;; (setq split-width-threshold 100)

(defun my-split-window-sensibly (&optional window)
  (let ((window (or window (selected-window))))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (with-selected-window window
               (split-window-right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (with-selected-window window
               (split-window-below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding
             ;; the value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (with-selected-window window
                   (split-window-right))))))))

(setq split-window-preferred-function 'my-split-window-sensibly)

(dimmer-activate)
(setq dimmer-percent 0.15)

(windmove-default-keybindings 'meta)

(global-set-key [C-dead-grave] 'other-window)

(global-set-key (kbd "C-x <up>")     'buf-move-up)
(global-set-key (kbd "C-x <down>")   'buf-move-down)
(global-set-key (kbd "C-x <left>")   'buf-move-left)
(global-set-key (kbd "C-x <right>")  'buf-move-right)

(setq mouse-wheel-progressive-speed nil)

(setq scroll-margin 2)

(global-set-key (kbd "<mouse-7>")
                (lambda () (interactive)))
(global-set-key (kbd "<mouse-6>")
                (lambda () (interactive)))

(setq visible-bell t)

(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(centered-window-mode t)

(setq initial-scratch-message nil)

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))

(global-set-key [(f10)] 'toggle-truncate-lines)

(global-hl-line-mode)
(global-set-key [(ctrl f10)] 'global-hl-line-mode)

(global-set-key [(ctrl shift f10)] 'column-highlight-mode)

;; (set-face-attribute 'highlight nil 
;;                   :background "saddle brown")
(global-set-key [(f9)] 'hlt-highlight)
(global-set-key [(ctrl f9)] 'hlt-next-highlight)
(global-set-key [(ctrl shift f9)] 'hlt-unhighlight-region)

(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character) ; use 'column for more visible guides

(global-set-key (kbd "C-+") 'count-words)

(global-set-key (kbd "C-x k") 'kill-this-buffer)

(setq recenter-positions '(top middle bottom))

(global-set-key (kbd "C-@") 'er/expand-region)

(save-place-mode 1)
(setq save-place-file (concat my-cache-dir "saved-places"))

(global-set-key [(f1)]
                (lambda () (interactive) (jump-to-register 1 nil)))
(global-set-key [(ctrl f1)]
                (lambda () (interactive) (point-to-register 1 nil)))
(global-set-key [(f2)]
                (lambda () (interactive) (jump-to-register 2 nil)))
(global-set-key [(ctrl f2)]
                (lambda () (interactive) (point-to-register 2 nil)))
(global-set-key [(f3)]
                (lambda () (interactive) (jump-to-register 3 nil)))
(global-set-key [(ctrl f3)]
                (lambda () (interactive) (point-to-register 3 nil)))
(global-set-key [(f4)]
                (lambda () (interactive) (jump-to-register 4 nil)))
(global-set-key [(ctrl f4)]
                (lambda () (interactive) (point-to-register 4 nil)))

(global-set-key (kbd "C-M-=") #'imenu)
(setq imenu-auto-rescan t)
;; (global-set-key [mouse-3] 'imenu)

(setq bookmark-default-file (concat my-cache-dir "bookmarks"))
(global-set-key [(ctrl shift f3)] 'bookmark-set)
(global-set-key [(ctrl shift f4)] 'list-bookmarks)

(column-number-mode 1)

(global-anzu-mode 1)
(anzu-mode 1)
(global-set-key (kbd "C-r") 'anzu-query-replace)
(global-set-key (kbd "C-M-r") 'anzu-query-replace-regexp)

(setq-default mode-line-format '(
                                 "%e"  ; Error message about full memory
                                 mode-line-front-space
                                 "%* " ; Modified or read-only buffer
                                 ;; mode-line-frame-identification
                                 mode-line-buffer-identification
                                 "      "
                                 '(vc-mode vc-mode)
                                 "  "
                                 mode-line-remote ; Remote file?
                                 "      "
                                 ;; mode-line-modes ; This includes minor modes
                                 "%m" ; Only major mode
                                 "      "
                                 mode-line-position
                                 ;; "%l:%c (%p)" ; line number : column number (percent) 
                                 (does not work with PDF mode)
                                 mode-line-misc-info ; Not sure…
                                 mode-line-end-spaces
                                 ))

(fset 'yes-or-no-p 'y-or-n-p)

(ivy-mode 1)
(ivy-prescient-mode 1)
(counsel-mode 1)

(setq
 max-mini-window-height 0.30
 ivy-use-virtual-buffers t
 ivy-count-format "(%d/%d) "
 ivy-use-selectable-prompt t
 ivy-re-builders-alist '(
                         ;; (swiper . ivy--regex-plus)
                         (counsel-M-x . ivy--regex-fuzzy)
                         (read-file-name-internal . ivy--regex-fuzzy)
                         (t . ivy--regex-plus))
 ivy-initial-inputs-alist nil)

(global-set-key (kbd "C-S-s") 'ivy-resume)

(global-set-key (kbd "<C-tab>") 'ivy-switch-buffer)

(ivy-rich-mode 1)  
(setq ivy-virtual-abbreviate 'full
      ivy-rich-switch-buffer-align-virtual-buffer t
      ivy-rich-path-style 'abbrev)

(setq smex-save-file (concat my-cache-dir "smex-items"))

(setq counsel-find-file-at-point t)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-x C-o") 'find-file-other-window)
(global-set-key (kbd "C-S-y") 'counsel-yank-pop)
;; (global-set-key (kbd "<f1> f") 'counsel-describe-function)
;; (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
;; (global-set-key (kbd "<f1> l") 'counsel-find-library)
;; (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
;; (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
;; (global-set-key (kbd "C-c g") 'counsel-git)
;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
;; (global-set-key (kbd "C-c k") 'counsel-ag)
;; (global-set-key (kbd "C-x l") 'counsel-locate)
;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)

(global-set-key (kbd "C-s") 'swiper)

(setq-default indent-tabs-mode nil)

(global-set-key (kbd "C-z") 'undo)

(setq cua-rectangle-mark-key (kbd "C-S-RET"))
(cua-selection-mode t)
(global-set-key [(ctrl shift return)] 'cua-set-rectangle-mark)

(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\` . ?\`)
                            (?\( . ?\))
                            (?\[ . ?\])
                            (?\{ . ?\})
                            ) )

(setq show-paren-delay 0)
(show-paren-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(global-set-key (kbd "C-c q") 'auto-fill-mode)

(add-hook 'prog-mode-hook 'subword-mode)

(global-set-key (kbd "C-c C-u") 'string-inflection-custom-cycle)
(setq string-inflection-skip-backward-when-done t)

(defun string-inflection-custom-cycle ()
  "foo_bar => fooBar => foo-bar => foo_bar"
  (interactive)
  (string-inflection-insert
   (string-inflection-custom-cycle-function (string-inflection-get-current-word))))

(fset 'string-inflection-cycle 'string-inflection-custom-cycle)

(defun string-inflection-custom-cycle-function (str)
  "foo_bar => fooBar => foo-bar => foo_bar"
  (cond
   ((string-inflection-underscore-p str)
    (string-inflection-lower-camelcase-function str))
   ((string-inflection-lower-camelcase-p str)
    (string-inflection-kebab-case-function str))
   (t
    (string-inflection-underscore-function str))))

(add-hook 'after-init-hook 'global-company-mode)
(with-eval-after-load 'company
  (define-key company-active-map (kbd "TAB") #'company-complete-common)
  (define-key company-active-map [tab] #'company-complete-common))
(setq company-idle-delay 0
      company-echo-delay 0
      company-dabbrev-downcase nil
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-transformers '(company-sort-by-occurrence
                             company-sort-by-backend-importance))

(setq mc/list-file (concat my-cache-dir "mc-lists.el"))
(global-set-key [(f11)] 'mc/edit-lines)
(global-set-key [(ctrl f11)] 'mc/mark-all-dwim)
(global-set-key [(ctrl shift f11)] 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(load-library "sexp-eval-and-replace")
(global-set-key (kbd "C-x C-y") 'sexp-eval-and-replace)

(global-set-key [M-S-down] 'move-text-down)
(global-set-key [M-S-up]   'move-text-up)

(global-set-key (kbd "C-c C-d") 'sudden-death)

(setq ispell-program-name "hunspell"
      ispell-local-dictionary "en_US")

(add-hook 'text-mode-hook 'turn-on-flyspell)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(global-set-key [f12] 'flyspell-correct-word-before-point)
(global-set-key [C-f12] 'flyspell-mode) ; + flyspell-buffer when on!
(global-set-key [C-S-f12] 'ispell-change-dictionary)

(global-set-key (kbd "C-'") 'powerthesaurus-lookup-word-dwim)

(setq current-language-environment "UTF-8")

(setq create-lockfiles nil)

(setq
 auto-save-file-name-transforms `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                                   ,(concat my-cache-dir "save/\\2") t))
 auto-save-list-file-name (concat my-cache-dir "auto-save-list")
 auto-save-interval 100
 auto-save-timeout 10)

(setq
 backup-directory-alist `((".*" . ,(concat my-cache-dir "save/")))
 backup-by-copying t
 version-control t
 kept-new-versions 2
 kept-old-versions 2
 delete-old-versions t)

(setq recentf-save-file (expand-file-name "recentf" my-cache-dir))

(setq abbrev-file-name (concat my-cache-dir "abbrev_defs"))
(setq save-abbrevs 'silently)

(setq
 dired-listing-switches "-aBhl  --group-directories-first"
 ;; dired-omit-files "^\\.$"
 dired-auto-revert-buffer t
 dired-dwim-target t
 dired-recursive-copies (quote always)
 dired-recursive-deletes (quote always))
(global-set-key (kbd "<f6>")
                (lambda ()
                  (interactive)
                  (dired ".")))
(add-hook 'dired-mode-hook 'auto-revert-mode)
(eval-after-load "dired"
  '(progn
     (load-library "dired-ediff-files")
     (hl-line-mode)
     (define-key dired-mode-map [(ctrl f6)] #'dired-toggle-read-only)
     (define-key dired-mode-map "/" 'dired-narrow)
     (define-key dired-mode-map "e" 'dired-ediff-files)
     ))

(eval-after-load "dired"
  '(progn
  (define-key dired-mode-map [return] 'dired-single-buffer)
  ;; (define-key dired-mode-map [mouse-1] 'dired-single-buffer-mouse) ; Does not work
  (define-key dired-mode-map [remap dired-mouse-find-file-other-window]
    #'dired-single-buffer-mouse)
  (define-key dired-mode-map "^" 'dired-single-up-directory)
  (define-key dired-mode-map [(backspace)] 'dired-single-up-directory)
  ))
(dired-quick-sort-setup)

(setq-default dired-omit-files-p t)
(setq
 dired-omit-verbose nil
 dired-omit-files "^\\.$"
 dired-omit-extensions nil)

(setq neo-theme 'ascii)
(global-set-key [(f5)] 'neotree-toggle)
;; (define-key neotree-mode-map (kb "RET")
;;   (neotree-make-executor
;;    :file-fn 'neo-open-file
;;    :dir-fn 'neo-open-dir))
(add-hook 'neo-after-create-hook
          #'(lambda (_)
              (with-current-buffer (get-buffer neo-buffer-name)
                (setq truncate-lines t))))

(setq
 tramp-persistency-file-name (concat my-cache-dir "tramp")
 tramp-completion-reread-directory-timeout nil
 tramp-histfile-override nil
 )
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(load-library "magit-repository-directories")
(shell-command "git config --global status.showUntrackedFiles all") ; List files in folders
(global-set-key [(f8)] 'magit-status)
;; (setq vc-handled-backends (delq 'Git vc-handled-backends)) ; Remove Git from the list of backends handled by Emacs version control
;; (setq vc-handled-backends nil) ; Remove VC altogether
(setq
 transient-history-file (concat my-cache-dir "transient/history.el")
 magithub-dir (concat my-cache-dir "magithub/")
 magit-completing-read-function 'ivy-completing-read
 magit-view-git-manual-method 'man
 magit-refs-show-commit-count 'all)
(with-eval-after-load 'magit
  (load-library "magit-ls-files")
  ;; (setq magit-repolist-columns
  ;;       '(("Name" 25 magit-repolist-column-ident nil)
  ;;         ("Version" 25 magit-repolist-column-version nil)
  ;;         ("D" 1 magit-repolist-column-dirty nil)
  ;;         ("B<U" 3 magit-repolist-column-unpulled-from-upstream
  ;;          ((:right-align t)))
  ;;         ("B>U" 3 magit-repolist-column-unpushed-to-upstream
  ;;          ((:right-align t)))
  ;;         ("Path" 99 magit-repolist-column-path nil)))
  
  ;; (setcdr (cdr magit-repolist-columns)
  ;;         (cons '("D" 1 magit-repolist-column-dirty nil)
  ;;               (cddr magit-repolist-columns)))
  (require 'forge)
  (setq forge-database-file (expand-file-name "forge-database.sqlite" my-cache-dir))
  (define-key magit-mode-map (kbd "K") 'magit-ls-files)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status))
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
(with-eval-after-load 'magit-mode
  ;; C-tab is for ivy-switch-buffer
  (define-key magit-mode-map [C-tab] nil)
  ;; (magithub-feature-autoinject t)
  )

(setq
 eshell-buffer-shorthand t
 eshell-cmpl-ignore-case t)
(eshell-git-prompt-use-theme 'powerline)

(with-eval-after-load 'shell
  (define-key shell-mode-map (kbd "C-l")
    (lambda (seq) (interactive "k") (process-send-string nil seq))))

(load-library "shell-xterm")
(global-set-key [(ctrl f8)] 'shell-xterm)

(add-to-list 'auto-mode-alist '("\\.pdf" . pdf-tools-install))
(setq-default pdf-view-display-size 'fit-page) ; Start PDF in full page
(setq pdf-annot-activate-created-annotations t) ; Automatically annotate highlights
(add-hook 'pdf-view-mode-hook 
          '(lambda ()
             (pdf-misc-size-indication-minor-mode) ; Show Top/Bot number in mode line?
             ;; (pdf-links-minor-mode)                ; Activate links
             (pdf-isearch-minor-mode)              ; Incremental search using normal isearch
             (define-key pdf-view-mode-map (kbd "h") 'pdf-view-fit-height-to-window) ; Fit height with 'h'
             (define-key pdf-view-mode-map (kbd "w") 'pdf-view-fit-width-to-window) ; Fit width with 'w'
             (define-key pdf-view-mode-map (kbd "f") 'pdf-view-fit-page-to-window) ; Fit page with 'f' DOES NOT WORK!
             ;; Conflict with Pdf-Links minor mode, which uses 'f' for link search
             (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward) ; bound to `C-s`
             ;; (cua-mode 0) ; Turn off CUA so copy works
             (define-key pdf-view-mode-map (kbd "M-w") 'pdf-view-kill-ring-save) ; Use normal isearch
             (define-key pdf-view-mode-map (kbd "<C-home>") 'pdf-view-first-page) ; First page with C-Home
             (define-key pdf-view-mode-map (kbd "<C-end>") 'pdf-view-last-page))) ; Last page with C-End

(with-eval-after-load 'eww
  (define-key eww-mode-map "f" 'eww-browse-with-external-browser)
  (define-key eww-mode-map [backspace] 'eww-back-url))

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-switchb)
(setq
 org-replace-disputed-keys t
 org-return-follows-link t)

(setq org-startup-indented 1)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(add-hook 'org-mode-hook 'toc-org-enable)

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((css . t)
     (ditaa . t)
     (emacs-lisp . t)
     (latex . t)
     (lilypond . t)
     (org . t)
     (shell . t)
     (sql . t)
     (R . t))))

(with-eval-after-load 'org
  ;; C-tab is for ivy-switch-buffer
  (define-key org-mode-map (kbd "<C-tab>") nil)
  ;; Prevent Org from overriding the bindings for windmove.
  (define-key org-mode-map (kbd "M-<left>") nil)
  (define-key org-mode-map (kbd "M-<right>") nil)
  (define-key org-mode-map (kbd "M-<up>") nil)
  (define-key org-mode-map (kbd "M-<down>") nil))
;; (define-key org-agenda-mode-map (kbd "M-<up>") nil)
;; (define-key org-agenda-mode-map (kbd "M-<down>") nil)
;; (define-key org-agenda-mode-map (kbd "M-<left>") nil)
;; (define-key org-agenda-mode-map (kbd "M-<right>") nil)

;; Add replacements for the some of keybindings we just removed. It
;; looks like Org already binds C-up and C-down separately from M-{
;; and M-}, so we can't use those. Users will just have to make do
;; with C-c <up> and C-c <down> for now.
;;
;; Now for Org Agenda on the other hand, we could use C-up and
;; C-down because M-{ and M-} are bound to the same commands. But I
;; think it's best to take the same approach as before, for
;; consistency.
;; (define-key org-mode-map (kbd "C-<left>") #'org-shiftleft)
;; (define-key org-mode-map (kbd "C-<right>") #'org-shiftright)
;; (define-key org-agenda-mode-map (kbd "C-<left>") #'org-agenda-do-date-earlier)
;; (define-key org-agenda-mode-map (kbd "C-<right>") #'org-agenda-do-date-later))

(setq org-ditaa-jar-path (expand-file-name "/usr/share/ditaa/ditaa.jar"))

(require 'ox-taskjuggler)

(setq TeX-parse-self t                ; Enable parse on load.
      TeX-auto-save t                 ; Enable parse on save.
      TeX-auto-local ".auctex-auto"   ; Parsed information saved in .auctex-auto
      TeX-style-local ".auctex-style" ; Hand-generated information saved in .auctex-style
      TeX-source-correlate-mode t	; Forward and inverse search with Synctex
      TeX-clean-confirm nil ; Don't ask for confirmation to clean intermediary files
      reftex-plug-into-AUCTeX t       ; Plug RefTeX to AUCTeX
      reftex-default-bibliography '("/home/mathieu/Work/Bibliography/BiblioMB.bib") ; Default bib
      TeX-auto-untabify t             ; Replace Tabs by spaces on save
      )
(add-hook 'LaTeX-mode-hook 
          (lambda ()
            (TeX-global-PDF-mode t)   ; Compile as PDF
            (add-to-list 'TeX-command-list '("XeLaTeX" "%`xelatex%(mode)%' %t" TeX-run-TeX nil t))
            (LaTeX-math-mode)         ; Math mode
            (turn-on-reftex)          ; RefTeX on
            (outline-minor-mode 1)	; Fold LaTeX sections
            (TeX-fold-mode 1)         ; Fold LaTeX environments
            ))

(setq auctex-latexmk-inherit-TeX-PDF-mode t) ; LaTeXMk inherits PDF mode 
(with-eval-after-load "tex"
  (auctex-latexmk-setup))
(add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "LatexMk")))

(with-eval-after-load "tex"
  (company-auctex-init)
  )

(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(require 'ivy-bibtex)
(setq
 bibtex-completion-bibliography '("/home/mathieu/Work/Bibliography/BiblioMB.bib")
 bibtex-completion-library-path '("/home/mathieu/Work/Bibliography/PDF/")
 bibtex-completion-find-additional-pdfs t
 bibtex-completion-pdf-extension '(".pdf" ".djvu" ".ps" ".epub" ".mobi" ".zip")
 )

(defun bibtex-completion-open-pdf-external (keys &optional fallback-action)
  (let ((bibtex-completion-pdf-open-function
         (lambda (fpath) (start-process "evince" "*helm-bibtex-evince*" "/usr/bin/evince" fpath))))
    (bibtex-completion-open-pdf keys fallback-action)))

(ivy-bibtex-ivify-action bibtex-completion-open-pdf-external ivy-bibtex-open-pdf-external)

(ivy-add-actions
 'ivy-bibtex
 '(("P" ivy-bibtex-open-pdf-external "Open PDF file in external viewer (if present)")))

(setq bibtex-completion-additional-search-fields
      '(keywords journal booktitle)
      bibtex-completion-find-additional-pdfs t
      bibtex-completion-display-formats
      '(
        (article        . "${=has-pdf=:1} ${author:36} ${title:*} ${year:4} ${journal:40}")
        (book           . "${=has-pdf=:1} ${author:36} ${title:*} ${year:4} Book: ${booktitle:34}")
        (inbook         . "${=has-pdf=:1} ${author:36} ${title:*} ${year:4} Book: ${booktitle:34}")
        (incollection   . "${=has-pdf=:1} ${author:36} ${title:*} ${year:4} Book: ${booktitle:34}")
        (inproceedings  . "${=has-pdf=:1} ${author:36} ${title:*} ${year:4} Book: ${booktitle:34}")
        (t              . "${=has-pdf=:1} ${author:36} ${title:*} ${year:4} Type: ${=type=:34}")))

(advice-add 'bibtex-completion-candidates
            :filter-return 'reverse)

(global-set-key (kbd "C-c b") 'ivy-bibtex)

(defun current-date ()
  (format-time-string "%Y.%m.%d"))
(defun bibtex-add-date-owner ()
  ;; Tyler https://emacs.stackexchange.com/users/262/tyler
  ;; https://emacs.stackexchange.com/a/46339
  "Adds a timestamp and owner field to a bibtex entry.
  Checks to make sure it doesn't exist first."
  (interactive)
  (save-excursion
    (bibtex-beginning-of-entry)
    (if (assoc "timestamp" (bibtex-parse-entry))
        (message "timestamp already exists!")
      (bibtex-make-field '("timestamp" nil current-date) t nil))
    (bibtex-beginning-of-entry)
    (if (assoc "owner" (bibtex-parse-entry))
        (message "owner already exists!")
      (bibtex-make-field '("owner" nil user-login-name) t nil))
    ))
(setq
 bibtex-entry-format '(opts-or-alts required-fields numerical-fields whitespace realign unify-case sort-fields) ; Clean optional fields, remove brackets around numerical fields, remove white space, realign, unify case of entry type and fields, sort fields in predefined order
 bibtex-align-at-equal-sign t    ; Also align = sign
 bibtex-autokey-name-year-separator "_" ; Underscore between Name and Year
 bibtex-autokey-year-length 4           ; Year as YYYY
 bibtex-autokey-name-case-convert-function 'capitalize ; Name with capitale
 bibtex-autokey-titlewords 0                           ; No title
 bibtex-autokey-titleword-length 0                     ; No title
 bibtex-autokey-edit-before-use nil                    ; Don't edit before use
 bibtex-user-optional-fields '( ; Additional fields: DOI, url, date, owner, abstract
                               ("doi" "DOI for the entry")
                               ("url" "URL for the entry")
                               ("timestamp" "Time the entry was created" current-date)
                               ("owner" "Owner of the entry" user-login-name)
                               ("abstract" "Abstract for the entry"))
 )
(add-hook 'bibtex-clean-entry-hook 'bibtex-add-date-owner)
(setq biblio-cleanup-bibtex-function #'bibtex-clean-entry)

(setq
 markdown-command
 (concat ; Use Pandoc to convert Markdown to HTML, to produce a
         ; standalone HTML document rather than a snippet, to enable
         ; MathJax (to render LaTeX as MathML), and to use Pygments
         ; for syntax highlighting of code blocks
  "/usr/local/bin/pandoc"
  " --from=markdown --to=html"
  " --standalone --mathjax --highlight-style=pygments")
 markdown-asymmetric-header t           ; Asymetric headers (only # on the left)
 markdown-enable-math t                 ; Enable mathematical expressions (LaTeX)
 )

(setq ess-ask-for-ess-directory nil)

(setq inferior-R-args "--quiet --no-save")

(global-set-key (kbd "C-c r") 'ess-rdired)

(setq display-buffer-alist
      `(("*R Dired"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("*R"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . 1)
         (window-width . 0.5)
         (reusable-frames . nil)
         (dedicated . t))
        ("*Help"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("magit:"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("COMMIT_EDITMSG"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . right)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ("magit-diff:"
         (display-buffer-reuse-window display-buffer-in-side-window)
         (side . left)
         (slot . -1)
         (window-width . 0.5)
         (reusable-frames . nil))
        ))

(setq ess-auto-width 'window)

(setq ess-eval-visibly 'nowait)

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook 
            (lambda ()
              (define-key ess-r-mode-map (kbd "C-c C-x")
                #'polymode-eval-chunk)
              (define-key inferior-ess-r-mode-map (kbd "C-c C-x")
                #'polymode-eval-chunk))))

(setq ess-default-style 'OWN)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-own-style-list
   (quote
    ((ess-indent-offset . 4)
     (ess-indent-from-lhs)
     (ess-indent-from-chain-start)
     (ess-indent-with-fancy-comments . t)
     (ess-offset-arguments . prev-line)
     (ess-offset-arguments-newline . prev-line)
     (ess-offset-block . prev-line)
     (ess-offset-continued . straight)
     (ess-align-nested-calls)
     (ess-align-arguments-in-calls)
     (ess-align-continuations-in-calls . prev-line)
     (ess-align-blocks control-flow))))
 '(ess-roxy-str "#'")
 '(package-selected-packages
   (quote
    (forge yaml-mode xkcd writeroom-mode web-mode toc-org tango-plus-theme sudden-death string-inflection sqlup-mode sql-indent smex powerthesaurus poly-org poly-R pdf-tools pandoc-mode org-plus-contrib org-bullets nord-theme neotree multiple-cursors move-text markdown-toc magit-gitflow ivy-rich ivy-prescient ivy-bibtex highlight-indent-guides highlight format-sql flycheck flx expand-region ess eshell-git-prompt ebib dired-single dired-quick-sort dired-narrow dimmer counsel-world-clock counsel company-reftex company-math company-auctex color-theme-sanityinc-tomorrow circadian centered-window buffer-move bonjourmadame auctex-latexmk apropospriate-theme anzu 2048-game))))

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook
            (lambda ()
            (load-library "ess-indent-region-r")
            (set (make-local-variable 'indent-region-function)
               'ess-indent-region-with-styler))))

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook
            '(lambda()
               (add-hook 'write-contents-functions
                         (lambda ()
                           (ess-nuke-trailing-whitespace)))
               (setq ess-nuke-trailing-whitespace-p t))))

(setq ess-use-ido nil)

(setq ess-assign-list '(" <- " " %>% " " -> "))
(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook 
            (lambda ()
              (define-key ess-r-mode-map (kbd "C-=") #'ess-cycle-assign)
              (define-key inferior-ess-r-mode-map (kbd "C-=") #'ess-cyle-assign))))

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook
            (lambda ()
              (prettify-symbols-mode))))

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook 
            (lambda ()
              (flyspell-prog-mode))))

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook 
            (lambda ()
            (highlight-indent-guides-mode))))

(with-eval-after-load "ess" 
  (add-hook 'ess-mode-hook 
            (lambda ()
              (subword-mode))))

(setq ess-swv-plug-into-auctex-p t)

(add-hook 'markdown-mode-hook 
          (lambda ()
            (load-library "ess-rmd-library")
            (local-set-key [C-return] 'ess-rmd-fenced-r-code-block)
            (local-set-key [C-S-return] 'ess-rmd-inline-r-code)
            ))

(with-eval-after-load 'polymode
  (define-key polymode-mode-map [(f7)] #'ess-rmd-render)
  (define-key polymode-mode-map [(ctrl f7)] #'ess-rmd-bookdown)
  (define-key polymode-mode-map [(shift ctrl f7)] #'ess-md-pandoc))

(add-hook 'sql-mode-hook
          (lambda ()
            (load-library "sql-library")
            (local-set-key (kbd "<C-return>") 'sql-send-region-or-paragrap)))

(with-eval-after-load 'sql (load-library "sql-indent"))

(add-hook 'sql-mode-hook 'sqlup-mode)
(add-hook 'sql-interactive-mode-hook 'sqlup-mode)
(add-hook 'sql-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c u") 'sqlup-capitalize-keywords-in-region)))

(add-hook 'sql-mode-hook
          (lambda ()
            (local-set-key (kbd "C-M-]") 'format-sql-region)))

(setq sql-postgres-login-params
      '((server :default "localhost")
        (port :default 5432)
        (user :default "mathieu")
        (database :default "test")))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            ;; (toggle-truncate-lines t)))
            (setq truncate-lines t)))

(add-to-list 'auto-mode-alist '("\\.htm?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.shtml?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinga\\'" . web-mode))
(setq
 web-mode-enable-auto-pairing t
 web-mode-enable-css-colorization t
 web-mode-engines-alist
 '(("php"    . "\\.phtml\\'")
   ("django" . "\\.jinja\\'")
   ("blade"  . "\\.blade\\.")))

(defun web-mode-flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face))
         rlt)
    (cond
     ;; Check the words with these font faces, possibly.
     ;; this *blacklist* will be tweaked in next condition
     ((not (memq f '(web-mode-html-attr-value-face
                     web-mode-html-tag-face
                     web-mode-html-attr-name-face
                     web-mode-constant-face
                     web-mode-doctype-face
                     web-mode-keyword-face
                     web-mode-comment-face ;; focus on get html label right
                     web-mode-function-name-face
                     web-mode-variable-name-face
                     web-mode-css-property-name-face
                     web-mode-css-selector-face
                     web-mode-css-color-face
                     web-mode-type-face
                     web-mode-block-control-face)))
      (setq rlt t))
     ;; check attribute value under certain conditions
     ((memq f '(web-mode-html-attr-value-face))
      (save-excursion
        (search-backward-regexp "=['\"]" (line-beginning-position) t)
        (backward-char)
        (setq rlt (string-match "^\\(value\\|class\\|ng[A-Za-z0-9-]*\\)$"
                                (thing-at-point 'symbol)))))
     ;; finalize the blacklist
     (t
      (setq rlt nil)))
    rlt))
(put 'web-mode 'flyspell-mode-predicate 'web-mode-flyspell-verify)

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-markdown+r-mode))
(add-to-list 'auto-mode-alist '("\\.[sS]nw\\'" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.[rR]nw\\'" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . poly-org-mode))

(with-eval-after-load 'polymode
  (define-key polymode-mode-map [(C-prior)] #'polymode-previous-chunk)
  (define-key polymode-mode-map [(C-next)] #'polymode-next-chunk)
  (define-key polymode-mode-map [(C-S-prior)] #'polymode-previous-chunk-same-type)
  (define-key polymode-mode-map [(C-S-next)] #'polymode-next-chunk-same-type))

(setq xkcd-cache-dir (concat my-cache-dir "xkcd"))

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
  tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
