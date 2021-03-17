;; This file replaces itself with the actual configuration at first run.

;; Add X options to ~/.Xresources and initialize them
(write-region "
! Emacs geometry
!
emacs.fullscreen: maximized
emacs.menuBar: off
emacs.toolBar: off
emacs.verticalScrollBars: off
emacs.horizontalScrollbars: off

" nil "~/.Xresources" 'append)
(shell-command "xrdb -merge ~/.Xresources")

;; Create necessary directories and save abbrev silently
(mkdir (concat user-emacs-directory "cache") t)
(mkdir (concat user-emacs-directory "functions") t)
(mkdir (concat user-emacs-directory "save") t)
(mkdir "~/.emacs.d/save/" 1)
(setq abbrev-file-name "~/.emacs.d/cache/abbrev_defs")
(setq save-abbrevs 'silently)

;; We can't tangle without org!
(require 'org)
;; Open the configuration
(find-file (concat user-emacs-directory "init.org"))
;; tangle it
(org-babel-tangle)
;; load it
(load-file (concat user-emacs-directory "init.el"))
;; finally byte-compile it
(byte-compile-file (concat user-emacs-directory "init.el"))  
