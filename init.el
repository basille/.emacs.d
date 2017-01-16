;; This file replaces itself with the actual configuration at first run.

;; Add X options to ~/.Xresources and initialize them
(write-region "
! Emacs geometry
!
emacs.fullscreen: maximized
emacs.geometry: 250x250
emacs.toolBar: off
emacs.verticalScrollBars: off
emacs.horizontalScrollbars: off
emacs25.fullscreen: maximized
emacs25.geometry: 250x250
emacs25.toolBar: off
emacs25.verticalScrollBars: off
emacs25.horizontalScrollbars: off

" nil "~/..Xresources" 'append)
(shell-command "xrdb -merge ~/.Xresources")

;; Create necessary directories and save abbrev silently
(mkdir "~/.emacs.d/cache/" 1)
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

