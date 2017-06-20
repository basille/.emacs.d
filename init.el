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

" nil "~/.Xresources" 'append)
(shell-command "xrdb -merge ~/.Xresources")

;; Create a starter for Emacs daemon
(mkdir "~/.config/systemd/user/")
(write-region "[Unit]
Description=GNU Emacs 25 (daemon)
Documentation=man:emacs(1) info:Emacs

[Service]
Type=forking
ExecStart=/usr/bin/emacs --daemon
ExecStop=/usr/bin/emacsclient --eval \"(progn (setq kill-emacs-hook nil) (kill-emacs))\"
Restart=always
#Environment=DISPLAY=:%i
TimeoutStartSec=0

[Install]
WantedBy=default.target

" nil "~/.config/systemd/user/emacs.service")
(shell-command "systemctl --user enable emacs")
(shell-command "systemctl --user start emacs")

;; Create a Desktop entry for Emacs client
(write-region "[Desktop Entry]
Version=1.0
Name=GNU Emacs 25 (client)
GenericName=Text Editor
Comment=GNU Emacs using the daemon
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Exec=/usr/bin/emacsclient --create-frame --alternate-editor "" %F
Icon=emacs25
Type=Application
Terminal=false
Categories=Utility;Development;TextEditor;
StartupWMClass=Emacs

" nil "~/.local/share/applications/emacsclient.desktop")


;; Create necessary directories and save abbrev silently
(mkdir (concat user-emacs-directory "cache"))
(mkdir (concat user-emacs-directory "functions"))
(mkdir (concat user-emacs-directory "save"))
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
