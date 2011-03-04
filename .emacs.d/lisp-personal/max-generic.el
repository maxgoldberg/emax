;;
;; Generic emacs stuff that's important to me.
;;

;;;;
;; NOTE:
;;
;; For the best experience, use the following keymap for SecureCRT:
;;

;; CEVK_END              "\033[1;5F"
;; CEVK_HOME             "\033[1;5H"
;; CEVK_LEFT             "\033[1;5D"
;; CEVK_UP               "\033[1;5A"
;; CEVK_RIGHT            "\033[1;5C"
;; CEVK_DOWN             "\033[1;5B"
;; SEVK_END              "\033[1;2F"
;; SEVK_HOME             "\033[1;2H"
;; SEVK_LEFT             "\033[1;2D"
;; SEVK_UP               "\033[1;2A"
;; SEVK_RIGHT            "\033[1;2C"
;; SEVK_DOWN             "\033[1;2B"
;; CSEVK_END             "\033[1;6F"
;; CSEVK_HOME            "\033[1;6H"
;; CSEVK_LEFT            "\033[1;6D"
;; CSEVK_UP              "\033[1;6A"
;; CSEVK_RIGHT           "\033[1;6C"
;; CSEVK_DOWN            "\033[1;6B"
;; AEVK_LEFT             "\033\033[D"
;; AEVK_UP               "\033\033[A"
;; AEVK_RIGHT            "\033\033[C"
;; AEVK_DOWN             "\033\033[B"
;; SEVK_PRIOR            "\033[5$"
;; SEVK_NEXT             "\033[6$"

;;
;; and add:
;;   alias emacs='TERM=xterm-vt220 emacs -Q -nw'
;;
;; to your .bashrc
;;
;; This will allow you to select regions using the shift key,
;; and is also required to make drag-stuff mode work.
;;

;;;;;
;;
;; Emacs training wheels :)
;;

;;
;; Don't allow accidental `M-~' to mark buffer as unmodified.
;;

(put 'not-modified 'disabled t)

;;
;; Turn off upcase-region and downcase-region with C-x C-u and C-x C-l
;;

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;
;; Turn off erase-buffer.
;;

(put 'erase-buffer 'disabled nil)

;;
;; Turn off dired's buffer replacement.
;;

(put 'dired-find-alternate-file 'disabled nil)


;;;;;
;;
;; Auto-save and backup options.
;;
;; I like backup files, and while it adds a lot of clutter, they're easy to clean up.
;;

(setq make-backup-files t)
(setq auto-save-default t)

;; Even on version controlled files

(setq vc-make-backup-files t)

;; Copy symlinks instead of breaking them.

(setq backup-by-copying t)

;;
;; save all backups in ~/.emacs.d/backups you can change this by file type/path etc.
;;
;; e.g. (setq backup-directory-alist '(("\\.conf$" . "~/.emacs.d/backups/config_files")))
;;

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Automatically clean up old backups

(setq delete-old-versions t)

;; How many backups to keep before we clean up.

(setq kept-new-versions 6)

;; How many to clean up.

(setq kept-old-versions 2)

;; Use numbered backup files to keep track of what "version" a file is.

(setq version-control t)


;;;;;
;;
;;
;; Misc options.
;;

;; Use "y or n" answers instead of full words "yes or no"

(fset 'yes-or-no-p 'y-or-n-p)

;;
;; PC select mode on.
;;
;; Add custom keybindings to allow for shift+pageup and pagedown.
;; see max-generic.el for keymap info.
;;
;; Also add C-x g for goto line which I can never remember.
;;

(require 'pc-select)

(global-set-key (kbd "M-[ 5 $") 'scroll-down-mark)
(global-set-key (kbd "M-[ 6 $") 'scroll-up-mark)
(global-set-key (kbd "C-x g") 'goto-line)

;;
;; Make TAB + arrow keys move from window to window.
;; (disabled due to conflicts)
;;

;;(windmove-default-keybindings 'tab)


;;
;; Don't insert instructions in the *scratch* buffer
;;

(setq initial-scratch-message nil)


;; Make C-h a act as C-u C-h a

(setq apropos-do-all t)

;; For C-u C-x v d. Probably a good idea for everything else too

(setq completion-ignore-case t)

;; Ask me whether to add a final newline to files which don't have one

(setq require-final-newline 'ask)

;; Send deletions to the Trash folder - Emacs 23.2

(setq delete-by-moving-to-trash t)


;;;;;
;;
;; recentf - a list of recent files
;;
;;

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)


;;;;;;
;;
;;  User info
;;

(setq user-full-name "max goldberg")
(setq user-mail-address "max@ytmnd.com")

;; Used in ChangeLog entries

(setq add-log-mailing-address "max@ytmnd.com")

;;
;; When selecting a file to visit, // will mean / and ~ will mean $HOME regardless of preceding text.
;;

(setq file-name-shadow-tty-properties '(invisible t))
(file-name-shadow-mode 1)


;;
;; Spell check comments in programming modes :)
;;

(dolist (mode-hook '(c-mode-hook
                     perl-mode-hook
                     emacs-lisp-mode-hook
                     php-mode-hook
                     conf-mode-hook
                     html-mode-hook))
  (add-hook mode-hook 'flyspell-prog-mode))

;;
;; Spell check everything in other modes
;;

(dolist (mode-hook '(html-mode-mode-hook))
  (add-hook mode-hook 'flyspell-mode))

;;
;; Shell out, instead of suspend.
;;
;; I found this obnoxious.
;;

;(global-set-key (kbd "C-z") 'shell)


;; don't add lines when scrolling at the end of a buffer (dont think this is needed in newer versions)

(setq next-line-add-newlines nil)

;; Change the default behaivor of emacs23 default split to be like emacs22.
(setq split-width-threshold nil)
(setq-default split-width-threshold most-positive-fixnum)

;;
;;;; Minibuffer options.
;;

;; Minibuffer history length

(setq history-length 250)

;; Minibuffer completion mode

(icomplete-mode 99)

(defun trailing-whitespace () "remove trailing whitespace" (interactive)
  (replace-regexp "[ ]+\n" "\n"))


(provide 'max-generic)
