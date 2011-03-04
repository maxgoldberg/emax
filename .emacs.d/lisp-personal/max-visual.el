;;
;; Customization of how emacs looks.
;;

;;
;; Turn on visual bell (and prevent the audible bell)
;;

(setq visible-bell t)

;;
;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info (is this default now?)
;;

(setq transient-mark-mode t)

;;
;; Display line and column numbers
;;

(setq line-number-mode    t)
(setq column-number-mode  t)

;;
;; Enable color! (for older emacs)
;;

(add-hook 'shell-mode-hook (lambda ()
                             (ansi-color-for-comint-mode-on)))

;;
;; Syntax highlighting on by default
;;

(global-font-lock-mode t)

;;
;; emacs23 sets the minibuffer color to be dark blue (on black) by default. it's hard to see.
;;

(set-face-foreground 'minibuffer-prompt "white")

(provide 'max-visual)
