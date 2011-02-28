;;
;; max's .emacs
;;

;;
;; Load paths
;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;
;; Third party lisp
;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;
;; Personal lisp
;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))

;;
;; generic settings
;;

(require `max-generic)

;;
;; custom formatting/style/color settings
;;

(require `max-coding-style)

;;
;; custom major/minor editing modes.
;;

(require `max-modes)


;;
;; Make TAB + arrow keys move from window to window.
;;

;;(windmove-default-keybindings 'tab)

;;
;; PC select mode on.
;;
;; Add custom keybindings to allow for shift+pageup and pagedown.
;; see max-generic.el for keymap info.
;;

(require 'pc-select)
(global-set-key (kbd "M-[ 5 $") 'scroll-down-mark)
(global-set-key (kbd "M-[ 6 $") 'scroll-up-mark)
