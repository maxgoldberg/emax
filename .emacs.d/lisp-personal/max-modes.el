;;
;; Mode specific emacs options.
;;

;;
;; show-wspace.el
;;
;; Highlight tabs and trailing whitspace so we can remove them.
;;

(require 'show-wspace)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-tabs)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-hard-spaces)
(add-hook 'font-lock-mode-hook 'show-ws-highlight-trailing-whitespace)

;;
;; psvn.el
;;
;; Provides full SVN support
;;

(require 'psvn)

;;
;; php-mode.el and php-lint-mode.el
;;
;; Provides PHP mode and forces a PHP CLI lint check before the buffer gets saved.
;; This is very good to avoid saving files with parse errors in them on production sites :)
;;
;;

(autoload 'php-mode "php" nil t)
(autoload 'php-lint "php-lint-mode" nil t)
(autoload 'php-lint-mode "php-lint-mode" nil t)

(add-hook 'php-mode-user-hook 'turn-on-font-lock)

(defvar php-lint-bin "/usr/bin/php" "The php binary used to check for lint.")

;; Automatically enable php-lint-mode for php-mode, comment this out if it becomes annoying.

(eval-after-load "php-mode" '(add-hook 'php-mode-hook 'php-lint-mode))

;;
;; List files which files should automatically be in php-mode.
;;

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . php-mode))

;;
;; drag-stuff.el
;;
;; Drag stuff mode. Allows you to drag regions around using M-<up>, M-<down>, M-<left>, M-<right>
;;

(require 'drag-stuff)
(drag-stuff-global-mode t)
(setq drag-stuff-modifier 'escape)

;;
;; js2-mode.el
;;
;; JavaScript major mode.
;; Doesn't use font-faces by default so it may be a bit wonky.
;;

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(setq js2-basic-offset 2)
(setq js2-use-font-lock-faces t)

;;
;; as3-mode.el
;;
;; Actionscript major mode
;;

(autoload 'as3-mode "as3" nil t)
(add-to-list 'auto-mode-alist '("\\.as3$" . as3-mode))



;;
;; python-mode.el
;;
;; python major mode.
;;

(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode)
                                   interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;;
;; apache-mode.el
;;
;; Apache config mode,
;;

(autoload 'apache-mode "apache-mode" "autoloaded" t)
(add-to-list 'auto-mode-alist '("\\.htaccess$"   . apache-mode))
(add-to-list 'auto-mode-alist '("httpd\\.conf$"  . apache-mode))
(add-to-list 'auto-mode-alist '("srm\\.conf$"    . apache-mode))
(add-to-list 'auto-mode-alist '("access\\.conf$" . apache-mode))

;;
;; smart-tab.el
;;
;; Allows you to hit TAB and the selected region will have indent-region called on it.
;;

(require 'smart-tab)
(setq smart-tab-using-hippie-expand t)

;;
;; delsel.el
;;
;; Allows the deletion of selected area/region when the mark is active with the DEL key.
;; Also allows overwriting of content while the mark is active.
;;

(delete-selection-mode 1)
(require 'delsel)

(provide 'max-modes)