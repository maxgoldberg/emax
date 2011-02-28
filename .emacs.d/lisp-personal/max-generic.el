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

;;
;; Whitspace and tabs
;;

;; Indent/offset is two spaces

(setq c-basic-indent 2)
(setq c-basic-offset 2)

;; Tabs are four spaces

(setq tab-width 4)

;; Never use tab characters.

(setq-default indent-tabs-mode nil)

;; Use "y or n" answers instead of full words "yes or no"

(fset 'yes-or-no-p 'y-or-n-p)

;; Turn on visual bell (and prevent the audible bell)

(setq visible-bell t)

;; Highlight regions and add special behaviors to regions.
;; "C-h d transient" for more info (is this default now?)

(setq transient-mark-mode t)

;; Display line and column numbers

(setq line-number-mode    t)
(setq column-number-mode  t)

;; Word-wrap at 120.

(set-default 'fill-column 120)

;;
;; Convert all line endings to Unix automatically.
;;
;; This will turn all mac/windows line endings into line feeds, then set the buffer as modified.
;;


(add-hook 'find-file-hook 'find-file-check-line-endings)

(defun dos-file-endings-p ()
  (string-match "dos" (symbol-name buffer-file-coding-system)))

(defun mac-file-endings-p ()
  (string-match "mac" (symbol-name buffer-file-coding-system)))


(defun find-file-check-line-endings ()
  (when (dos-file-endings-p)
    (set-buffer-file-coding-system 'utf-8-unix)
    (set-buffer-modified-p nil))
  (when (mac-file-endings-p)
    (set-buffer-file-coding-system 'utf-8-unix)
    (set-buffer-modified-p nil))

  ;; To auto save as well:
  ;; (set-buffer-file-coding-system 'utf-8-unix)(save-buffer)
)


;;
;; Enable color! (for older emacs)
;;

(add-hook 'shell-mode-hook (lambda ()
                             (ansi-color-for-comint-mode-on)))

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

(provide 'max-generic)
