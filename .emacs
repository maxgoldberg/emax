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
