;;
;; max's .emacs
;;
;; lots of help and ideas gathered from github, thanks pals!
;;

;;
;;;; Load paths
;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;;
;;;; Third party lisp
;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;;
;;;; Personal lisp
;;

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp-personal"))

;;
;;;; generic settings
;;

(require `max-generic)

;;
;;;; whitespace and file-encoding settings
;;

(require `max-whitespace)

;;
;;;; settings that change the look-and-feel of emacs
;;

(require `max-visual)

;;
;;;; custom formatting/style/color settings
;;

(require `max-coding-style)

;;
;;;; custom major/minor editing modes.
;;

(require `max-modes)


;;
;; TODO
;;
;; Look into flyspell-prog-mode to ensure comments are properly spellchecked across all languages.
;;
;; OCD this for a while:
;;  http://www.gnu.org/software/emacs/manual/html_node/ccmode/Operator-Line_002dUp.html#Operator-Line_002dUp
;;  and http://www.emacswiki.org/emacs/AlignCommands
;;
;; more minor mode automatics
;;
;; auto-byte-compile.
;;
;; Look into better VCS options: http://www.delorie.com/gnu/docs/emacs/emacs_127.html
;;
;; Quick function to navigate emacs options when they become large.
;;

(defun show-dot-emacs-structure ()
  "Show the outline-mode structure of ~/.emacs"
  (interactive)
  (occur "^;;;;+"))
