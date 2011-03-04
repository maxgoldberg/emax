;;
;; Whitespace and file-encoding options.
;;

;;;;
;;
;; UTF-8 > *
;;
;; Some of the below might be redundant.
;;

(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;;
;; Indent/offset is two spaces
;;

(setq-default c-basic-indent 2)
(setq-default c-basic-offset 2)

;; Tabs are four spaces

(setq-default tab-width 4)

;; Never use tab characters.

(setq-default indent-tabs-mode nil)

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



(provide 'max-whitespace)
