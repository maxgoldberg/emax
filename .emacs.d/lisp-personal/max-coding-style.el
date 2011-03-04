;;
;; Code style/color/faces specific options
;;

;;
;; Highlight the opening brackets when the cursor is on a close bracket (and do it with 0 delay)
;;

(show-paren-mode t)
(setq show-paren-delay 0)

;;
;; Force all comments to be red, each emacs mode sets coments to have different/colors
;; style which is annoying when switching between modes.
;;

(make-face 'max-comment-face)
(set-face-foreground 'max-comment-face "red")
(set-face-background 'max-comment-face  "dark")
(setq font-lock-comment-face 'max-comment-face)

;;
;; Custom C-style, stolen from vivek and modified.
;; Overly comprehensive, but useful for reference.
;;

(defvar maxs-c-style
  '("bsd"
    (c-comment-only-line-offset . 0)
    (c-basic-offset             . 2)
    (c-offsets-alist .
                     ( (string                        . c-lineup-dont-change)
                       (c                             . c-lineup-C-comments)
                       (defun-open                    . 0)
                       (defun-close                   . 0)
                       (defun-block-intro             . +)
                       (class-open                    . 0)
                       (class-close                   . 0)
                       (inline-open                   . +)
                       (inline-close                  . 0)
                       (func-decl-cont                . +)
                       (knr-argdecl-intro             . +)
                       (knr-argdecl                   . 0)
                       (topmost-intro                 . 0)
                       (topmost-intro-cont            . 0)
                       (member-init-intro             . +)
                       (member-init-cont              . 0)
                       (inher-intro                   . +)
                       (inher-cont                    . c-lineup-multi-inher)
                       (block-open                    . 0)
                       (block-close                   . 0)
                       (brace-list-open               . 0)
                       (brace-list-close              . 0)
                       (brace-list-intro              . +)
                       (brace-list-entry              . 0)
                       (brace-entry-open              . 0)
                       (statement                     . 0)
                       (statement-cont                . 0)
                       (statement-block-intro         . +)
                       (statement-case-intro          . *)
                       (statement-case-open           . 0)
                       (substatement                  . +)
                       (substatement-open             . 0)
                       (case-label                    . *)
                       (access-label                  . -)
                       (label                         . 0)
                       (do-while-closure              . 0)
                       (else-clause                   . 0)
                       (catch-clause                  . 0)
                       (comment-intro                 . c-lineup-comment)
                       (arglist-intro                 . c-lineup-arglist-intro-after-paren)
                       (arglist-cont                  . c-lineup-arglist-intro-after-paren)
                       (arglist-cont-nonempty         . c-lineup-arglist)
                       (arglist-close                 . c-lineup-arglist-close-under-paren)
                       (stream-op                     . c-lineup-streamop)
                       (inclass                       . +)
                       (cpp-macro                     . -1000)
                       (cpp-macro-cont                . c-lineup-dont-change)
                       (friend                        . 0)
                       (objc-method-intro             . -1000)
                       (objc-method-args-cont         . c-lineup-ObjC-method-args)
                       (objc-method-call-cont         . c-lineup-ObjC-method-call)
                       (extern-lang-open              . 0)
                       (extern-lang-close             . 0)
                       (inextern-lang                 . +)
                       (namespace-open                . 0)
                       (namespace-close               . 0)
                       (innamespace                   . +)
                       (template-args-cont            . +)
                       (inlambda                      . c-lineup-inexpr-block)
                       (lambda-intro-cont             . +)
                       (inexpr-statement              . 0)
                       (inexpr-class                  . +) ))
    (c-echo-syntactic-information-p . t) ;; Helpful when debugging indentation, turn off if not needed.
    )
  "Max's C Style"
  )

;;
;; Turn on Max's C style by default.
;;

(require 'cc-mode)

(add-to-list 'c-mode-common-hook
             (lambda ()
               (c-add-style "maxstyle"  maxs-c-style   t)
               (c-set-style "maxstyle")))




(provide 'max-coding-style)
