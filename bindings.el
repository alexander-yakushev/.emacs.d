;; define-keys is a macro for convenient keybindings definition. It expects
;; key-value pairs where key is a string for a keybinding, and value is a
;; function to execute. Pairs can be intersected with context switchers. You can
;; put in :global for next bindings to become glonbal, or :local <mode-map>
;; <hook-or-file> for bindings to be bound to the specific mode. If mode file is
;; used instead of a hook, bindings will be bound with `EVAL-AFTER-LOAD`.

(require 'cl)
(require 'dash)

(defun define-keys (args)
  (lexical-let ((defkeys (lambda (map args)
                           (-map (lambda (x)
                                   (if map
                                       (define-key map (kbd (car x)) (cadr x))
                                     (global-set-key (kbd (car x)) (cadr x))))
                                 (-partition 2 args)))))
    (-map (lambda (modelist)
            (cond
             ((eq (car modelist) :global) (funcall defkeys nil (cdr modelist)))
             (t (lexical-let ((modelist modelist))
                  (if (stringp (caddr modelist))
                      (eval-after-load (caddr modelist)
                        `(-map (lambda (x)
                                 (define-key ,(cadr modelist) (kbd (car x)) (cadr x)))
                               (-partition 2 ',(cdddr modelist))))
                    (add-hook (caddr modelist)
                              (lambda ()
                                (funcall defkeys (eval (cadr modelist)) (cdddr modelist)))))))))
          (-partition-by-header #'keywordp args))))

(define-keys
  '(;;; Navigation
    :global
    "C-l" backward-char
    "C-'" forward-char
    "C-p" previous-line
    "C-;" next-line

    "M-l" backward-word
    "M-'" forward-word

    "C-M-'" forward-sexp
    "C-M-l" backward-sexp

    "M-G M-G" goto-line

    "C-a" smart-move-beginning-of-line

    "M-o" helm-swoop
    "M-a" highlight-symbol-prev
    "M-e" highlight-symbol-next
    "C-o" recenter-top-bottom
    "C-M-o" er/expand-region
    "C-M-q" narrow-or-widen-dwim

    "C-s" isearch-forward-regexp
    "\C-r" isearch-backward-regexp
    "M-%" query-replace-regexp
    "C-x C-i" imenu

    ;;; Editing
    "C-w" backward-delete-word
    "<C-backspace>" backward-delete-word
    "M-d" kill-region
    "C-b" kill-whole-line
    "C-S-k" (lambda () (interactive) (delete-region (point) (line-end-position)))
    "C-M-d" delete-region
    "C-c q" join-line
    "C-c C-q" join-line
    "C-M-j" join-line
    "C-=" comment-or-uncomment-region
    "C-f" fill-paragraph
    "<f6>" whitespace-cleanup
    "C-c C-r" eval-and-replace
    "M-\\" indent-region
    "C-M-\\" indent-buffer
    "C-c TAB" quoted-insert
    "C-c c" clone-and-comment-line
    "M-c" just-one-space
    "M-/" hippie-expand
    "C-c r" revert-buffer
    "C-M-=" comment-or-uncomment-sexp
    "C-c C-l" goto-last-change
    "s--" (lambda () (interactive) (insert "—"))

    ;;; Buffer manipulation
    "C-x <C-return>" other-window
    "C-\\" kill-this-buffer

    "C-n" create-temp-buffer
    "C-x 0" (lambda () (interactive) (other-window -1))
    "<S-left>" windmove-left
    "<S-right>" windmove-right
    "<S-up>" windmove-up
    "<S-down>" windmove-down
    "C-x O" swap-buffers-in-windows
    "<f2>" nil
    "C-x C-z" nil

    ;;; Miscellaneous
    "C-/" toggle-input-method
    "C-c M-r" query-replace
    "M-SPC" auto-complete
    "M-=" count-words

    ;; Undo
    "C-z" undo-only
    "C-]" undo-redo

    ;; Paredit
    :local paredit-mode-map paredit-mode-hook
    "M-(" paredit-wrap-sexp
    "M-[" paredit-wrap-square
    "M-{" paredit-wrap-curly
    "M-p" paredit-backward-down
    "M-;" paredit-forward-down
    "C-M-p" paredit-backward-up
    "C-M-;" paredit-forward-up
    "C-M-'" forward-sexp
    "C-M-l" backward-sexp
    ;; "C-M-q" beginning-of-defun
    "M-a" highlight-symbol-prev
    ;; "M-q" highlight-symbol-prev
    "M-k" kill-line
    "M-d" kill-region
    "C-M-d" delete-region
    "C-w" paredit-backward-kill-word
    "<C-backspace>" paredit-backward-kill-word

    :local cider-inspector-mode-map "cider-inspect.el"
    ";" cider-inspector-next-inspectable-object
    "p" cider-inspector-previous-inspectable-object
    "C-;" cider-inspector-operate-on-point
    "C-p" cider-inspector-pop
    "SPC" cider-inspector-next-page
    "M-SPC" cider-inspector-prev-page

    ;; Flyspell mode
    :local flyspell-mode-map "flyspell"
    "C-;" next-line

    ;; Flymake mode
    :global
    "C-x ," flymake-goto-next-error

    ;; Org-mode
    :local org-mode-map org-mode-hook
    "C-'" forward-char

    ;; HideShow mode
    :global
    "C-h" hs-hide-all
    "C-S-h" hs-show-all
    "<C-tab>" hs-toggle-hiding

    ;; Magit mode
    :local magit-mode-map magit-mode-hook
    ";" magit-section-forward
    "M-;" magit-section-forward-sibling
    "X" magit-reset-hard

    :local magit-refs-mode-map magit-refs-mode-hook
    ";" magit-section-forward

    :local magit-log-mode-map magit-log-mode-hook
    "p" previous-line
    ";" next-line

    :local git-rebase-mode-map git-rebase-mode-hook
    ";" forward-line
    "M-;" git-rebase-move-line-down

    :local magit-status-mode-map magit-status-mode-hook
    "W" magit-toggle-whitespace

    :local magit-blame-mode-map magit-blame-mode-hook
    ";" magit-blame-next-chunk

    :global
    "C-M-." git-gutter:next-hunk
    "C-M-," git-gutter:previous-hunk

    ;; bs mode
    :global
    "C-x C-l" bs-show

    :local Buffer-menu-mode-map Buffer-menu-mode-hook
    ";" bs-down

    :local bs-mode-map bs-mode-hook
    ";" bs-down
    "<C-down-mouse-1>" mouse-bs-show

    ;; Occur mode
    :local occur-mode-map occur-mode-hook
    ";" occur-next
    "p" occur-prev

    ;; Helm-swoop mode
    :local helm-swoop-map "helm-swoop"
    "C-;" helm-next-line

    ;; Helm
    :global
    "M-m" helm-mini

    ;; Grep-mode
    :local grep-mode-map grep-mode-hook
    "q" kill-buffer-and-its-windows

    ;; Elisp-mode
    :local emacs-lisp-mode-map emacs-lisp-mode-hook
    "C-c C-e" eval-defun
    "C-c M-e" eval-and-replace

    ;; Package mode
    :local package-menu-mode-map package-menu-mode-hook
    ";" next-line

    :local LaTeX-mode-map LaTeX-mode-hook
    "C-c C-a" TeX-texify

    :local reftex-mode-map reftex-mode-hook
    "C-c [" (lambda () (interactive)
              (LaTeX-add-all-bibitems-from-bibtex)
              (reftex-citation))

    :local zencoding-mode-keymap zencoding-mode-hook
    "C-j" electric-newline-and-maybe-indent
    "C-c C-z" zencoding-expand-line

    ;; Langtool
    "C-x 4 l" langtool-check
    "C-x 4 L" langtool-check-done
    "C-x 4 s" langtool-show-message-at-point))
