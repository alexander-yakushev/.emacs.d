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

    "C-M-'" forward-list
    "C-M-l" backward-list

    "C-q" move-beginning-of-line

    "M-o" helm-swoop
    "M-q" highlight-symbol-prev
    "M-e" highlight-symbol-next
    "C-o" recenter-top-bottom
    "C-M-o" er/expand-region
    "C-a" narrow-or-widen-dwim

    ;;; Editing
    "C-w" backward-kill-word
    "M-d" kill-region
    "C-b" kill-whole-line
    "C-c C-q" join-line
    "C-=" comment-or-uncomment-region
    "C-f" fill-paragraph
    "<f6>" whitespace-cleanup
    "C-c C-r" eval-and-replace
    "M-\\" indent-region
    "C-M-\\" indent-buffer
    "C-c TAB" quoted-insert
    "C-c c" clone-and-comment-line
    "M-c" just-one-space

    ;;; Buffer manipulation
    "C-x <C-return>" other-window
    "C-\\" kill-buffer-and-its-windows
    "C-." stesla-rotate-buffers
    "C-," (lambda () (interactive)
            (stesla-rotate-buffers -1))
    "C-n" create-temp-buffer

    ;;; Miscellaneous
    "C-/" toggle-input-method
    "C-c M-r" query-replace
    "<f5>" minimap-toggle
    "C-c d" ediff-opened-buffers
    "M-SPC" auto-complete

    ;;; Sunrise
    :global
    "<f7>" sunrise
    "<C-f7>" sunrise-cd

    :local sr-mode-map sr-mode-hook
    ";" dired-next-line
    "C-;" sr-advertised-find-file
    "C-h" (lambda () (interactive)
            (sr-goto-dir "~/"))
    "j" ido-sunrise
    "<f9>" sr-open-custom-terminal

    :local sr-tabs-mode-map sr-mode-hook
    "C-j" sr-cycle-bookmark
    "C-p" sr-dired-prev-subdir

    ;; Undo-tree-mode
    :global
    "C-z" undo-tree-undo
    "C-]" undo-tree-redo

    :local undo-tree-map "undo-tree"
    "C-/" toggle-input-method

    ;; Paredit
    :local paredit-mode-map paredit-mode-hook
    "M-[" paredit-wrap-square
    "M-{" paredit-wrap-curly
    "M-p" paredit-backward-down
    "M-;" paredit-forward-down
    "C-M-p" paredit-backward-up
    "C-M-;" paredit-forward-up
    "C-M-q" beginning-of-defun
    "M-q" highlight-symbol-prev
    "M-k" kill-line
    "M-d" kill-region
    "C-w" paredit-backward-kill-word

    ;; cider-inspector
    :local cider-mode-map cider-mode-hook
    "C-c i" cider-inspect

    :local cider-inspector-mode-map "cider-inspect.el"
    ";" cider-inspector-next-inspectable-object
    "p" cider-inspector-previous-inspectable-object
    "C-;" cider-inspector-operate-on-point
    "C-p" cider-inspector-pop

    ;; Flyspell mode
    :local flyspell-mode-map "flyspell"
    "C-;" next-line
    "C-." stesla-rotate-buffers
    "C-," (lambda () (interactive)
            (stesla-rotate-buffers -1))

    ;; Flymake mode
    :global
    "C-x ," flymake-goto-next-error

    ;; Org-mode
    :local org-mode-map org-mode-hook
    "C-'" forward-char
    "M-h" helm-do-projectile-grep
    "C-," (lambda () (interactive)
            (stesla-rotate-buffers -1))
    "C-c 9" org-insert-codeblock

    ;; HideShow mode
    :global
    "C-h" hs-hide-all
    "C-S-h" hs-show-all
    "<C-tab>" hs-toggle-hiding

    ;; Magit mode
    :global
    "C-x g" magit-status
    "<f8>" magit-blame-mode

    :local magit-mode-map magit-mode-hook
    ";" magit-goto-next-section

    :local magit-log-mode-map magit-log-mode-hook
    "p" previous-line
    ";" next-line

    :local magit-status-mode-map magit-status-mode-hook
    "W" magit-toggle-whitespace

    :local magit-blame-map "magit-blame"
    ";" magit-blame-next-chunk

    :local vc-annotate-mode-map vc-annotate-mode-hook
    "c" vc-annotate-show-commit-at-line

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

    ;; Multiple cursors
    :global
    "C-S-<mouse-1>" mc/add-cursor-on-click
    "<C-down>" mc/mark-next-like-this
    "C-c m" mc/mark-all-like-this-dwim

    ;; Helm
    :global
    "M-m" helm-mini
    "M-f" projectile-find-file
    "M-h" helm-git-grep

    :local helm-grep-map helm-after-initialize-hook
    "C-;" helm-next-line
    "M-;" helm-goto-next-file
    "M-p" helm-goto-precedent-file

    :local helm-git-grep-map "helm-git-grep"
    "C-;" helm-next-line
    "M-;" helm-goto-next-file
    "M-p" helm-goto-precedent-file

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

    :local octave-mode-map octave-mode-hook
    "C-c C-c" octave-send-region
    "C-x C-e" octave-send-line

    :local LaTeX-mode-map LaTeX-mode-hook
    "C-c C-a" TeX-texify

    :global
    "C-x t" thesaurus-choose-synonym-and-replace

    ;; Google translate
    :global
    "C-x M-t" google-translate-fast))
