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
  '(:global
    ;; Global bindings
    ;;; Navigation
    "C-l" backward-char
    "C-'" forward-char
    "C-p" previous-line
    "C-;" next-line

    "M-l" backward-word
    "M-'" forward-word

    "C-M-'" forward-list
    "C-M-l" backward-list

    "C-q" move-beginning-of-line

    "M-o" occur-at-point
    "M-q" highlight-symbol-prev
    "M-e" highlight-symbol-next
    "C-o" recenter-top-bottom
    "C-M-o" er/expand-region

    ;;; Editing
    "C-w" backward-kill-word
    "M-d" kill-region
    "C-c C-q" join-line
    "C-=" comment-or-uncomment-region
    "C-f" fill-paragraph
    "<f6>" whitespace-cleanup
    "C-c C-r" eval-and-replace
    "M-\\" indent-region
    "C-M-\\" indent-buffer
    "C-c TAB" quoted-insert
    "C-c c" clone-and-comment-line

    ;;; Buffer manipulation
    "C-x <C-return>" other-window
    "C-\\" kill-buffer-and-its-windows
    "C-." stesla-rotate-buffers
    "C-," (lambda () (interactive)
            (stesla-rotate-buffers -1))

    ;;; Miscellaneous
    "C-/" toggle-input-method
    "C-c M-r" query-replace
    "<f5>" minimap-toggle
    "<f8>" stupid-encoding

    ;; Local bindings
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

    ;; nrepl
    :local nrepl-mode-map nrepl-mode-hook
    "C-c C-e" nrepl-eval-expression-at-point

    :local nrepl-inspector-mode-map "nrepl-inspect.el"
    ";" nrepl-inspector-next-inspectable-object
    "p" nrepl-inspector-previous-inspectable-object
    "C-;" nrepl-inspector-operate-on-point
    "C-p" nrepl-inspector-pop

    ;; Flyspell mode
    :local flyspell-mode-map "flyspell"
    "C-;" next-line
    "C-." stesla-rotate-buffers
    "C-," (lambda () (interactive)
            (stesla-rotate-buffers -1))

    ;; Org-mode
    :local org-mode-map org-mode-hook
    "C-'" forward-char
    "M-h" helm-do-projectile-grep
    "C-," (lambda () (interactive)
            (stesla-rotate-buffers -1))

    ;; HideShow mode
    :global
    "C-h" hs-hide-all
    "C-S-h" hs-show-all
    "<C-tab>" hs-toggle-hiding

    ;; Magit mode
    :global
    "C-x g" magit-status

    :local magit-mode-map magit-mode-hook
    ";" magit-goto-next-section

    :local magit-log-mode-map magit-log-mode-hook
    "p" previous-line
    ";" next-line

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

    ;; Multiple cursors
    :global
    "C-S-<mouse-1>" mc/add-cursor-on-click
    "<C-down>" mc/mark-next-like-this
    "C-c m" mc/mark-all-like-this-dwim

    ;; Helm
    :global
    "M-m" helm-mini
    "M-f" find-file-in-project
    "M-h" helm-do-projectile-grep
    "C-c p h" helm-do-projectile-grep

    :local helm-grep-map helm-after-initialize-hook
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
    ))
