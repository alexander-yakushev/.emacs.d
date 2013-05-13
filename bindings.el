;; Global bindings
;;; Navigation
(global-set-key (kbd "C-l") 'backward-char)
(global-set-key (kbd "C-'") 'forward-char)
(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-;") 'next-line)
(global-set-key (kbd "C-M-o") 'er/expand-region)

(global-set-key (kbd "M-l") 'backward-word)
(global-set-key (kbd "M-'") 'forward-word)

(global-set-key (kbd "C-M-'") 'forward-list)
(global-set-key (kbd "C-M-l") 'backward-list)

(global-set-key (kbd "C-q") 'move-beginning-of-line)
(global-set-key (kbd "M-q") 'backward-sentence)

(global-set-key (kbd "M-o") 'occur-at-point)
(global-set-key (kbd "C-o") 'recenter-top-bottom)

;;; Editing
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-d") 'kill-region)
(global-set-key (kbd "C-c C-q") 'join-line)
(global-set-key (kbd "C-=") 'comment-or-uncomment-region)
(global-set-key (kbd "C-f") 'fill-paragraph)
(global-set-key (kbd "<f6>") 'whitespace-cleanup)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)
(global-set-key (kbd "<C-return>") (lambda () (interactive) (move-end-of-line 1)
                                     (reindent-then-newline-and-indent)))
(global-set-key (kbd "M-\\") 'indent-region)
(global-set-key (kbd "C-M-\\") 'indent-buffer)
(global-set-key (kbd "C-c c") 'clone-and-comment-line)
;; (global-set-key (kbd "<tab>") 'indent-for-tab-command)

;;; Buffer manipulation
(global-set-key (kbd "C-x <C-return>") 'other-window)
(global-set-key (kbd "C-c a") 'mark-whole-buffer)
(global-set-key (kbd "C-w") 'kill-buffer-and-its-windows)

;;; Miscellaneous
(global-set-key (kbd "C-/") 'toggle-input-method)
(global-set-key (kbd "M-m") 'helm-mini)
(global-set-key (kbd "C-c M-r") 'query-replace)
(global-set-key (kbd "<f5>") 'minimap-toggle)
(global-set-key (kbd "<f8>") 'stupid-encoding)
(global-set-key (kbd "C-x g") 'magit-status)

;; Local bindings
;;; Sunrise
(global-set-key (kbd "<f7>") 'sunrise)
(global-set-key (kbd "<C-f7>") 'sunrise-cd)
(add-hook 'sr-mode-hook
          (lambda ()
            (define-key sr-mode-map (kbd "C-;") 'sr-advertised-find-file)
            (define-key sr-mode-map (kbd ";") 'dired-next-line)
            (define-key sr-tabs-mode-map (kbd "C-h") (lambda () (interactive) (sr-goto-dir "~/")))
            (define-key sr-tabs-mode-map (kbd "C-j") 'sr-cycle-bookmark)
            (define-key sr-tabs-mode-map (kbd "j") 'ido-sunrise)
            (define-key sr-tabs-mode-map (kbd "C-p") 'sr-dired-prev-subdir)
            (define-key sr-tabs-mode-map (kbd "C-.") 'sr-tabs-next)
            (define-key sr-tabs-mode-map (kbd "C-,") 'sr-tabs-prev)
            (define-key sr-tabs-mode-map (kbd "<f9>") 'sr-open-custom-terminal)))

;;; Undo-tree mode
(global-set-key (kbd "C-\\") 'undo-tree-undo)
(global-set-key (kbd "C-]") 'undo-tree-redo)
(eval-after-load "undo-tree" '(define-key undo-tree-map (kbd "C-/") 'toggle-input-method))

;; Paredit mode
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square)
            (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
            (define-key paredit-mode-map (kbd "M-p") 'paredit-backward-down)
            (define-key paredit-mode-map (kbd "M-;") 'paredit-forward-down)
            (define-key paredit-mode-map (kbd "C-M-p") 'paredit-backward-up)
            (define-key paredit-mode-map (kbd "C-M-;") 'paredit-forward-up)
            (define-key paredit-mode-map (kbd "M-k") 'kill-line)
            (define-key paredit-mode-map (kbd "M-d") 'kill-region)))

;; Flyspell mode
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mode-map (kbd "C-;") 'next-line)
     (define-key flyspell-mode-map (kbd "C-.") 'stesla-rotate-buffers)
     (define-key flyspell-mode-map (kbd "C-,") (lambda ()
                                                 (interactive)
                                                 (stesla-rotate-buffers -1)))))

;; Org-mode
(add-hook 'org-mode-hook
          (lambda () (define-key org-mode-map (kbd "C-'") 'forward-char)))

;; HideShow mode
(global-set-key (kbd "C-h") 'hs-hide-all)
(global-set-key (kbd "C-S-h") 'hs-show-all)
(global-set-key (kbd "<C-tab>") 'hs-toggle-hiding)

;; Magit mode
(add-hook 'magit-mode-hook
          (lambda () (define-key magit-mode-map (kbd ";") 'magit-goto-next-section)))
(add-hook 'magit-log-mode-hook
          (lambda ()
            (define-key magit-log-mode-map (kbd "p") 'previous-line)
            (define-key magit-log-mode-map (kbd ";") 'next-line)))

;; Read-only modes
(add-hook 'buffer-menu-mode-hook (lambda () (define-key Buffer-menu-mode-map (kbd ";") 'next-line)))
(add-hook 'occur-mode-hook
          (lambda ()
            (define-key occur-mode-map (kbd ";") 'occur-next)
            (define-key occur-mode-map (kbd "p") 'occur-prev)
            (define-key occur-mode-map (kbd "C-p") 'occur-prev)
            (define-key occur-mode-map (kbd "C-;") 'occur-next)))

;; Buffer selection menu
(global-set-key (kbd "C-x C-l") 'bs-show)
(add-hook 'bs-mode-hook (lambda () (define-key bs-mode-map (kbd ";") 'next-line)))
(global-set-key (kbd "<C-down-mouse-1>") 'mouse-bs-show)

;; Multiple cursors
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
(global-set-key (kbd "<C-down>") 'mc/mark-next-like-this)

;; Helm
(global-set-key (kbd "C-c p h") 'helm-do-projectile-grep)
(add-hook 'helm-grep-mode-hook
          (lambda ()
            (define-key helm-c-grep-map (kbd "C-;") 'helm-next-line)
            (define-key helm-c-grep-map (kbd "M-;") 'helm-c-goto-next-file)
            (define-key helm-c-grep-map (kbd "M-p") 'helm-c-goto-precedent-file)))
