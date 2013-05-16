(require 'cl)
(require 'dash)

(defun define-keys (mode global-map args)
  (lexical-let ((lmode mode)
                (defkeys (lambda (map args)
                           (-map (lambda (x)
                                   (define-key map (kbd (car x)) (cadr x)))
                                 (-partition 2 args)))))
    (-map (lambda (modelist)
            (cond
             ((eq (car modelist) :global) (funcall defkeys global-map (cdr modelist)))
             ((eq (car modelist) :disable) (-map (lambda (hook)
                                                   (add-hook hook (lambda () (funcall lmode 0))))
                                                 (cdr modelist)))
             (t (lexical-let ((modelist modelist))
                  (add-hook (caddr modelist)
                            (lambda ()
                              (funcall defkeys (eval (cadr modelist)) (cdddr modelist))))))))
          (-partition-by-header #'keywordp args))))

(define-minor-mode custom-keys-mode
  "Get your foos in the right places."
  :lighter "CustomKeys"
  :keymap (make-sparse-keymap))

(define-globalized-minor-mode global-custom-keys-mode
  custom-keys-mode (lambda () (custom-keys-mode 1)))

(define-keys 'custom-keys-mode custom-keys-mode-map
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
    "C-c C-y" (lambda () (interactive)
                (dired-next-line 1))

    :local sr-tabs-mode-map sr-mode-hook
    "C-h" (lambda () (interactive)
            (sr-goto-dir "~/"))
    "C-j" sr-cycle-bookmark
    "j" ido-sunrise
    "C-p" sr-dired-prev-subdir
    "<f9>" sr-open-custom-terminal

    ;; Undo-tree-mode
    :global
    "C-z" undo-tree-undo
    "C-]" undo-tree-redo
    ;; :local undo-tree-visualizer-map undo-tree-visualizer-
    ;; (eval-after-load "undo-tree" '(define-key undo-tree-map (kbd "C-/") 'toggle-input-method))

    ;; Paredit
    :local paredit-mode-map paredit-mode-hook
    "M-[" paredit-wrap-square
    "M-{" paredit-wrap-curly
    "M-p" paredit-backward-down
    "M-;" paredit-forward-down
    "C-M-p" paredit-backward-up
    "C-M-;" paredit-forward-up
    "M-k" kill-line
    "M-d" kill-region
    "C-w" paredit-backward-kill-word

    ;; Flyspell mode
    ;; (eval-after-load "flyspell"
    ;;   '(progn
    ;;      (define-key flyspell-mode-map (kbd "C-;") 'next-line)
    ;;      (define-key flyspell-mode-map (kbd "C-.") 'stesla-rotate-buffers)
    ;;      (define-key flyspell-mode-map (kbd "C-,") (lambda ()
    ;;                                                  (interactive)
    ;;                                                  (stesla-rotate-buffers -1)))))

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
    "C-S-<mouse-1>" mc/add-cursor-on-click
    "<C-down>" mc/mark-next-like-this

    ;; Helm
    :global
    "M-m" helm-mini
    "M-f" projectile-find-file
    "M-h" helm-do-projectile-grep
    "C-c p h" helm-do-projectile-grep

    :local helm-grep-map helm-after-initialize-hook
    "C-;" helm-next-line
    "M-;" helm-goto-next-file
    "M-p" helm-goto-precedent-file

    :disable minibuffer-setup-hook sr-mode-hook
    ))

;; Enable globally
(global-custom-keys-mode)

;; Don't enable in the following modes
;; (add-hook 'minibuffer-setup-hook (lambda () (custom-keys-mode 0)))
;; (add-hook 'sr-mode-hook (lambda () (custom-keys-mode 0)))

;; (eval-after-load "undo-tree" '(define-key undo-tree-map (kbd "C-/") 'toggle-input-method))
