;;; Code:

(when window-system
  (tooltip-mode 1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(setq color-theme-is-global t)

;; ido-mode is like magic pixie dust!
(ido-mode t)
(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defalias 'auto-tail-revert-mode 'tail-mode)

(random t) ;; Seed the random-number generator

;; Cosmetics

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(TODO\\|FIXME\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'prog-mode-hook 'idle-highlight-mode)

(defun esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

;; Commands
(defun esk-sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun esk-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; A monkeypatch to cause annotate to ignore whitespace
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

;; Emacs Lisp

;;;###autoload
(progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'esk-prog-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)

;;; Enhance Lisp Modes

  (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
  (define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

  (use-package paren-face :ensure t :demand t
    :config
    (dolist (mode '(scheme emacs-lisp lisp))
      (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                'paren-face-mode))
    (add-hook 'clojure-mode-hook
              (lambda ()
                (setq-local paren-face-regexp "[(){}]\\|\\[\\|\\]")
                (paren-face-mode 1)))
    (add-hook 'clojurescript-mode-hook
              (lambda ()
                (setq-local paren-face-regexp "[(){}]\\|\\[\\|\\]")
                (paren-face-mode 1))))

  (defun esk--enable-paredit-mode ()
    (unless (equal major-mode 'cider-repl-history-mode)
      (paredit-mode)))

  (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
              'esk--enable-paredit-mode))

  (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
    (remove-hook (intern (concat (symbol-name mode) "-mode-hook"))
                 'paredit-mode)))
